#pragma once
#include "ast.h"
#include "null.h"
#include <boost/variant.hpp>
#include <vector>
#include <optional>
#include <iomanip>
#include <map>

namespace Eval {
    using boost::core::demangle;

    struct Value;
    using Values = std::vector<Value>;
    using Function = std::function<Value(Values const&)>;
    using ValueV = boost::variant<
        Null,
        Ast::Number,
        std::string,
        Ast::Boolean,
        Function
    >;

    static inline bool operator==(Function const& f, Function const& g) {
        return !f && !g; // only match if both uninitialized
    }
    static inline bool operator!=(Function const& f, Function const& g) {
        return f || g; // only match if both uninitialized
    }

    struct Value : ValueV {
        using ValueV::ValueV;
        using ValueV::operator=;
        Value operator==(const Value& o) const {
            if (!(which() && o.which())) return Null{};
            return boost::apply_visitor(_eq, *this, o);
        }
        Value operator!=(const Value& o) const {
            if (!(which() && o.which())) return Null{};
            return !boost::apply_visitor(_eq, *this, o);
        }

      private:
        struct EqualToVis {
            using result_type = bool;
            template <typename T> bool operator()(T const& v, T const& u) const { return v == u; }
            template <typename T, typename U> bool operator()(T const&, U const&) const { return false; }
            // will be overruled, but required to satisfy visitor typing rules
            bool operator()(Null const&, Null const&) const { return false; }
        } static constexpr _eq {};
    };

    static inline std::ostream& operator<<(std::ostream& os, Function const&) {
        return os << "function(...){}";
    }

    // Reflects a dynamic object instance in the Evaluation engine. Losely
    // based on the idea of ExpandoObject in System.Dynamic from the CLR
    struct Variable {
        using Sub = boost::recursive_wrapper<Variable>;
        using Key = std::string;

        using Array = std::vector<Sub>;
        using Object = std::map<Key, Sub>;

        Variable() = default;
        Variable(Value  v) : _value(std::move(v)) {}
        Variable(Object v) : _object(std::move(v)) {}
        Variable(Array  v) : _array(std::move(v)) {}

        Value  _value;
        Object _object;
        Array  _array;

        Variable& operator[](Values indices) {
            if (indices.empty())
                return *this;

            auto f = indices.front();

            if (auto index = boost::get<Ast::Number>(&f)) {
                auto i = index->convert_to<std::size_t>();

                if (i >= _array.size()) {
                    _array.insert(_array.end(), 1 + i - _array.size(), Variable{});
                }
                indices.erase(indices.begin());
                return _array.at(i).get()[indices];
            }
            if (auto key = boost::get<std::string>(&f)) {
                auto it = _object.find(*key);

                if (it == _object.end()) {
                    auto insertion = _object.emplace(*key, Variable{});
                    it = insertion.first;
                }
                indices.erase(indices.begin());
                return it->second.get()[indices];
            }

            throw std::runtime_error("Type Mismatch: " + type_name(f));
        }

        Variable& operator[](Value const& key) { return operator[](Values{ key }); }

        std::optional<std::string> path_to(Variable const& lvalue, std::string const& base = "") const {
            if (std::addressof(*this) == std::addressof(lvalue))
                return base;
            for (size_t i = 0; i < _array.size(); ++i) {
                if (auto r = _array[i].get().path_to(lvalue, base + "[" + std::to_string(i) + "]"))
                    return r;
            }
            for (auto const& [k,v] : _object) {
                if (auto r = v.get().path_to(lvalue, base.empty()?k : base + "." + k))
                    return r;
            }
            return std::nullopt;
        }

        void dump(std::ostream& os, bool verbose, std::string indent = "\n") const {
            auto sep = [&os, verbose, first=true]() mutable -> auto& {
                if (!std::exchange(first, false))
                    os << (verbose?" ":"/");
                return os;
            };

            if (!verbose) {
                sep() << _value;
                if (!_object.empty()) sep() << "obj{...}";
                if (!_array.empty()) sep() << "arr[...]";
                return;
            }

            sep() << _value;

            if (!_object.empty()) {
                sep() << "{";
                for (auto&[k, v] : _object) {
                    os << indent << std::quoted(k) << ": ";
                    v.get().dump(os, verbose, indent + "  ");
                }
                if (_object.empty())
                    os << " }";
                else
                    os << indent << "}";
            }

            if (!_array.empty()) {
                sep() << "[";
                size_t i = 0;
                for (auto& v : _array) {
                    v.get().dump(os << indent << "#" << i++ << ": ", verbose, indent + " ");
                }
                if (i)
                    os << indent << "]";
                else
                    os << " ]";
            }
        }
    private:
        static std::string type_name(Value const& v) {
            return demangle(v.type().name());
        }
        friend std::ostream& operator<<(std::ostream& os, Variable const& obj) {
            obj.dump(os, false);
            return os;
        }
    };

    // In our "typesystem" we will have Dynamic variables, where each variable
    // can be either an LValue or RValue
    struct WrapValue { Value _value; }; // to unconfuse boost::variant convert_construct
    using RValue = Value;
    using LValue = Variable & ;

    struct Dynamic : boost::variant<WrapValue, Variable*> {
        using Variant = boost::variant<WrapValue, Variable*>;
        Dynamic() = default;
        Dynamic(Value const& v) : Variant(WrapValue{ v }) {}
        Dynamic(LValue r) : Variant(&r) {}
        using Variant::operator=;

        LValue lvalue() const {
            if (auto* p = boost::strict_get<Variable*>(&base()))
                return **p;
            throw std::runtime_error("LValue required (" + boost::lexical_cast<std::string>(*this) + ")");
        }
        Value value() const {
            return which() == 0
                ? boost::strict_get<WrapValue>(base())._value
                : boost::strict_get<Variable*>(base())->_value;
        }

        operator LValue() const { return lvalue(); }
        operator Value() const { return value(); }

        friend inline bool is_rvalue(Dynamic const& v) { return 0 == v.which(); }
        friend inline bool is_lvalue(Dynamic const& v) { return 1 == v.which(); }

    private:
        Variant&       base() { return *this; }
        Variant const& base() const { return *this; }

        friend std::ostream& operator<<(std::ostream& os, Dynamic const& obj) {
            return is_lvalue(obj)
                ? os << obj.lvalue()
                : os << obj.value();
        }
    };

    namespace detail {
        struct Truthyness {
            template <typename... T>
            bool operator()(boost::variant<T...> const& v) const { return boost::apply_visitor(*this, v); }
            bool operator()(Dynamic const& o) const { return operator()(o.value()); }
            bool operator()(Value const& o) const { return boost::apply_visitor(*this, o); }
            bool operator()(Null) const { return false; }
            bool operator()(Ast::Boolean const& o) const { return o == Ast::Boolean::True; }
            bool operator()(Ast::Number const& o) const { return o != 0; }
            bool operator()(Ast::String const& o) const { return !o.empty(); }
            template <typename T>
            bool operator()(T const& v) const {
                std::ostringstream oss;
                oss << "No conversion of " << v << " (" << demangle(typeid(T).name()) << " to boolean";
                throw std::runtime_error(oss.str());
            }
        };

        static inline const Truthyness truthy = {};

        struct OperationBase {
            static Null boolcast(Null) {
                return {};
            }
            static Ast::Boolean boolcast(bool b) {
                return b ? Ast::Boolean::True : Ast::Boolean::False;
            }
        };

        template <typename Op>
        struct Arithmetic : OperationBase {
            using result_type = RValue;

            Ast::Number operator()(Ast::Number const& a) const { return Op{}(a); }

            template <typename T, typename U>
            RValue operator()(T const& a, U const& b, decltype(Op{}(T{}, U{}))* = nullptr) const {
                return Op{}(a, b);
            }

            template <typename... T>
            RValue operator()(T const&...) const {
                throw std::runtime_error("Incompatible operands to " + demangle(typeid(Op).name()));
            }
        };

        template <typename Op>
        struct Logical : OperationBase {
            using result_type = RValue;

            template <typename T>
            RValue operator()(T const& a) const { return boolcast(Op{}(truthy(a))); }
            template <typename T, typename U>
            RValue operator()(T const& a, U const& b) const { return boolcast(Op{}(truthy(a), truthy(b))); }
        };

        template <typename Op>
        struct Relational : OperationBase {
            using result_type = RValue;

            template <typename T, typename U>
            RValue operator()(T const& a, U const& b, decltype(Op{}(T{}, U{}))* = nullptr) const
            {
                return boolcast(Op{}(a, b));
            }

            template <typename... T>
            RValue operator()(T const&...) const {
                throw std::runtime_error("Incompatible operands to " + demangle(typeid(Op).name()));
            }
        };

        struct Modulus : std::modulus<> {
            auto operator()(Ast::Number const& a, Ast::Number const& b) const {
                return fmod(a, b);
            }
            using std::modulus<>::operator();
        };
    }

    struct Evaluator {
        using result_type = Dynamic;

        LValue _ctx;

        Evaluator(LValue root) : _ctx(root) {}
        Evaluator(Evaluator const&) = default;

        template <typename... T> Dynamic operator()(T&&...args) {
            return call(std::forward<T>(args)...);
        }

        std::function<void(LValue variable)> on_access;
        std::function<void(Ast::Call const& call, Values const& params, RValue value)> on_invoke;
        std::function<void(LValue dest, RValue value)> on_assign;

    private:
        Dynamic access(LValue ctx, Ast::Identifier const& id) {
            LValue var = ctx[id];
            if (on_access) on_access(var);
            return var;
        }

        template <typename... T> Dynamic call(boost::variant<T...> const& v) {
            return boost::apply_visitor(*this, v);
        }

        // Expression
        RValue call(Ast::Boolean const& o)    { return o; }
        RValue call(Ast::Number const& o)     { return o; }
        RValue call(Ast::String const& o)     { return o; }
        LValue call(Ast::Identifier const& o) { return access(_ctx, o); }
        Dynamic call(Ast::Member const& o)    { return access(call(o.obj), o.member); }

        RValue call(Ast::Unary const& o) {
            RValue const rhs = call(o.rhs);

            switch (o.op) {
              case Ast::Operator::UnaryPlus:
                  return rhs;
              case Ast::Operator::UnaryMinus:
                  return boost::apply_visitor(detail::Arithmetic<std::negate<>>{}, rhs);
              case Ast::Operator::NOT:
                  return boost::apply_visitor(detail::Logical<std::logical_not<>>{}, rhs);
              default:
                  throw std::runtime_error("Not implemented");
            }
        }

        Dynamic call(Ast::Binary const& o) {
            Dynamic lhs = call(o.lhs); // be sure to evaluate only once
            Dynamic rhs = call(o.rhs);

            ValueV rhsv = lhs.value();
            ValueV lhsv = rhs.value();

            auto arith = [&](auto op) {
                return boost::apply_visitor(detail::Arithmetic<decltype(op)>{}, lhsv, rhsv);
            };
            auto logical = [&](auto op) {
                return boost::apply_visitor(detail::Logical<decltype(op)>{}, lhsv, rhsv);
            };
            auto relational = [&](auto op) {
                return boost::apply_visitor(detail::Relational<decltype(op)>{}, lhsv, rhsv);
            };

            switch (o.op) {
                case Ast::Operator::Minus:     return arith(std::minus<>{});
                case Ast::Operator::Plus:      return arith(std::plus<>{});
                case Ast::Operator::Mult:      return arith(std::multiplies<>{});
                case Ast::Operator::Div:       return arith(std::divides<>{});
                case Ast::Operator::Mod:       return arith(detail::Modulus{});
                    //   logical
                case Ast::Operator::AND:       return logical(std::logical_and<>{});
                case Ast::Operator::OR:        return logical(std::logical_or<>{});
                case Ast::Operator::XOR:       return logical(std::not_equal_to<>{});
                    //   relational
                case Ast::Operator::Equal:     return relational(std::equal_to<>{});
                case Ast::Operator::NotEq:     return relational(std::not_equal_to<>{});
                case Ast::Operator::Less:      return relational(std::less<>{});
                case Ast::Operator::LessEq:    return relational(std::less_equal<>{});
                case Ast::Operator::Greater:   return relational(std::greater<>{});
                case Ast::Operator::GreaterEq: return relational(std::greater_equal<>{});
                case Ast::Operator::Assign: {
                    LValue dest = lhs;

                    if (on_assign) on_assign(dest, rhs);
                    dest._value = rhs; // Or: Replacing the entire obj/arr: dest = rhs.lvalue();

                    return dest;
                }
                default: throw std::runtime_error("Invalid operator");
            }
        }

        Dynamic call(Ast::Ternary const& o) {
            return detail::truthy(call(o.cond))
                ? call(o.true_)
                : call(o.false_);
        }

        RValue call(Ast::Call const& o) {
            Values params;
            for (auto& p : o.params) {
                params.push_back(call(p));
            }
            LValue fun = call(o.fun);
            if (auto* f = boost::get<Function>(&fun._value)) {
                RValue r = (*f)(params);
                if (on_invoke) on_invoke(o, params, r);
                return r;
            }
            throw std::runtime_error("Invalid callable");
        }

        LValue call(Ast::Subscript const& o) {
            Values indices;
            for (auto& i : o.indices) {
                indices.push_back(call(i));
            }
            LValue obj = call(o.obj);
            return obj[indices];
        }

        Dynamic call(Ast::SubExpression const& o) {
            return call(o.sub);
        }
    };
}
