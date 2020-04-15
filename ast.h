#pragma once
#include <iostream>
//#define BOOST_SPIRIT_DEBUG

#include <boost/multiprecision/cpp_dec_float.hpp>
#include <boost/variant.hpp>
#include <iomanip>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

namespace Ast {
    using Identifier = std::string;
    struct String : std::string {
        String(std::string const& s) : std::string(s) {}
        using std::string::string;
        using std::string::operator=;

    private:
        std::string const& str() const { return *this; }
        std::string      & str() { return *this; }

        friend std::ostream& operator<<(std::ostream& os, String const& s) {
            return os << std::quoted(s.str());
        }
    };

    enum class Boolean { False, True };
    // using Number = boost::multiprecision::cpp_dec_float_100;
    // Disabling expression templates (deferred evaluation):
    using Number = boost::multiprecision::number<
       boost::multiprecision::cpp_dec_float<100>, 
       boost::multiprecision::et_off>;

    enum class Operator {
        UnaryPlus, UnaryMinus,
        Plus, Minus, Mult, Div, Mod,
        Less, LessEq, Greater, GreaterEq, Equal, NotEq,
        NOT, AND, OR, XOR,
        Assign, Conditional,
    };

    using Precedence = int;
    enum class Associativity { LTR, RTL };

    struct OperatorDef {
        Precedence level;
        Associativity assoc;
        std::string_view token;
        Operator op;
    };
    std::vector<OperatorDef> const& operators();

    struct Member;
    struct Unary;
    struct Binary;
    struct Ternary;
    struct Call;
    struct Subscript;

    using Expression = boost::make_recursive_variant<
        Boolean,
        Number,
        String,
        Identifier,
        boost::recursive_wrapper<Member>,
        boost::recursive_wrapper<Unary>,
        boost::recursive_wrapper<Binary>,
        boost::recursive_wrapper<Ternary>,
        boost::recursive_wrapper<Call>,
        boost::recursive_wrapper<Subscript>,
        boost::recursive_variant_
    >::type;

    Precedence precedence(Expression const& e);
    Precedence precedence(Operator op);
    Associativity associativity(Expression const& e);
    Associativity associativity(Operator op);

    using Expressions = std::vector<Expression>;

    struct Member { Expression obj; Identifier member; };
    struct Unary { Operator op; Expression rhs; };
    struct Binary { Expression lhs, rhs; Operator op; };
    struct Ternary { Expression true_, cond, false_; };
    struct Call { Expression fun; Expressions params; };
    struct Subscript { Expression obj; Expressions indices; };
}

namespace Ast {
    static inline bool operator==(Member     const& a, Member     const& b) ;
    static inline bool operator==(Unary      const& a, Unary      const& b) ;
    static inline bool operator==(Binary     const& a, Binary     const& b) ;
    static inline bool operator==(Ternary    const& a, Ternary    const& b) ;
    static inline bool operator==(Call       const& a, Call       const& b) ;
    static inline bool operator==(Subscript  const& a, Subscript  const& b) ;

    static inline bool operator==(Member     const& a, Member     const& b) {
        return std::tie(a.obj, a.member) == 
               std::tie(b.obj, b.member);
    }
    static inline bool operator==(Unary      const& a, Unary      const& b) {
        return std::tie(a.op, a.rhs) == 
               std::tie(b.op, b.rhs);
    }
    static inline bool operator==(Binary     const& a, Binary     const& b) {
        return std::tie(a.lhs, a.op, a.rhs) == 
               std::tie(b.lhs, b.op, b.rhs);
    }
    static inline bool operator==(Ternary    const& a, Ternary    const& b) {
        return std::tie(a.true_, a.cond, a.false_) == 
               std::tie(b.true_, b.cond, b.false_);
    }
    static inline bool operator==(Call       const& a, Call       const& b) {
        return std::tie(a.fun, a.params) == 
               std::tie(b.fun, b.params);
    }
    static inline bool operator==(Subscript  const& a, Subscript  const& b) {
        return std::tie(a.obj, a.indices) == 
               std::tie(b.obj, b.indices);
    }
}

namespace Ast {
    enum class Format { MyLang, CxxCompat };

    struct FormatManip {
        constexpr FormatManip(Format fmt) : fmt(fmt) {}
        Format fmt;
        friend std::ostream& operator<<(std::ostream& os, FormatManip m);
    };
    static inline constexpr FormatManip MyLang {Format::MyLang};
    static inline constexpr FormatManip CxxCompat {Format::CxxCompat};

    std::ostream& operator<<(std::ostream& os, Boolean const&b);
    std::ostream& operator<<(std::ostream& os, Operator const&op);
    std::ostream& operator<<(std::ostream& os, Expressions const&ee);
    std::ostream& operator<<(std::ostream& os, Member const&o);
    std::ostream& operator<<(std::ostream& os, Unary const&o);
    std::ostream& operator<<(std::ostream& os, Binary const&o);
    std::ostream& operator<<(std::ostream& os, Ternary const&o);
    std::ostream& operator<<(std::ostream& os, Call const&o);
    std::ostream& operator<<(std::ostream& os, Subscript const&o);

    struct Simplify {
        using result_type = Expression;

        template <typename T> Expression operator()(T const& v) const {
            return call(v);
        }

      private:
        auto call(Boolean    const& v) const { return v; }
        auto call(Number     const& v) const { return v; }
        auto call(String     const& v) const { return v; }
        auto call(Identifier const& v) const { return v; }
        auto call(Expressions vv)      const { for (auto& v : vv) v = call(v); return vv; }

        Member    call(Member    const& v) const { return { call(v.obj), call(v.member) }; }
        Unary     call(Unary     const& v) const { return { v.op, call(v.rhs) }; }
        Binary    call(Binary    const& v) const { return { call(v.lhs), call(v.rhs), v.op }; }
        Ternary   call(Ternary   const& v) const { return { call(v.true_), call(v.cond), call(v.false_) }; }
        Call      call(Call      const& v) const { return { call(v.fun), call(v.params) }; }
        Subscript call(Subscript const& v) const { return { call(v.obj), call(v.indices) }; }

        Expression call(Expression const& v) const {
            if (auto* sub = boost::get<Expression>(&v))
                return *sub; // remove redundant levels sub-nodes
            return boost::apply_visitor(*this, v);
        }
    };
}
