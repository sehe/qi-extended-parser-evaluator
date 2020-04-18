#include "ast.h"
#include <iostream>

namespace Ast {
    // Operator definitions / metadata
    std::vector<OperatorDef> const& operators() {
        using Op = Operator;

        // levels after https://en.cppreference.com/book/operator_precedence
        static std::vector<OperatorDef> const s_table {
            // level 0 is unary ops
            {0, "+",   Op::UnaryPlus},
            {0, "-",   Op::UnaryMinus},
            {0, "not", Op::NOT},

            // left-to-right binary ops follow
            {1, "*",   Op::Mult},
            {1, "/",   Op::Div},
            {1, "%",   Op::Mod},

            {2, "+",   Op::Plus},
            {2, "-",   Op::Minus},

            {3, "<",   Op::Less},
            {3, "<=",  Op::LessEq},
            {3, ">",   Op::Greater},
            {3, ">=",  Op::GreaterEq},

            {4, "=",   Op::Equal},
            {4, "<>",  Op::NotEq},

            {5, "and", Op::AND},
            {6, "xor", Op::XOR},
            {7, "or",  Op::OR},
            // level 8 is RTL associative
            {8, ":=",  Op::Assign},
            {8, "if/else",  Op::Conditional}, // fake entry, for precedence only
            {0, "?", Op::NONE}
        };
        return s_table;
    }

    static OperatorDef const& lookup(Operator op) {
        for (auto& def : operators())
            if (op == def.op)
                return def;
        throw std::logic_error("Undefined operator");
    }

}

namespace Ast { // IO
    static int cookie() {
        static int s_cookie = std::ios_base::xalloc();
        return s_cookie;
    }

    std::ostream& operator<<(std::ostream& os, FormatManip m) {
        os.iword(cookie()) = static_cast<long>(m.fmt);
        return os;
    }

    static Format IOFmt(std::ios_base& os) {
        return static_cast<Format>(os.iword(cookie()));
    }

    namespace {
        struct DefitionF {
            using result_type = OperatorDef const&;

            OperatorDef const& none_ = lookup(Operator::NONE);

            template <typename... T> OperatorDef const& operator()(T const&... args) const { return call(args...); }

          private:
            OperatorDef const& call(Expression const& e) const { return boost::apply_visitor(*this, e); }
            OperatorDef const& call(Boolean    const& )  const { return none_; }
            OperatorDef const& call(Number     const& )  const { return none_; }
            OperatorDef const& call(String     const& )  const { return none_; }
            OperatorDef const& call(Identifier const& )  const { return none_; }
            OperatorDef const& call(Member     const& )  const { return none_; }
            OperatorDef const& call(Call       const& )  const { return none_; }
            OperatorDef const& call(Subscript  const& )  const { return none_; }
            OperatorDef const& call(SubExpression const& ) const { return none_; }
            OperatorDef const& call(Ternary    const& )  const { return lookup(Operator::Conditional); }
            OperatorDef const& call(Binary const& e) const { return lookup(e.op); }
            OperatorDef const& call(Unary const& e) const { return lookup(e.op); }
        };

        static inline const DefitionF get_def {};
    }

    OperatorDef const& operator_def(Operator op) {
        return lookup(op);;
    }

    OperatorDef const& operator_def(Expression const& e) {
        return get_def(e);
    }

    // If our operands have higher precedence, add parentheses.
    //
    // In the case of equal precedence, we base it on the associativy of the
    // surrounding expression
    static inline std::ostream& relative(std::ostream& os, Precedence to, Expression const& oper, bool override_associativity = false) {
        auto level = operator_def(oper).precedence;
        if (to < level || (to == level && override_associativity))
            return os << "(" << oper << ")";
        return os << oper;
    }
    std::ostream& operator<<(std::ostream& os, Boolean const&b) {
        switch (b) {
            case Boolean::False: return os << "False";
            case Boolean::True:  return os << "True";
        }
        return os << "?bool?";
    }

    std::ostream& operator<<(std::ostream& os, Operator const&op) {
        if (IOFmt(os) == Format::CxxCompat) {
            switch (op) {
                case Operator::AND:       return os << "&";
                case Operator::NOT:       return os << "!";
                case Operator::OR:        return os << "|";
                case Operator::XOR:       return os << "^";
                case Operator::Equal:     return os << "==";
                case Operator::NotEq:     return os << "!=";
                case Operator::Assign:    return os << "=";
                default: break;
            }
        }

        if (auto& def = lookup(op))
            return os << def.token;
        return os << "?op?";
    }

    std::ostream& operator<<(std::ostream& os, Expressions const&ee) {
        auto first = true;
        for (auto& e : ee) {
            if (!std::exchange(first, false))
                os << ", ";
            os << e;
        }
        return os;
    }
    std::ostream& operator<<(std::ostream&os, Member const&o) {
        return os << o.obj << "." << o.member;
    }
    std::ostream& operator<<(std::ostream&os, Unary const&o) {
        return relative(os << o.op << " ", get_def(o).precedence, o.rhs);
    }
    std::ostream& operator<<(std::ostream&os, Binary const&o) {
        auto& def = operator_def(o);

        relative(os, def.precedence, o.lhs, def.right_to_left_associative());
        os << " " << o.op << " ";
        relative(os, def.precedence, o.rhs, !def.right_to_left_associative());
        return os;
    }
    std::ostream& operator<<(std::ostream&os, Ternary const&o) {
        auto re = [&](Expression const& e) -> auto& {
            static const auto precedence = lookup(Operator::Conditional).precedence;
            return relative(os, precedence, e, true);
        };
        if (Format::CxxCompat == IOFmt(os)) {
            re(o.cond) << "? ";
            re(o.true_) << " : ";
            return re(o.false_);
        } else {
            re(o.true_) << " if ";
            re(o.cond) << " else ";
            return re(o.false_);
        }
    }
    std::ostream& operator<<(std::ostream&os, Call const&o) {
        return os << o.fun << "(" << o.params << ")";
    }
    std::ostream& operator<<(std::ostream&os, Subscript const&o) {
        return os << o.obj << "[" << o.indices << "]";
    }
    std::ostream& operator<<(std::ostream&os, SubExpression const&o) {
        return os << "(" << o.sub << ")";
    }
}
