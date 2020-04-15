#include "ast.h"
#include <iostream>

namespace Ast {
    // Operator definitions / metadata
    std::vector<OperatorDef> const& operators() {
        using Assoc = Associativity;
        using Op = Operator;
        auto bin = [](Precedence p, std::string_view tok, Op op) {
            return OperatorDef{p, Arity::Binary, Assoc::LTR, tok, op};
        };

        // levels after https://en.cppreference.com/book/operator_precedence
        static std::vector<OperatorDef> const s_table {
            // level 0 is unary ops
            {0, Arity::Unary, Assoc::LTR, "+",   Op::Plus},
            {0, Arity::Unary, Assoc::LTR, "-",   Op::Minus},
            {0, Arity::Unary, Assoc::LTR, "not", Op::NOT},

            // left-to-right binary ops follow
            bin(1, "*",   Op::Mult),
            bin(1, "/",   Op::Div),
            bin(1, "%",   Op::Mod),

            bin(2, "+",   Op::Plus),
            bin(2, "-",   Op::Minus),

            bin(3, "<",   Op::Less),
            bin(3, "<=",  Op::LessEq),
            bin(3, ">",   Op::Greater),
            bin(3, ">=",  Op::GreaterEq),

            bin(4, "=",   Op::Equal),
            bin(4, "<>",  Op::NotEq),

            bin(5, "and", Op::AND),
            bin(6, "xor", Op::XOR),
            bin(7, "or",  Op::OR),
            // Ternary belongs here, but doesn't fit the pattern
            {8, Arity::Binary, Assoc::RTL, ":=",  Op::Assign},
            // fake entry, for precedence only
            {8, Arity::Binary, Assoc::RTL, "if/else",  Op::Conditional},
        };
        return s_table;
    }

    static OperatorDef const* by_op(Operator op, Arity arity) {
        for (auto& def : operators())
            if (op == def.op && arity == def.arity)
                return &def;
        return nullptr;
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
            using result_type = OperatorDef const*;

            template <typename... T> OperatorDef const* operator()(T const&... args) const { return call(args...); }

          private:
            OperatorDef const* call(Expression const& e) const { return boost::apply_visitor(*this, e); }
            OperatorDef const* call(Boolean    const& )  const { return nullptr; }
            OperatorDef const* call(Number     const& )  const { return nullptr; }
            OperatorDef const* call(String     const& )  const { return nullptr; }
            OperatorDef const* call(Identifier const& )  const { return nullptr; }
            OperatorDef const* call(Member     const& )  const { return nullptr; }
            OperatorDef const* call(Call       const& )  const { return nullptr; }
            OperatorDef const* call(Subscript  const& )  const { return nullptr; }
            OperatorDef const* call(Ternary    const& )  const { return by_op(Operator::Conditional, Arity::Binary); }
            OperatorDef const* call(Binary const& e) const { return by_op(e.op, Arity::Binary); }
            OperatorDef const* call(Unary const& e) const { return by_op(e.op, Arity::Unary); }
        };

        static inline constexpr DefitionF get_def {};
    }

    Precedence precedence(OperatorDef const* def) {
        return def? def->level : 0;
    }

    Precedence precedence(Expression const& e) {
        return precedence(get_def(e));
    }

    Precedence precedence(Operator op, Arity arity) {
        return precedence(by_op(op, arity));
    }

    Associativity associativity(OperatorDef const* def) {
        return def? def->assoc : Associativity::LTR;
    }

    Associativity associativity(Expression const& e) {
        return associativity(get_def(e));
    }

    Associativity associativity(Operator op, Arity arity) {
        return associativity(by_op(op, arity));
    }

    // If our operands have higher precedence, add parentheses.
    //
    // In the case of equal precedence, we base it on the associativy of the
    // surrounding expression
    static inline std::ostream& relative(std::ostream& os, Precedence to, Expression const& oper, bool override_associativity = false) {
        auto level = precedence(oper);
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

        if (auto* def = by_op(op, Arity::Binary))
            return os << def->token;
        if (auto* def = by_op(op, Arity::Unary))
            return os << def->token;
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
        return relative(os << o.op << " ", precedence(o), o.rhs);
    }
    std::ostream& operator<<(std::ostream&os, Binary const&o) {
        Precedence level = precedence(o);
        bool left_associative = (Associativity::LTR == associativity(o.op, Arity::Binary));
        relative(os, level, o.lhs, !left_associative);
        os << " " << o.op << " ";
        relative(os, level, o.rhs, left_associative);
        return os;
    }
    std::ostream& operator<<(std::ostream&os, Ternary const&o) {
        auto re = [&](Expression const& e) -> auto& {
            return relative(os, by_op(Operator::Conditional, Arity::Binary)->level, e, true);
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
}
