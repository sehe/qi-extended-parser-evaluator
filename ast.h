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

    struct SourceLocation {
        //unsigned line = -1, column = -1, length = 0;
        std::string_view range;
    };

    struct String : std::string {
        String(std::string const& s, SourceLocation loc = {}) : std::string(s), _loc(loc) {}
        using std::string::string;
        using std::string::operator=;

        SourceLocation _loc;

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
        NONE // artificial entry to represent non-operation nodes
    };

    using Precedence = int;

    struct OperatorDef {
        Precedence precedence;
        std::string_view token;
        Operator op;

        bool right_to_left_associative() const { return precedence == 8;      } 
        explicit operator bool() const         { return op != Operator::NONE; } 
    };
    std::vector<OperatorDef> const& operators();

    struct Member;
    struct Unary;
    struct Binary;
    struct Ternary;
    struct Call;
    struct Subscript;
    struct SubExpression;

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
        boost::recursive_wrapper<SubExpression>
    >::type;

    OperatorDef const& operator_def(Operator op);
    OperatorDef const& operator_def(Expression const& e);

    using Expressions = std::vector<Expression>;

    struct Member { Expression obj; Identifier member; SourceLocation _loc; };
    struct Unary { Operator op = Operator::NONE; Expression rhs; SourceLocation _loc; };
    struct Binary { Expression lhs, rhs; Operator op = Operator::NONE; SourceLocation _loc; };
    struct Ternary { Expression true_, cond, false_; SourceLocation _loc; };
    struct Call { Expression fun; Expressions params; SourceLocation _loc; };
    struct Subscript { Expression obj; Expressions indices; SourceLocation _loc; };
    struct SubExpression { Expression sub; SourceLocation _loc; };
}

namespace Ast {
    static inline bool operator==(Member        const& a, Member        const& b) ;
    static inline bool operator==(Unary         const& a, Unary         const& b) ;
    static inline bool operator==(Binary        const& a, Binary        const& b) ;
    static inline bool operator==(Ternary       const& a, Ternary       const& b) ;
    static inline bool operator==(Call          const& a, Call          const& b) ;
    static inline bool operator==(Subscript     const& a, Subscript     const& b) ;
    static inline bool operator==(SubExpression const& a, SubExpression const& b) ;

    static inline bool operator==(Member const& a, Member const& b) {
        return std::tie(a.obj, a.member) == 
               std::tie(b.obj, b.member);
    }
    static inline bool operator==(Unary const& a, Unary const& b) {
        return std::tie(a.op, a.rhs) == 
               std::tie(b.op, b.rhs);
    }
    static inline bool operator==(Binary const& a, Binary const& b) {
        return std::tie(a.lhs, a.op, a.rhs) == 
               std::tie(b.lhs, b.op, b.rhs);
    }
    static inline bool operator==(Ternary const& a, Ternary const& b) {
        return std::tie(a.true_, a.cond, a.false_) == 
               std::tie(b.true_, b.cond, b.false_);
    }
    static inline bool operator==(Call const& a, Call const& b) {
        return std::tie(a.fun, a.params) == 
               std::tie(b.fun, b.params);
    }
    static inline bool operator==(Subscript const& a, Subscript const& b) {
        return std::tie(a.obj, a.indices) == 
               std::tie(b.obj, b.indices);
    }
    static inline bool operator==(SubExpression const& a, SubExpression const& b) {
        return a.sub == b.sub;
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
    std::ostream& operator<<(std::ostream& os, SubExpression const&o);
}
