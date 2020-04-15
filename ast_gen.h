#pragma once
#include "ast.h"

namespace AstGen {

    using Ast::Operator;

    struct ListGen;
    struct Gen {
        Ast::Expression wrapped;

        Gen(Gen const&) = default;

        template <typename T> Gen(T v) : wrapped(v) {}
        Gen(int v)                     : wrapped(Ast::Number(v)) {}
        Gen(unsigned long long v)      : wrapped(Ast::Number(v)) {}
        Gen(long double v)             : wrapped(Ast::Number(v)) {}

        operator Ast::Expression const&() const { return wrapped; }

        Gen operator=(Gen const& rhs) const { return Ast::Binary { wrapped, Gen(rhs), Operator::Assign }; }

        template <typename... T> Gen operator()(T const&... v) const {
            return { Ast::Call { wrapped, {Gen(v)...} } };
        }
        Gen operator[](ListGen) const;

        struct MemGen {
            Ast::Expression obj;
            Ast::Identifier member;

            Gen gen() const { return {Ast::Member {obj, member} }; }
            operator Gen() const { return gen(); }
            operator Ast::Expression() const { return gen(); }
            template <typename... T> Gen operator()(T const&... v) const {
                return gen()(v...);
            }
            Gen operator[](ListGen) const;
        } const
            foo { wrapped, Ast::Identifier("foo") },
            bar { wrapped, Ast::Identifier("bar") },
            qux { wrapped, Ast::Identifier("qux") };
    };

    static inline Gen operator+(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Plus }}; }
    static inline Gen operator-(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Minus }}; }
    static inline Gen operator*(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Mult }}; }
    static inline Gen operator/(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Div }}; }
    static inline Gen operator%(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Mod }}; }

    static inline Gen operator< (Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Less }}; }
    static inline Gen operator<=(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::LessEq }}; }
    static inline Gen operator> (Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Greater }}; }
    static inline Gen operator>=(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::GreaterEq }}; }
    static inline Gen operator==(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::Equal }}; }
    static inline Gen operator!=(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::NotEq }}; }

    static inline Gen operator& (Gen a, Gen b) { return {Ast::Binary { a, b, Operator::AND }}; }
    static inline Gen operator| (Gen a, Gen b) { return {Ast::Binary { a, b, Operator::OR }}; }
    static inline Gen operator^ (Gen a, Gen b) { return {Ast::Binary { a, b, Operator::XOR }}; }
    static inline Gen operator&&(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::AND }}; }
    static inline Gen operator||(Gen a, Gen b) { return {Ast::Binary { a, b, Operator::OR }}; }

    static inline Gen operator+(Gen a) { return {Ast::Unary { Operator::UnaryPlus, a }}; }
    static inline Gen operator-(Gen a) { return {Ast::Unary { Operator::UnaryMinus, a }}; }
    static inline Gen operator!(Gen a) { return {Ast::Unary { Operator::NOT, a }}; }

    struct ListGen {
        std::vector<Ast::Expression> elements;
        ListGen(Gen g) : elements{g} {}
        ListGen(std::initializer_list<Ast::Expression> i) : elements(i) {}
    };

    static inline ListGen operator,(Gen a, Gen b) { return { {a,b} }; }
    static inline ListGen operator,(ListGen a, Gen b) { a.elements.push_back(b); return a; }
}
