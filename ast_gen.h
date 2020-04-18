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

        operator Ast::Expression()&& { return std::move(wrapped); }
        operator Ast::Expression() const& { return wrapped; }

        Gen operator=(Gen rhs) && { return Ast::Binary { std::move(wrapped), std::move(rhs), Operator::Assign }; }
        Gen operator=(Gen rhs) const& { return Ast::Binary { wrapped, std::move(rhs), Operator::Assign }; }

        template <typename... T> Gen operator()(T&&... v) && {
            return { Ast::Call { wrapped, {Gen(std::forward<T>(v))...} } };
        }
        Gen operator[](ListGen) &&;

        struct MemGen {
            Ast::Expression obj;
            Ast::Identifier member;

            Gen gen() && { return {Ast::Member {std::move(obj), std::move(member)} }; }
            Gen gen() const& { return {Ast::Member {obj, member} }; }
            operator Ast::Expression() && { return std::move(*this).gen(); }
            operator Ast::Expression() const& { return gen(); }
            template <typename... T> Gen operator()(T&&... v) && {
                return std::move(*this).gen()(std::forward<T>(v)...);
            }
            template <typename... T> Gen operator()(T&&... v) const& {
                return gen()(std::forward<T>(v)...);
            }
            Gen operator[](ListGen) &&;
            Gen operator[](ListGen) const&;
        } const
            foo { wrapped, Ast::Identifier("foo") },
            bar { wrapped, Ast::Identifier("bar") },
            qux { wrapped, Ast::Identifier("qux") };
    };

    static inline Gen operator+(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Plus }}; }
    static inline Gen operator-(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Minus }}; }
    static inline Gen operator*(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Mult }}; }
    static inline Gen operator/(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Div }}; }
    static inline Gen operator%(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Mod }}; }

    static inline Gen operator< (Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Less }}; }
    static inline Gen operator<=(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::LessEq }}; }
    static inline Gen operator> (Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Greater }}; }
    static inline Gen operator>=(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::GreaterEq }}; }
    static inline Gen operator==(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::Equal }}; }
    static inline Gen operator!=(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::NotEq }}; }

    static inline Gen operator& (Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::AND }}; }
    static inline Gen operator| (Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::OR }}; }
    static inline Gen operator^ (Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::XOR }}; }
    static inline Gen operator&&(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::AND }}; }
    static inline Gen operator||(Gen a, Gen b) { return {Ast::Binary { std::move(a), std::move(b), Operator::OR }}; }

    static inline Gen operator+(Gen a) { return {Ast::Unary { Operator::UnaryPlus, std::move(a) }}; }
    static inline Gen operator-(Gen a) { return {Ast::Unary { Operator::UnaryMinus, std::move(a) }}; }
    static inline Gen operator!(Gen a) { return {Ast::Unary { Operator::NOT, std::move(a) }}; }

    struct ListGen {
        std::vector<Ast::Expression> elements;
        ListGen(Gen&& g) : elements{std::move(g)} {}
        ListGen(std::initializer_list<Ast::Expression> i) : elements(i) {}
    };

    static inline ListGen operator,(Gen a, Gen b) { return { {std::move(a),std::move(b)} }; }
    static inline ListGen operator,(ListGen a, Gen b) { a.elements.push_back(std::move(b)); return std::move(std::move(a)); }
}
