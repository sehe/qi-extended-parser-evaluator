#pragma once
//#define BOOST_SPIRIT_DEBUG
#include "ast.h"
#include <boost/spirit/include/qi.hpp>
#include <boost/core/ignore_unused.hpp>
#include <boost/fusion/adapted.hpp>
#include <boost/spirit/include/phoenix.hpp>

namespace qi = boost::spirit::qi;
namespace px = boost::phoenix;

namespace Parser {
    namespace {
        template <typename T> struct make_ {
            template <typename... Args>
            auto operator()(Args const&... args) const {
                return T{ args... };
            }
        };

        template<> struct make_<Ast::Unary> {
            Ast::Unary operator()(Ast::OperatorDef const* opdef, Ast::Expression const& operand) const {
                return { opdef->op, operand };
            }
        };

        /*
         * Instead of fracturing the precedence levels throughout the parser,
         * we compose binary operators in "Shunting-Yard" style.
         *
         * We also know that the default ast is left-associative due to the
         * PEG production.
         *
         * This means that only in right-to-right associative situations we may
         * need to transform the AST to reflect that.
         */
        template<> struct make_<Ast::Binary> {
            Ast::Binary operator()(Ast::Expression const& lhs, Ast::Expression const& rhs, Ast::OperatorDef const* opdef) const {
                if (auto* lhs_binary = boost::get<Ast::Binary>(&lhs)) {
                    auto& lhopdef = operator_def(lhs_binary->op);
                    bool shuffle = opdef->precedence < lhopdef.precedence 
                        || (opdef->right_to_left_associative() && opdef->precedence == lhopdef.precedence);

                    if (shuffle) {
                        // (L.lhs ? L.rhs) [op] rhs --> L.lhs ? (L.rhs [op] rhs)
                        return { lhs_binary->lhs, Ast::Binary { lhs_binary->rhs, rhs, opdef->op }, lhs_binary->op };
                    }
                }
                return { lhs, rhs, opdef->op };
            }
        };
    }

    template <typename It>
    struct Expression : qi::grammar<It, Ast::Expression()> {
        Expression() : Expression::base_type(start) {
            identifier_ = qi::char_("a-zA-Z_") >> *qi::char_("a-zA-Z0-9_");
            quoted_string = '"' >> *(R"("")" >> qi::attr('"') | ~qi::char_('"')) >> '"';

            for (auto& def : Ast::operators()) {
                if (def.op == Ast::Operator::NONE)
                    continue;
                [&]() -> Ops& {
                    switch(def.precedence) {
                        case 0:  return _unops; // these happen to be unaries
                        case 8:  return _rtlops; // these are special-cased (ternary and assignment)
                        default: return _binops;
                    }
                }().add(def.token, &def);
            }

            using namespace boost::spirit::labels;
            unary_ = (qi::no_case[_unops] >> simple_)[_val = make_unary(_1, _2)];

            simple_
                = ('(' >> expression_ >> ')'
                    | quoted_string
                    | unary_
                    | qi::no_case[bool_]
                    | identifier_
                    | number_
                 ) [_val = _1] >>
                *(
                    ('.' >> identifier_) [_val = make_member(_val, _1)]
                  | ('(' >> list_ >> ')')[_val = make_call(_val, _1)]
                  | ('[' >> list_ >> ']')[_val = make_subscript(_val, _1)]
                );

            term_
                = simple_ [_val = _1] >> *(
                   (qi::no_case[_binops] >> simple_) [_val = make_binary(_val, _2, _1)]
                );

            expression_
                = term_ [_val = _1]
                >> *(
                    ("if" >> expression_ >> "else" >> expression_) [_val = make_ternary(_val, _1, _2)]
                  | (qi::no_case[_rtlops] >> term_) [_val = make_binary(_val, _2, _1)]
                );

            list_ = -(expression_ % ',');

            start = qi::skip(qi::blank)[expression_];

            BOOST_SPIRIT_DEBUG_NODES(
                (start)(expression_)(term_)(simple_)
                (quoted_string)(identifier_)(unary_)
            )
        }

    private:
        px::function<make_<Ast::Binary>    > make_binary{};
        px::function<make_<Ast::Unary>     > make_unary{};
        px::function<make_<Ast::Ternary>   > make_ternary{};
        px::function<make_<Ast::Member>    > make_member{};
        px::function<make_<Ast::Call>      > make_call{};
        px::function<make_<Ast::Subscript> > make_subscript{};

        using Ops = qi::symbols<char, Ast::OperatorDef const*>;
        Ops _unops, _binops, _rtlops;

        struct bool_sym : qi::symbols<char, Ast::Boolean> {
            bool_sym() {
                this->add
                    ("false", Ast::Boolean::False)
                    ("true", Ast::Boolean::True)
                    ;
            }
        } bool_;

        qi::rule<It, Ast::Expression()> start;
        qi::rule<It, Ast::Expression(),  qi::blank_type> expression_, simple_;
        qi::rule<It, Ast::Expression(),  qi::blank_type> term_;
        qi::rule<It, Ast::Expressions(), qi::blank_type> list_;
        qi::rule<It, Ast::Unary(),       qi::blank_type> unary_;

        // implicit lexemes
        qi::real_parser<Ast::Number>    number_;
        qi::rule<It, Ast::Identifier()> identifier_;
        qi::rule<It, Ast::String()>     quoted_string;
    };

    struct ParseError : std::runtime_error {
        using std::runtime_error::runtime_error;
    };
    struct InvalidExpression : ParseError {
        InvalidExpression() : ParseError("InvalidExpression") {}
    };
    struct RemainingUnparsed : ParseError {
        std::string const trailing;

        RemainingUnparsed(std::string trailing)
            : ParseError("RemainingUnparsed: " + trailing), trailing(std::move(trailing))
        {}
    };

    Ast::Expression parse_expression(std::string const& text);
    bool check_ast(std::string const& txt, Ast::Expression expected);
}
