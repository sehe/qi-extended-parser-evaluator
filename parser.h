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

        /*
         * Due to the way the term_(n) rule is designed, we know that both
         * rhs/lhs have lower precedence than the binding operator.
         *
         * We also know that the default ast is left-associative due to the
         * PEG production.
         *
         * This means that only in right-to-right associative situations we may
         * need to transform the AST to reflect that.
         */
        template<> struct make_<Ast::Binary> {
            struct FixRTL { // binary visitor assuming equal precedence
                Ast::Operator op;

                template <typename E>
                Ast::Binary operator()(Ast::Binary const& lb, E const& rhs) const {
                    // (lb.lhs ? lb.rhs) [op] rhs --> lb.lhs ? (lb.rhs [op] rhs)
                    return { lb.lhs, Ast::Binary { lb.rhs, rhs, op }, lb.op };
                }

                template <typename... E>
                Ast::Binary operator()(E const&... oper) const { return { oper..., op }; }
            };

            Ast::Binary operator()(Ast::Expression const& lhs, Ast::Expression const& rhs, Ast::Operator op) const {
                if (associativity(op) == Ast::Associativity::RTL && 
                    precedence(op) == precedence(lhs)) 
                {
                    return boost::apply_visitor(FixRTL {op}, lhs, rhs);
                }
                return { lhs, rhs, op };
            }
        };
    }

    template <typename It>
    struct Expression : qi::grammar<It, Ast::Expression()> {
        Expression() : Expression::base_type(start) {
            identifier_ = qi::char_("a-zA-Z_") >> *qi::char_("a-zA-Z0-9_");
            quoted_string = '"' >> *(R"("")" >> qi::attr('"') | ~qi::char_('"')) >> '"';

            for (auto& def : Ast::operators())
                _ops[def.level].add(def.token, def.op);

            using namespace boost::spirit::labels;
            unary_ = (qi::no_case[_ops[0]] >> simple_)[_val = make_unary(_1, _2)];

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

            qi::_r1_type precedence_;
            term_
                = qi::eps(precedence_ > 0)
                  >> term_(precedence_ - 1) [_val = _1]
                  >> *( (qi::no_case[qi::lazy(px::ref(_ops)[precedence_])] >> term_(precedence_ - 1))
                            [_val = make_binary(_val, _2, _1)]
                      )
                | simple_ [_val = _1]
                ;

            expression_
                = term_(7) [_val = _1]
                >> *(
                    ("if" >> expression_ >> "else" >> expression_) [_val = make_ternary(_val, _1, _2)]
                  | (qi::no_case[_ops[8]] >> term_(7)) [_val = make_binary(_val, _2, _1)]
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

        using Ops = qi::symbols<char, Ast::Operator>;
        std::map<int, Ops> _ops;

        struct bool_sym : qi::symbols<char, Ast::Boolean> {
            bool_sym() {
                this->add
                    ("false", Ast::Boolean::False)
                    ("true", Ast::Boolean::True)
                    ;
            }
        } bool_;

        qi::rule<It, Ast::Expression()> start;
        qi::rule<It, Ast::Expression(),    qi::blank_type> expression_, simple_;
        qi::rule<It, Ast::Expression(int), qi::blank_type> term_;
        qi::rule<It, Ast::Expressions(),   qi::blank_type> list_;
        qi::rule<It, Ast::Unary(),         qi::blank_type> unary_;

        // implicit lexemes
        qi::real_parser<Ast::Number>    number_;
        qi::rule<It, Ast::Identifier()> identifier_;
        qi::rule<It, Ast::String()>     quoted_string;
    };

    Ast::Expression parsed(std::string const& text);
    bool check_ast(std::string const& txt, Ast::Expression expected);
}
