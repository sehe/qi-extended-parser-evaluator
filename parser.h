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
        template <typename V>
        static decltype(auto) unnest(V&& v) {
            return std::forward<V>(v);
        }

        static Ast::Expression unnest(Ast::Expression&& e)
        {
            auto* real = &e;
            while (auto* nested = boost::get<Ast::SubExpression>(real))
                real = &nested->sub;
            return std::move(*real);
        }

        template <typename T> struct make_ {
            template <typename... Args> auto operator()(Args&... args) const
            {
                return T{unnest(std::move(args))..., Ast::SourceLocation{}};
            }
        };

        template<> struct make_<Ast::Unary> {
            Ast::Unary operator()(Ast::OperatorDef const* opdef,
                                  Ast::Expression&        operand) const
            {
                return {opdef->op, unnest(std::move(operand)),
                        Ast::SourceLocation{}};
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
            Ast::Binary operator()(Ast::Expression& lhs, Ast::Expression& rhs,
                                   Ast::OperatorDef const* opdef) const
            {
                if (auto* lhs_binary = boost::get<Ast::Binary>(&lhs)) {
                    auto lhprec = operator_def(lhs_binary->op).precedence;
                    bool shuffle = opdef->precedence < lhprec ||
                        (opdef->right_to_left_associative() &&
                         opdef->precedence == lhprec);

                    if (shuffle) {
                        // (L.lhs ? L.rhs) [op] rhs --> L.lhs ? (L.rhs [op] rhs)
                        return {std::move(lhs_binary->lhs),
                                Ast::Binary{std::move(lhs_binary->rhs),
                                            unnest(std::move(rhs)), opdef->op,
                                            Ast::SourceLocation{}},
                                lhs_binary->op, Ast::SourceLocation{}};
                    }
                }
                return {unnest(std::move(lhs)), unnest(std::move(rhs)),
                        opdef->op, Ast::SourceLocation{}};
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

            simple_
                = (('(' > expression_ > ')')         [_val = make_sub(_1)]
                   | (&qi::lit('"') > quoted_string) [_val = _1]
                   | (qi::no_case[_unops] > simple_) [_val = make_unary(_1, _2)]
                   | (qi::no_case[bool_])            [_val = _1]
                   | (identifier_)                   [_val = _1]
                   | (number_)                       [_val = _1]
                 ) >>
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
                (quoted_string)(identifier_)(list_)
            )

            auto annotate = px::bind(_annotate, _val, _1, _3);
            qi::on_success(start, annotate);
            qi::on_success(expression_,   annotate);
            qi::on_success(term_,         annotate);
            qi::on_success(simple_,       annotate);
            qi::on_success(quoted_string, annotate);
            qi::on_success(identifier_,   annotate);
        }

    private:
        px::function<make_<Ast::Binary>        > make_binary{};
        px::function<make_<Ast::Unary>         > make_unary{};
        px::function<make_<Ast::Ternary>       > make_ternary{};
        px::function<make_<Ast::Member>        > make_member{};
        px::function<make_<Ast::Call>          > make_call{};
        px::function<make_<Ast::Subscript>     > make_subscript{};
        px::function<make_<Ast::SubExpression> > make_sub{};

        struct annotation_f {
          typedef void result_type;

          template <typename Val, typename First, typename Last>
          void operator()(Val &v, First f, Last l) const {
            do_annotate(v, f, l);
          }

        private:
          template <typename Val>
          void static do_annotate(Val& v, It f, It l,
                                  decltype(v._loc)* /*_enable*/ = nullptr)
          {
              // C++20: v._loc.range = {f, l};
              v._loc.range = std::string_view(&*f, std::distance(f, l));
          }
          static void do_annotate(...) {}
        };
        annotation_f _annotate{};

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
    bool check_ast(std::string const& txt, Ast::Expression const& expected);
}
