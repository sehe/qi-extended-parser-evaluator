#include "parser.h"
namespace Parser {
    static Parser::Expression<std::string::const_iterator> const s_parser {};

    Ast::Expression parse_expression(std::string const& text) {
        Ast::Expression expr;

        auto f = text.begin(), l = text.end();
        if (!qi::parse(f, l, s_parser >> qi::omit[*qi::space],
                       expr)) { // to allow trailing whitespace
            throw InvalidExpression();
        }
        if (f != l) {
            throw RemainingUnparsed(std::string(f, l));
        }
        return expr;
    }

    bool check_ast(std::string const& txt, Ast::Expression const& expected) {
        auto const& actual = parse_expression(txt);
        bool ok = (expected == actual);
        if (ok) {
            //std::cout << txt << " -> " << Ast::MyLang    << actual << " PASSED\n";
            //std::cout << txt << " -> " << Ast::CxxCompat << actual << " PASSED\n";
        }
        else
            std::cout << txt << " -> " << actual << " FAILED (expected " << expected << " instead)\n";

        return ok;
    }
}
