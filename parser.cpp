#include "parser.h"

namespace Parser {
    Ast::Expression parsed(std::string const& text) {
        Ast::Expression expr;
        if (!phrase_parse(
                text.begin(), text.end(),
                Parser::Expression<std::string::const_iterator>() >> qi::eoi,
                qi::space, // to allow trailing whitespace
                expr))
        {
            throw std::runtime_error("illegal expression");
        }
        return expr;
    }

    bool check_ast(std::string const& txt, Ast::Expression expected) {
        Ast::Simplify clean;
        expected = clean(expected);
        auto const actual = clean(parsed(txt));
        bool ok = (expected == actual);
        if (ok)
        {
            //std::cout << txt << " -> " << Ast::MyLang    << actual << " PASSED\n";
            //std::cout << txt << " -> " << Ast::CxxCompat << actual << " PASSED\n";
        }
        else
            std::cout << txt << " -> " << actual << " FAILED (expected " << expected << " instead)\n";

        return ok;
    }
}
