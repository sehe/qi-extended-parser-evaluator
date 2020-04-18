#include "parser.h"

namespace Parser {
    static Parser::Expression<std::string::const_iterator> const s_parser {};

    static StatsMap s_stats;

    Ast::Expression parse_expression(std::string const& text) {
        Ast::Expression expr;

        auto f = text.begin(), l = text.end();
        bool ok = qi::parse(f, l, s_parser >> *qi::space, expr);

        for (auto& [key, count] : s_parser.get_and_reset_stats()) {
            s_stats[key] += count;
        }

        if (!ok) { // to allow trailing whitespace
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

#include <fstream>
namespace Parser {
    void report_parser_stats(std::string const& fname) {
        std::ofstream ofs(fname);
        ofs << "rule,state,ast,count\n";
        for (auto const& [key, count] : s_stats) {
            auto& [rule, state, ast] = key;
            auto state_str = [state] { switch(state) {
                case qi::debug_handler_state::failed_parse: return "failed_parse";
                case qi::debug_handler_state::pre_parse: return "pre_parse";
                case qi::debug_handler_state::successful_parse: return "successful_parse";
                default: return "unknown";
            }}();
            ofs << rule << "," << state_str << "," << std::quoted(ast) << "," << count << "\n";
        }
    }
}
