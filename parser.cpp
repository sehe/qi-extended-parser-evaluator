#include "parser.h"
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics.hpp>

namespace Parser {
    static Parser::Expression<std::string::const_iterator> const s_parser {};

    namespace ba = boost::accumulators;
    namespace bat = ba::tag;
    using Accum = boost::accumulators::accumulator_set<size_t, ba::stats<bat::count, bat::min, bat::max, bat::median, bat::variance>>;
    struct Stat { Accum matched, failed; size_t parse_runs = 0; };
    static std::map<std::string, Stat> s_stats;

    Ast::Expression parse_expression(std::string const& text) {
        Ast::Expression expr;

        auto f = text.begin(), l = text.end();
        bool ok = qi::parse(f, l, s_parser >> *qi::space, expr);

        for (auto& [rule, counts] : s_parser.get_and_reset_stats()) {
            auto& s = s_stats[rule];
            s.matched(counts.matched);
            s.failed(counts.failed);
            ++s.parse_runs;
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
        ofs << "rule,result,parse_runs,samples,min,max,median,stddev\n";

        auto extract = [](auto& accum) {
            struct { 
                size_t samples, min, max;
                double median, stddev;
            } r {
                ba::count(accum), ba::min(accum), ba::max(accum),
                ba::median(accum), std::sqrt(ba::variance(accum))
            };
            return r;
        };
        auto report = [&](auto& accum, auto rule, auto result, size_t parse_runs) {
            auto s = extract(accum);
            ofs << rule << "," << result << "," << parse_runs << "," << s.samples << "," <<
                s.min << "," << s.max << "," << s.median << "," << s.stddev << "\n";
        };
        for (auto const& [rule, item] : s_stats) {
            report(item.matched, rule, "MATCHED", item.parse_runs);
            report(item.failed,  rule, "FAILED",  item.parse_runs);
        }
    }
}
