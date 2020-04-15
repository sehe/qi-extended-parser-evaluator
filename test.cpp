#include "ast.h"
#include "ast_gen.h"
#include "parser.h"
#include "eval.h"
#include <boost/convert/lexical_cast.hpp>
#include <boost/convert.hpp>

static void generate_cases();
static auto str = boost::cnv::apply<std::string, Ast::Expression>(boost::cnv::lexical_cast());

#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/seq/for_each.hpp>

#define UNITS \
    (aa)(ab)(ac)(ad)(ae)(af)(ag)(ah)(ai)(aj)(ak)(al)(am)(an)(ao)(ap) \
    (aq)(ar)(as)(at)(au)(av)(aw)(ax)(ay)(az)(ba)(bb)(bc)(bd)(be)(bf) \
    (bg)(bh)(bi)(bj)(bk)(bl)(bm)(bn)(bo)(bp)(bq)(br)(bs)(bt)(bu)(bv) \
    (bw)(bx)(by)(bz)(ca)(cb)(cc)(cd)(ce)(cf)(cg)(ch)(ci)(cj)(ck)(cl) \
    (cm)(cn)

#define INVOKE_CHECK_FUN(r, data, elem)  \
    void BOOST_PP_CAT(check_ast_, elem)(size_t& good, size_t& bad); \
    BOOST_PP_CAT(check_ast_, elem)(good, bad);

static inline void run_generated_ast_checks(size_t& good, size_t& bad) {
    boost::ignore_unused(good, bad);
    //BOOST_PP_SEQ_FOR_EACH(INVOKE_CHECK_FUN, _, UNITS)
}

static inline Eval::Value demofunction(Eval::Values const& params) {
#if 1   
    std::ostringstream oss;
    oss << "demofunction returned: c(";
    bool first = true;
    for (auto& param : params)
        oss << (std::exchange(first, false) ? "" : ", ") << param;
    oss << ")";
    return oss.str();
#else
    boost::ignore_unused(params);
    return Ast::Number(23.45);
#endif
}

static inline auto make_context() {
    using namespace Eval;
    Variable ctx;
    Value f = demofunction;
    Value identity = [](Values v) { return v.front(); };
    ctx["a"] = Value(43);
    ctx["b"] = Value(1);
    ctx["c"] = Value(7);
    ctx["a"]["d"][2]["c"] = f;
    ctx["a"]["d"][3]["c"] = f;
    ctx["a"]["b"]["c"] = identity;
    ctx["d"]["a"]["c"] = Value(1);

    return ctx;
}

static inline void foo() {
    Ast::Number b;
    Eval::Null a;
    void(a < b);
    void(a == b);
    void(a != b);
}

static bool eval_trace = false;

void check_eval(Eval::Variable context, std::vector<std::string> const& inputs) {
    Parser::Expression<std::string::const_iterator> const g;
    Eval::Evaluator evaluator{ context };

    if (eval_trace) {
        evaluator.on_assign.connect([&context](Eval::LValue dest, Eval::RValue value) {
            std::cout << "Assigning " << value << " to " << context.path_to(dest).value_or("?") << "\n";
        });
        evaluator.on_invoke.connect([](Ast::Call const& call, Eval::RValue retval) {
            std::cout << "Invoking: " << call << " -> " << retval << "\n";
        });
    }

    for (std::string const str : inputs) {
        std::vector<std::pair<std::string, std::string> > accesses;

        boost::signals2::scoped_connection on_access =
            evaluator.on_access.connect([&context,&accesses](Eval::LValue var) {
                if (!eval_trace)
                    return;
                auto path = context.path_to(var).value_or("?");
                if (!accesses.empty() && 0 == path.find(accesses.back().first)) // condense sub-object accesses
                    accesses.back() = {path,boost::lexical_cast<std::string>(var)};
                else
                    accesses.emplace_back(path, boost::lexical_cast<std::string>(var));
            });

        auto iter = str.begin(), end = str.end();

        std::cout << std::quoted(str) << " ";
        Ast::Expression parsed;
        if (phrase_parse(
            iter, end, g >> qi::eoi,
            qi::space, // to allow trailing whitespace
            parsed))
        {
            std::cout << "OK: " << parsed << "\n";

            try {
                Eval::Value outcome = evaluator(parsed);
                std::cout << "Outcome: " << outcome << "\n"; // , context: " << context << std::endl;
            }
            catch (std::exception const& e) {
                std::cout << "Failed: " << e.what() << "\n";
            }
            for (auto& [var,val] : accesses)
                std::cout << "Accessed: " << var << " -> " << val << "\n";

            std::cout << "----------------------\n";
        }
        else {
            std::cout << "Failed\n";
        }
        if (iter != end) {
            std::cout << "Remaining unparsed: " << std::quoted(std::string(iter, end)) << "\n";
        }
    }
}

void run_eval_tests()
{
    make_context().dump(
        std::cout << std::unitbuf << "/** Context: ", true, "\n * ");
    std::cout << "\n*/\n";

    check_eval(make_context(), {
        R"(a > 20)",
        R"(a+10 > 30)",
        R"(a = True)",
        R"(a = b.a.c)",
        R"(not a)",
        R"(a and (b and c))",
        R"(a*34+a.b.c(23))",
        R"(10 if a > b else 30)", // python style unary
        });

    check_eval(make_context(), {
        R"("hello")",
        R"(10)",
        R"(a)",
        R"(a.b)",
        R"(a.b[2])",
        R"(a.d)",
        R"(a.d[2].c(a.e*3,20,"hello"))",
        R"(a.d[3])",
        R"(a.d[3].c(34))",
        R"(a.d[3].c)",
        R"(a.e)",
        R"(b.c.d*20)",
        R"(-20)"
        });

    // Mutability makes the universe wonky:
    check_eval(make_context(), {
        R"(a.b.c := 10)",
        // Invoking a function doesn't yield an LValue
        R"(a.d[3].c(34) := a.b[2])",
        R"(a.d[2].c(a.e*3,20,"hello") := b.c.d*20)",
        // NOTE: replaces a.d entirely!
        R"(a := 10)",
        // Now, this will not invoke a.d[2].c anymore:
        R"(a.d[2].c(a.e*3,20,"hello") := b.c.d*20)",
        R"(a.d := a.b)",
        R"(a.d[3] := 10)",
        R"(a.d[3] := a.b[2])",
        R"(a.e := "hello")",
        });

    check_eval(make_context(), {
        R"(-a)",
        R"(b)",
        R"(a>-b)",
        R"(a<-b)",
        R"(a<a)",
        R"(a<=a)",
        R"(True = (a=a))",
        R"(True = not (False))",
        R"(True xor (a=a))",
        R"(True or (a=a))",
        R"(True and (a=a))",
        R"(False xor (a=a))",
        R"(False or (a=a))",
        R"(False and (a=a))",
        R"(2*8 + 7)",
        R"(2*8 + False)",
        R"(a.d[3].c(34))",
        });

    check_eval(make_context(), {
        R"(-a)",
        R"(b)",
        R"(a-b)",
        R"(b-a)",
        R"(b/a)",
        R"(b*a)",
        R"(b%3)",
        R"(b/0)",
        });

    check_eval(make_context(), {
        R"(2*8 + 7)",
        R"((1*2+3*4+5)*6)",
        R"((1+2*3+4*5)+6)",
        R"(1+2+3+4)",
        R"(-20)",
        R"(1+3*4)",
        R"(1+4/2)",
        R"(1+4%3)",
        R"((1+3)*4)",
        R"((1+4)/2)",
        R"((1+4)%3)",
        R"(True or True and False)",
        R"(False = (1 < 20))", // should be False - OK
        R"(False = 1 < 20)", // should be also False - OK
        R"(False = (1 > 20))", // should be True - OK
        R"(False = 1 > 20)", // should be also True - OK
        R"(10 < 20 = 10 > 20)", // should be False - OK
        R"((false and true) = (false and true))", // should be True - OK
        R"(false and (true = false) and true)", // should be False - OK
        R"(false and true = false and true)", // should be False - OK
        R"(a.b.c(123))",
        R"(a.d[3].c(321))",

        R"((8*2+7)=((8*2)+7))", // True: * stringer binding then +
        R"((8*2-7)=((8*2)-7))", // True: * stringer binding then -
        R"((8/2+7)=((8/2)+7))", // True: / stringer binding then +
        R"((8/2-7)=((8/2)-7))", // True: / stringer binding then -
        R"((8/2*7)=(8/2*7))", // True: * / equal binding
        R"((8/2*7)=(7*8/2))", // True: * / equal binding
        R"((8-2+7)=(8-2+7))", // True: * / equal binding
        R"((8-2+7)=(7+8-2))", // True: * / equal binding
        R"((False = 1 < 20)=(False = (1 < 20)))", // True: '<' stronger binding then '=='
        R"((False = 1 > 20)=(False = (1 > 20)))", // True: '>' stronger binding then '=='
        R"((False = 1 <= 20)=(False = (1 <= 20)))", // True: '<' stronger binding then '=='
        R"((False = 1 >= 20)=(False = (1 >= 20)))", // True: '>' stronger binding then '=='
        R"((False and True = False)=(False and (True = False)))", // True: '==' stronger binding then '&&'
        R"((True or False and False)=(True or (False and False)))", // True: '&&' stronger binding then '||'
        });

    check_eval(make_context(), {
        R"((8*2+7)=((8*2)+7))", // True: * stronger binding then +
        R"((8*2-7)=((8*2)-7))", // True: * stronger binding then -
        R"((8/2+7)=((8/2)+7))", // True: / stronger binding then +
        R"((8/2-7)=((8/2)-7))", // True: / stronger binding then -

        R"((8/2*7)=(8/2*7))", // True: * / equal binding, etc. test
        R"((8/2*7)=(7*8/2))", // True: * / equal binding, etc. test

        R"((8-2+7)=(8-2+7))", // True: + - equal binding, etc. test
        R"((8-2+7)=(7+8-2))", // True: + - equal binding, etc. test // FAILS

        R"(8-2+7)", // 13 // OK
        R"(7+8-2)", // -1 // ??

        R"((False = 1 < 20)=(False = (1 < 20)))", // True: '<' stronger binding then '=='
        R"((False = 1 > 20)=(False = (1 > 20)))", // True: '>' stronger binding then '=='

        R"((False = 1 <= 20)=(False = (1 <= 20)))", // True: '<' stronger binding then '=='
        R"((False = 1 >= 20)=(False = (1 >= 20)))", // True: '>' stronger binding then '=='

        R"((False and True = False)=(False and (True = False)))", // True: '==' stronger binding then '&&'

        R"((True or False and False)=(True or (False and False)))", // True: '&&' stronger binding then '||'
        });

    check_eval(make_context(), {
        "(8/2*7)=(8/2*7)",
        "(8/2*7)=(7*8/2)",
        "(8-2+7)=(8-2+7)",
        "(8-2+7)=(7+8-2)",
        });

    check_eval(make_context(), { "(x)=(x)" });

    check_eval(make_context(), { 
        "a = d.a.c",
        "a",
        "d.a.c",
        "a.d[3].c(34)",
        "a.b[2]",
        "a.d[2].c(a.e*3,20,\"hello\")",
        "b.c.d*20",
        });

    check_eval(make_context(), {
        "a", // not an assignment
        "a.d[3].c(34) := a.b[2]",
        "a.d[2].c(a.e*3,20,\"hello\") := b.c.d*20",
        });
}

static AstGen::Gen const
    x = Ast::Identifier("x"),
    y = Ast::Identifier("y"),
    z = Ast::Identifier("z");

int main(int argc, char const** argv) {
    std::set<std::string> const args { argv+1, argv+argc };
    if (args.count("generate")) {
        generate_cases();
    }

    eval_trace = args.count("trace");

    if (args.count("ast")) {
        size_t good=0, bad=0;

        run_generated_ast_checks(good, bad);

        using Parser::check_ast;
        ++(check_ast("x if y else z",                  (Ast::Ternary { x, y, z }))?good:bad);
        ++(check_ast("x if y else y if z else x",      (Ast::Ternary { x, y, Parser::parsed("y if z else x") }))?good:bad);
        ++(check_ast("x or y if y and z else z xor x", (Ast::Ternary { x or y, y and z, z xor x }))?good:bad);
        ++(check_ast("x if y and z if x else z else z xor x if y else z or x", ( 
             Ast::Ternary { x, Ast::Ternary { y and z, x, z}, Ast::Ternary { z xor x, y, z or x } }))?good:bad);

        ++(check_ast("x = (y if z else x)", (x == Ast::Ternary { y, z, x }))?good:bad);
        ++(check_ast("(x = y) if z else x", (Ast::Ternary { x==y, z, x }))?good:bad);
        ++(check_ast("x = y if z else x", (Ast::Ternary { x==y, z, x }))?good:bad);

        ++(check_ast("x := (y if z else x)", (x = Ast::Ternary { y, z, x }))?good:bad);
        ++(check_ast("(x := y) if z else x", (Ast::Ternary { x=y, z, x }))?good:bad);
        ++(check_ast("x := y if z else x", (Ast::Ternary { x=y, z, x }))?good:bad);

        // take associativity into account
        ++(check_ast("x+y+z", x+y+z)?good:bad);
        ++(check_ast("(x+y)+z", (x+y)+z)?good:bad);
        ++(check_ast("x+(y+z)", x+(y+z))?good:bad);

        ++(check_ast("x:=y:=z", x=y=z)?good:bad);
        ++(check_ast("(x:=y):=z", (x=y)=z)?good:bad);
        ++(check_ast("x:=(y:=z)", x=(y=z))?good:bad);

        auto total = good+bad;
        std::cout
        << "Success: " << good << " out of " << total
        << " (" << std::setprecision(1) << std::fixed << (100.0*good/total) << "%)\n";
    }

    if (args.count("str")) {
        #define CHECK_STR(expr, expected_str) do { \
            std::string const& actual = str(expr); \
            if (actual == expected_str) { \
                std::cout << "OK: " << #expr << " -> " << actual << "\n"; \
            } else { \
                std::cout << "FAILED: " << #expr << " -> " << actual << "\n"; \
                std::cout << " -- expected: " << expected_str << "\n"; \
            } \
        } while(0)

        // check that parentheses are added when needed:
        CHECK_STR(x + (y * z), "x + y * z");
        CHECK_STR(x * (y + z), "x * (y + z)");
        CHECK_STR(x.foo(), "x.foo()");
        CHECK_STR(x.foo(3*y), "x.foo(3 * y)");
        CHECK_STR(x.foo(3*y, z), "x.foo(3 * y, z)");
        CHECK_STR(x.foo[3*y], "x.foo[3 * y]");
        CHECK_STR((x.foo[3*y, z]), "x.foo[3 * y, z]");

        // take associativity into account
        CHECK_STR(x + y + z, "x + y + z");
        CHECK_STR((x + y) + z, "x + y + z");
        CHECK_STR(x + (y + z), "x + (y + z)");

        CHECK_STR(x = y = z, "x := y := z");
        CHECK_STR((x = y) = z, "(x := y) := z");
        CHECK_STR(x = (y = z), "x := y := z");
    }

    if (args.count("eval")) {
        run_eval_tests();
    }
}

void generate_cases() {
    using sv = std::string_view;
    struct token {
        sv txt, dsl=txt;
        token(const char* txt) : token(txt, txt) {}
        token(sv txt, sv dsl) : txt(txt), dsl(dsl) {}
    };
    token unops[] = { "", " +", " -", " not ", };
    token binops[] = {
        "*", "/", "%", "+", "-",
        "<", "<=", ">", ">=", {"=", "=="}, {"<>", "!="}, // TODO {":=", "="}
        // bitwise for consistent c++ precedence:
        {" and ", "&"}, {" xor ", "^"}, {" or ", "|"},
    };

    // 0    1   2     3    4   5     6    7   8
    // [ux] "x" [op1] [uy] "y" [op2] [uz] "z"
    // (    (         (    (         (    (
    //          )              )               )
    std::bitset<9> openloc { 0b011'011'011 }, closeloc { ~openloc };

    for (auto op1: binops) for (auto op2: binops)
    for (auto ux: unops) for (auto uy: unops) for (auto uz: unops)
    {
        std::vector<token> parts {ux, "x", op1, uy, "y", op2, uz, "z"}, wparens;

        for (auto i = 0u; i < parts.size(); ++i)
        for (auto j = i;  j < parts.size(); ++j) {
            if (
                    (openloc[i]  && parts.at(i).txt.size())
                 && (closeloc[j] && parts.at(j).txt.size()))
            {
                wparens = parts;
                wparens.insert(begin(wparens)+i,   "(");
                wparens.insert(begin(wparens)+j+1, ")");
                std::cout << "    CHECK_AST(\"";
                for (auto tok : wparens) std::cout << tok.txt;
                std::cout << "\", ";
                for (auto tok : wparens) std::cout << tok.dsl;
                std::cout << ");\n";
            }
        }
    }
}
