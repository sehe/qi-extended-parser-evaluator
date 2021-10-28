#include "ast_gen.h"

namespace AstGen {

    Gen Gen::operator[](ListGen l) const {
        return {Ast::Subscript{wrapped, l.elements, Ast::SourceLocation{}}};
    }

    Gen Gen::MemGen::operator[](ListGen l) const { return gen()[l]; }

}
