#include "ast_gen.h"

namespace AstGen {

    Gen Gen::operator[](ListGen l) && {
        return { Ast::Subscript { std::move(wrapped), std::move(l.elements) } };
    }

    Gen Gen::MemGen::operator[](ListGen l) && { return std::move(*this).gen()[std::move(l)]; }
    Gen Gen::MemGen::operator[](ListGen l) const& { return gen()[std::move(l)]; }

}
