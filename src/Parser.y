{
module Parser (
    parseProg
) where

import Syntax
import Lexer
}

%name prog Prog
%tokentype { Token }
%error { parseError }

%token
    '*'     { TStar }
    '[]'    { TBox }
    ':'     { TColon }
    '\\'    { TLambda }
    '.'     { TDot }
    '('     { TLParen }
    ')'     { TRParen }
    '->'    { TArrow }
    '='     { TEquals }
    ';'     { TSemicolon }
    var     { TVar $$ }

%right '->'

%%

Prog : Decl ';' Prog                    { DSeq $1 $3 }
     | Bind ';' Prog                    { BSeq $1 $3 }
     | Term                             { Tm $1 }

Bind : var '=' Term                     { ($1, $3) }

Decl : var ':' Term                     { ($1, $3) }

Term : Sort                             { Srt $1 }
     | '\\' var ':' Term '.' Term       { Lam $2 $4 $6 }
     | '(' var ':' Term ')' '->' Term   { Pi $2 $4 $7 }
     | Term '->' Term                   { Pi "_" $1 $3 }
     | Fact                             { $1 }

Fact : Fact Atom                        { App $1 $2 }
     | Atom                             { $1 }

Atom : '(' Term ')'                     { $2 }
     | var                              { Var $1 }

Sort : '*'                              { Star }
     | '[]'                             { Box }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseProg :: String -> Prog
parseProg = prog . scanTokens
}