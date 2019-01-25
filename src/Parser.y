{
module Parser (
    parseTerm
) where

import Syntax
import Lexer
}

%name term Term
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
    'let'   { TLet }
    'in'    { TIn }
    var     { TVar $$ }

%right '->'

%%

Term : Sort                             { Srt $1 }
     | '\\' var ':' Term '.' Term       { Lam $2 $4 $6 }
     | '(' var ':' Term ')' '->' Term   { Pi $2 $4 $7 }
     | Term '->' Term                   { Pi "_" $1 $3 }
     | Fact                             { $1 }
     | 'let' var '=' Term 'in' Term     { Let $2 $4 $6 }
     | 'let' var ':' Term 'in' Term     { Decl $2 $4 $6 }

Fact : Fact Atom                        { App $1 $2 }
     | Atom                             { $1 }

Atom : '(' Term ')'                     { $2 }
     | var                              { Var $1 }

Sort : '*'                              { Star }
     | '[]'                             { Box }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseTerm :: String -> Term
parseTerm = term . scanTokens
}