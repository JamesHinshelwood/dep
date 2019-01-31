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
    ','     { TComma }
    '×'     { TProduct }
    '.1'    { TFirst }
    '.2'    { TSecond }
    'inl'   { TInL }
    'inr'   { TInR }
    '+'     { TSum }
    '|'     { TBar }
    'case'  { TCase }
    'of'    { TOf }
    'let'   { TLet }
    'in'    { TIn }
    var     { TVar $$ }

%right '->'

%%

Term : Sort                                                         { Srt $1 }
     | '\\' var ':' Term '.' Term                                   { Lam $2 $4 $6 }
     | '(' var ':' Term ')' '->' Term                               { Pi $2 $4 $7 }
     | Term '->' Term                                               { Pi "_" $1 $3 }
     | '(' Term ',' Term ')'                                        { Pair $2 $4 }
     | '(' Term '×' Term ')'                                        { Product $2 $4 }
     | Term '.1'                                                    { First $1 }
     | Term '.2'                                                    { Second $1 }
     | 'inl' Term ':' Term                                          { InL $2 $4 }
     | 'inr' Term ':' Term                                          { InR $2 $4 }
     | '(' Term '+' Term ')'                                        { Sum $2 $4 }
     | 'case' Term 'of' 'inl' var '->' Term '|' 'inr' var '->' Term { Case $2 $5 $7 $10 $12 }
     | 'let' var '=' Term 'in' Term                                 { Let $2 $4 $6 }
     | 'let' var ':' Term 'in' Term                                 { Decl $2 $4 $6 }
     | Fact                                                         { $1 }

Fact : Fact Atom                                                    { App $1 $2 }
     | Atom                                                         { $1 }

Atom : '(' Term ')'                                                 { $2 }
     | var                                                          { Var $1 }

Sort : '*'                                                          { Star }
     | '[]'                                                         { Box }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseTerm :: String -> Term
parseTerm = term . scanTokens
}