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
    'Type'  { TType }
    ':'     { TColon }
    '\\'    { TLambda }
    '.'     { TDot }
    '('     { TLParen }
    ')'     { TRParen }
    '['     { TLBracket }
    ']'     { TRBracket }
    '->'    { TArrow }
    '='     { TEquals }
    ','     { TComma }
    '*'     { TProduct }
    '.1'    { TFirst }
    '.2'    { TSecond }
    '+'     { TSum }
    '|'     { TBar }
    'case'  { TCase }
    'of'    { TOf }
    'let'   { TLet }
    'in'    { TIn }
    'unit'  { TUnit }
    'Unit'  { TUnitUpper }
    'refl'  { TRefl }
    var     { TVar $$ }

%right '->'

%%

Term : 'Type'                                                       { Type }
     | '\\' var ':' Term '.' Term                                   { lam $2 $4 $6 }
     | '(' var ':' Term ')' '->' Term                               { pi_ $2 $4 $7 }
     | Term '->' Term                                               { pi_ "_" $1 $3 }
     | '(' Term ',' Term ':' Term ')'                               { TypedPair $2 $4 $6 }
     | '(' var ':' Term '*' Term ')'                                { sigma $2 $4 $6 }
     | '(' Term '*' Term ')'                                        { sigma "_" $2 $4 }
     | Term '.1'                                                    { First $1 }
     | Term '.2'                                                    { Second $1 }
     | '(' var '(' Term ')' ':' Term ')'                            { Variant $2 $4 $7 }
     | '(' Sum ')'                                                  { Sum $2 }
     | 'case' Term 'of' Case                                        { case_ $2 $4 }
     | 'let' var '=' Term 'in' Term                                 { let_ $2 $4 $6 }
     | 'let' var ':' Term 'in' Term                                 { decl $2 $4 $6 }
     | 'unit'                                                       { Unit }
     | 'Unit'                                                       { UnitTy }
     | '(' Term '=' Term ':' Term ')'                               { Eq $2 $4 $6 }
     | '(' 'refl' ':' Term ')'                                      { Refl $4 }
     | 'case' '[' Term ']' Term 'of' 'refl' '(' var ')' '->' Term   { split $3 $5 $9 $12 }
     | Appl                                                         { $1 }

Sum  : var '(' Term ')'                                             { [($1, $3)] }
     | var '(' Term ')' '+' Sum                                     { ($1, $3):$6 }

Case : var '(' var ')' '->' Term                                    { [($1, $3, $6)] }
     | var '(' var ')' '->' Term '|' Case                           { ($1, $3, $6):$8 }

Appl : Appl Atom                                                    { App $1 $2 }
     | Atom                                                         { $1 }

Atom : '(' Term ')'                                                 { $2 }
     | var                                                          { var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseTerm :: String -> Term
parseTerm = term . scanTokens
}