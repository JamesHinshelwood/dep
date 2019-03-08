{
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Lexer (
    scanTokens,
    Token(..)
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white+                                 ;
    "--".                                   ;
    "Type"                                  { \s -> TType }
    \:                                      { \s -> TColon }
    \\                                      { \s -> TLambda }
    \.                                      { \s -> TDot }
    \(                                      { \s -> TLParen }
    \)                                      { \s -> TRParen }
    \[                                      { \s -> TLBracket }
    \]                                      { \s -> TRBracket }
    "->"                                    { \s -> TArrow }
    \=                                      { \s -> TEquals }
    \,                                      { \s -> TComma }
    "*"                                     { \s -> TProduct }
    ".1"                                    { \s -> TFirst }
    ".2"                                    { \s -> TSecond }
    \+                                      { \s -> TSum }
    \|                                      { \s -> TBar }
    "case"                                  { \s -> TCase }
    "of"                                    { \s -> TOf }
    "let"                                   { \s -> TLet }
    "in"                                    { \s -> TIn }
    "unit"                                  { \s -> TUnit }
    "Unit"                                  { \s -> TUnitUpper }
    "refl"                                  { \s -> TRefl }
    [$alpha \_] [$alpha $digit \_ \']*      { \s -> TVar s }

{
data Token = TType
           | TColon
           | TLambda
           | TDot
           | TLParen
           | TRParen
           | TLBracket
           | TRBracket
           | TArrow
           | TEquals
           | TComma
           | TProduct
           | TFirst
           | TSecond
           | TSum
           | TBar
           | TCase
           | TOf
           | TLet
           | TIn
           | TUnit
           | TUnitUpper
           | TRefl
           | TVar String
           deriving (Eq, Show)

scanTokens = alexScanTokens
}