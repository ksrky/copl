{
module Lexer
    ( Token (..)
    , alexScanTokens
    ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

@decimal = $digit+
@ident = $alpha [$alpha $digit \_ \']*
@int = \-? $digit+

tokens :-

$white+                     ;
\+                          { \_ -> Tplus }
\-                          { \_ -> Tminus }
\*                          { \_ -> Ttimes }
\<                          { \_ -> Tlt }
\(                          { \_ -> Tlparen }
\)                          { \_ -> Trparen }
if                          { \_ -> Tif }
then                        { \_ -> Tthen }
else                        { \_ -> Telse }
true                        { \_ -> Ttrue }
false                       { \_ -> Tfalse }
@ident                      { Ident }
@int                        { Number . read }

{
data Token
    = Tplus
    | Tminus
    | Ttimes
    | Tlt
    | Tlparen
    | Trparen
    | Tif
    | Tthen
    | Telse
    | Ttrue
    | Tfalse
    | Ident String
    | Number Int
    deriving (Eq, Show)
}