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
\=                          { \_ -> Tequal }
\(                          { \_ -> Tlparen }
\)                          { \_ -> Trparen }
\[                          { \_ -> Tlbracket }
\]                          { \_ -> Trbracket }
\,                          { \_ -> Tcomma }
\|\-                        { \_ -> Tvdash}
\-\>                        { \_ -> Tarrow }
\:\:                        { \_ -> Tcoloncolon }
\|                          { \_ -> Tpipe }
\:                          { \_ -> Tcolon }
if                          { \_ -> Tif }
then                        { \_ -> Tthen }
else                        { \_ -> Telse }
let                         { \_ -> Tlet }
in                          { \_ -> Tin }
rec                         { \_ -> Trec }
fun                         { \_ -> Tfun }
match                       { \_ -> Tmatch }
with                        { \_ -> Twith }
true                        { \_ -> Ttrue }
false                       { \_ -> Tfalse }
int                         { \_ -> Tint }
bool                        { \_ -> Tbool }
list                        { \_ -> Tlist }
@ident                      { Ident }
@int                        { Number . read }

{
data Token
    = Tplus
    | Tminus
    | Ttimes
    | Tlt
    | Tequal
    | Tlparen
    | Trparen
    | Tlbracket
    | Trbracket
    | Tcomma
    | Tvdash
    | Tarrow
    | Tcoloncolon
    | Tpipe
    | Tcolon
    | Tif
    | Tthen
    | Telse
    | Tlet
    | Tin
    | Trec
    | Tfun
    | Tmatch
    | Twith
    | Ttrue
    | Tfalse
    | Tint
    | Tbool
    | Tlist
    | Ident String
    | Number Int
    deriving (Eq, Show)
}