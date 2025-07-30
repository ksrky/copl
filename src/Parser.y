{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token

'+'             { Tplus }
'-'             { Tminus }   
'*'             { Ttimes }
'<'             { Tlt }
'='             { Tequal }
'('             { Tlparen }
')'             { Trparen }
'['             { Tlbracket }
']'             { Trbracket }
','             { Tcomma }
'|-'            { Tvdash }
'->'            { Tarrow }
'::'            { Tcoloncolon }
'|'             { Tpipe }
'if'            { Tif }
'then'          { Tthen }
'else'          { Telse }
'true'          { Ttrue }
'false'         { Tfalse }
'let'           { Tlet }
'in'            { Tin }
'rec'           { Trec }
'fun'           { Tfun }
'match'         { Tmatch }
'with'          { Twith }

ident           { Ident $$ }
num             { Number $$ }

%nonassoc '<'
%right '::'
%left '+' '-'
%left '*'

%%

Query :: { (Env, Exp) }
    : Env '|-' Exp                          { ($1, $3) }

Env :: { Env }
    : {- empty -}                           { Empty }
    | Env1                                  { $1 }

Env1 :: { Env }
    : ident '=' Val                         { Snoc Empty $1 $3 }
    | Env1 ',' ident '=' Val                { Snoc $1 $3 $5 }

Val :: { Val }
    : num                                   { VInt $1 }
    | 'true'                                { VBool True }
    | 'false'                               { VBool False }
    | '(' Env ')' '[' 'fun' ident '=' Exp ']'
                                            { VClos $2 $6 $8 }
    | '(' Env ')' '[' 'rec' ident '=' 'fun' ident '=' Exp ']'
                                            { VFix $2 $6 $9 $11 }

Exp :: { Exp }
    : Exp '+' Exp                           { Add $1 $3 }
    | Exp '-' Exp                           { Sub $1 $3 }
    | Exp '*' Exp                           { Mul $1 $3 }
    | Exp '<' Exp                           { Less $1 $3 }
    | Exp '::' Exp                          { Cons $1 $3 }
    | 'if' Exp 'then' Exp 'else' Exp        { Ite $2 $4 $6 }
    | 'let' ident '=' Exp 'in' Exp          { Let $2 $4 $6 }
    | 'fun' ident '->' Exp                  { Fun $2 $4 }
    | 'let' 'rec' ident '=' 'fun' ident '->' Exp 'in' Exp
                                            { Letrec $3 $6 $8 $10 }
    | 'match' Exp 'with' '[' ']' '->' Exp '|' ident '::' ident '->' Exp
                                            { Match $2 $7 $9 $11 $13 }
    | Exp2                                  { $1 }

Exp2 :: { Exp }
    : Exp2 Exp1                             { App $1 $2 }
    | Exp1                                  { $1 }

Exp1 :: { Exp }
    : num                                   { I $1 }
    | 'true'                                { B True }
    | 'false'                               { B False }
    | ident                                 { V $1 }
    | '[' ']'                               { Nil }
    | '(' Exp ')'                           { $2 }
{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
}