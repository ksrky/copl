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
'.'             { Tdot }
'|-'            { Tvdash }
'->'            { Tarrow }
'::'            { Tcoloncolon }
'|'             { Tpipe }
':'             { Tcolon }
'\''            { Tapost }
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
'int'           { Tint }
'bool'          { Tbool }
'list'          { Tlist }

ident           { Ident $$ }
num             { Number $$ }

%right '->'
%nonassoc '<'
%right '::'
%left '+' '-'
%left '*'

%%

Query :: { (Env, Exp, Typ) }
    : Env '|-' Exp ':' Typ                  { ($1, $3, $5) }

Env :: { Env }
    : {- empty -}                           { Empty }
    | Env1                                  { $1 }

Env1 :: { Env }
    : ident ':' TypeScheme                  { Snoc Empty $1 $3 }
    | Env1 ',' ident ':' TypeScheme         { Snoc $1 $3 $5 }

TypeScheme :: { TypeScheme }
    : Typ                                   { Forall [] $1 }
    | TypeVars '.' Typ                      { Forall $1 $3 }

TypeVars :: { [String] }
    : '\'' ident                            { [$2] }
    | TypeVars '\'' ident                   { $1 ++ [$3] }

Typ :: { Typ }
    : 'int'                                  { TInt }
    | 'bool'                                 { TBool }
    | '\'' ident                             { TNamed $2 }
    | Typ '->' Typ                           { TFun $1 $3 }
    | Typ 'list'                             { TList $1 }
    | '(' Typ ')'                            { $2 }

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