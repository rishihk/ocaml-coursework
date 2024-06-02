%{ open Common %}

%token <int> INT
%token <float> FLOAT
%token <string> STRING IDENT
%token TRUE FALSE NEG PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV MOD EXP CARAT
       LT GT LEQ GEQ EQUALS NEQ PIPE ARROW SEMI DSEMI DCOLON AT NIL
       LET REC AND IN IF THEN ELSE FUN MOD RAISE TRY WITH NOT LOGICALAND
       LOGICALOR LBRAC RBRAC LPAREN RPAREN COMMA UNDERSCORE UNIT
       HEAD TAIL PRINT FST SND EOF

%start main
%type <Common.dec> main

%%

main:
  | expression DSEMI                              { (Anon ($1)) }
  | LET IDENT EQUALS expression DSEMI             { (Let ($2, $4)) }
  | LET REC IDENT IDENT EQUALS expression DSEMI   { (LetRec ($3, $4, $6)) }

constants:
  | TRUE                                    { BoolConst true }
  | FALSE                                   { BoolConst false }
  | INT                                     { IntConst $1 }
  | FLOAT                                   { FloatConst $1 }
  | STRING                                  { StringConst $1 }
  | NIL                                     { NilConst }
  | UNIT                                    { UnitConst }

parentheses:
  | LPAREN RPAREN                           { ConstExp UnitConst }
  | LPAREN expression RPAREN                { $2 }

tupleExp:
  | LPAREN expression COMMA tuplesList RPAREN   { BinOpAppExp (CommaOp, $2, $4) }

tuplesList:
  | expression                                  { $1 }
  | expression COMMA tuplesList                 { BinOpAppExp (CommaOp, $1, $3) }

unaryOps:
  | HEAD                                    { HdOp }
  | TAIL                                    { TlOp }
  | PRINT                                   { PrintOp }
  | NEG                                     { IntNegOp }
  | FST                                     { FstOp }
  | SND                                     { SndOp }

comparatorOps:
  | comparatorOps EQUALS consing            { BinOpAppExp (EqOp,$1,$3) }
  | comparatorOps NEQ consing               { IfExp(BinOpAppExp (EqOp,$1,$3), ConstExp (BoolConst false), ConstExp (BoolConst true)) }
  | comparatorOps GT consing                { BinOpAppExp (GreaterOp,$1,$3) }
  | comparatorOps LT consing                { BinOpAppExp(GreaterOp,$3,$1) }
  | comparatorOps GEQ consing               { IfExp(BinOpAppExp(GreaterOp,$1,$3), BinOpAppExp(EqOp,$1,$3), ConstExp (BoolConst true)) }
  | comparatorOps LEQ consing               { IfExp(BinOpAppExp(GreaterOp,$3,$1), BinOpAppExp(EqOp,$1,$3), ConstExp (BoolConst true)) }
  | consing                                 { $1 }

plus:
  | PLUS                                  { IntPlusOp }
  | DPLUS                                 { FloatPlusOp }
  | CARAT                                 { ConcatOp }  

minus:
  | MINUS                                 { IntMinusOp }
  | DMINUS                                { FloatMinusOp }

times:
  | TIMES                                 { IntTimesOp }
  | DTIMES                                { FloatTimesOp }

div:
  | DIV                                   { IntDivOp }
  | DDIV                                  { FloatDivOp }
  | MOD                                   { ModOp }  

addSub:
  | addSub plus multDiv    { BinOpAppExp($2, $1, $3) }
  | addSub minus multDiv   { BinOpAppExp($2, $1, $3) }
  | multDiv                { $1 }

multDiv:
  | multDiv times baseExp           { BinOpAppExp($2, $1, $3) }
  | multDiv div baseExp             { BinOpAppExp($2, $1, $3) }
  | baseExp                         { $1 }

consing:
  | addSub DCOLON consing            { BinOpAppExp(ConsOp,$1,$3) }
  | addSub                           { $1 }

letIn:
  | LET IDENT EQUALS expression IN expression            { LetInExp($2, $4, $6) }

letRec:
  | LET REC IDENT IDENT EQUALS expression IN expression  { LetRecInExp($3, $4, $6, $8) }

arrowFn:
  | FUN IDENT ARROW expression                           { FunExp($2, $4) }

ifThenElse:
  | IF expression THEN expression ELSE expression        { IfExp($2, $4, $6) }

fnApp:
  | atomic_expression       { $1 }
  | fnApp fnDefn            { AppExp($1,$2) }

fnDefn:
  | controlStructures              { $1 }
  | fnApp                          { $1 }
  | raise                          { $1 }

orOp:
  | orOp LOGICALOR andOp           { IfExp($1, ConstExp (BoolConst true), $3) }
  | andOp                          { $1 }

andOp:
  | andOp LOGICALAND comparatorOps         { IfExp($1, $3, ConstExp (BoolConst false)) }
  | comparatorOps                          { $1 }

raise:
  | unaryOps fnDefn           { MonOpAppExp($1,$2) }
  | RAISE fnDefn              { RaiseExp $2 }

listStart:
  | LBRAC listContent      { $2 }

listContent:
  | expression listTerminate  { BinOpAppExp(ConsOp,$1,$2) }

listEnd:
  | RBRAC                  { ConstExp NilConst }
  | listContent            { $1 }

listTerminate:
  | RBRAC                  { ConstExp NilConst }
  | SEMI listEnd           { $2 }

expression:
  | orOp                               { $1 }

baseExp:
  | fnApp EXP baseExp              { BinOpAppExp (ExpoOp,$1,$3) }
  | raise EXP baseExp              { BinOpAppExp (ExpoOp,$1,$3) }
  | fnDefn                         { $1 }

controlStructures:
  | letRec                  { $1 }
  | letIn                   { $1 }
  | arrowFn                 { $1 }
  | ifThenElse              { $1 }

pat:
  | UNDERSCORE               { None }
  | INT                      { Some $1 }

atomic_expression:
  | IDENT                       { VarExp $1 }
  | constants                   { ConstExp $1 }
  | parentheses                 { $1 }
  | tupleExp                    { $1 }
  | unaryOps atomic_expression  { MonOpAppExp ($1,$2) }
  | listStart                   { $1 }