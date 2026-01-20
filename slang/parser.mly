%{
(* Auxiliary code *)

let get_loc () = Past.Loc.of_position (Parsing.symbol_start_pos ())
%}

(* ======== Tokens and types ======== *)

%token<string> IDENT (* Identifiers *)
%token LPAREN RPAREN
%token COLON (* Type annotation *)

%token NOT (* Boolean negation *)
%token ADD SUB MUL DIV (* Arithmetic operators; + and * also used for types *)
%token EQUAL LT AND OR (* Comparison and logical operators *)

%token IF THEN ELSE
%token BEGIN SEMICOLON END (* Sequential composition *)
%token LET REC IN
%token WHILE DO (* while, do *)
%token FUN (* Lambda expressions *)

%token COMMA FST SND (* Pair constructor and destructors *)
%token INL INR CASE OF BAR (* Sum constructor and destructors *)
%token REF BANG ASSIGN (* Reference, dereference, assignment *)

%token BOOL INTTYPE UNITTYPE ARROW (* Boolean type, integer type, unit type, arrow type *)

%token TRUE FALSE (* Boolean constants *)
%token UNIT (* Unit value *)
%token WHAT (* Read integer from input *)
%token<int> INT (* Integers *)

%token EOF

(* ======== Precedence ======== *)

(* Lowest to highest precedence *)

%left ASSIGN
%right OR
%right AND
%left EQUAL LT
%left ADD SUB
%left MUL DIV
%right ARROW

(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc UNIT INT WHAT IDENT TRUE FALSE LPAREN NOT BANG REF

%start start
%type <Past.Type.t> type_expr
%type <Past.t> simple_expr
%type <Past.t> app_expr
%type <Past.t> op_expr
%type <Past.t> expr
%type <Past.t list> expr_list
%type <Past.t> start

%%

(* ======== Grammar ======== *)

start:
| expr EOF { $1 }

(* Problem
   -e  (unary minus)
    e e (application)
    e1 - e2  (e1 (-e2) or e1 - e2?)
*)

simple_expr:
| LPAREN expr RPAREN                 { $2 }
| LPAREN expr COMMA expr RPAREN      { { loc = get_loc (); expr = Pair ($2, $4) } }
| UNIT                               { { loc = get_loc (); expr = Unit } }
| INT                                { { loc = get_loc (); expr = Integer $1 } }
| WHAT                               { { loc = get_loc (); expr = What } }
| IDENT                              { { loc = get_loc (); expr = Var $1 } }
| TRUE                               { { loc = get_loc (); expr = Boolean true } }
| FALSE                              { { loc = get_loc (); expr = Boolean false } }
| NOT simple_expr                    { { loc = get_loc (); expr = UnaryOp (Not, $2) } }
| BANG simple_expr                   { { loc = get_loc (); expr = Deref $2 } }

(* These have binding power like function application *)
app_expr:
| simple_expr                        { $1 }
| app_expr simple_expr               { { loc = get_loc (); expr = App ($1, $2) } }
| FST simple_expr                    { { loc = get_loc (); expr = Fst $2 } }
| SND simple_expr                    { { loc = get_loc (); expr = Snd $2 } }
| REF simple_expr                    { { loc = get_loc (); expr = Ref $2 } }
| INL type_expr simple_expr          { { loc = get_loc (); expr = Inl ($2, $3) } }
| INR type_expr simple_expr          { { loc = get_loc (); expr = Inr ($2, $3) } }

(*
    op_expr binds more tightly than expr, so
    (fun (x : int) -> x + x) 2
    parses as: (fun (x : int) -> (x + x)) 2
    NOT: ((fun (x : int) -> x) + x) 2
*)
op_expr:
| app_expr                           { $1 }
| SUB op_expr %prec UNIT             { { loc = get_loc (); expr = UnaryOp (Neg, $2) } }
| op_expr MUL op_expr                { { loc = get_loc (); expr = BinaryOp ($1, Mul, $3) } }
| op_expr DIV op_expr                { { loc = get_loc (); expr = BinaryOp ($1, Div, $3) } }
| op_expr ADD op_expr                { { loc = get_loc (); expr = BinaryOp ($1, Add, $3) } }
| op_expr SUB op_expr                { { loc = get_loc (); expr = BinaryOp ($1, Sub, $3) } }
| op_expr LT op_expr                 { { loc = get_loc (); expr = BinaryOp ($1, Lt, $3) } }
| op_expr EQUAL op_expr              { { loc = get_loc (); expr = BinaryOp ($1, Eq, $3) } }
| op_expr AND op_expr                { { loc = get_loc (); expr = BinaryOp ($1, And, $3) } }
| op_expr OR op_expr                 { { loc = get_loc (); expr = BinaryOp ($1, Or, $3) } }
| op_expr ASSIGN op_expr             { { loc = get_loc (); expr = Assign ($1, $3) } }

expr:
| op_expr                            { $1 }
| BEGIN expr_list END                { { loc = get_loc (); expr = Seq $2 } }
| IF expr THEN expr ELSE expr        { { loc = get_loc (); expr = If ($2, $4, $6) } }
| WHILE expr DO expr                 { { loc = get_loc (); expr = While ($2, $4) } }
| FUN LPAREN IDENT COLON type_expr RPAREN ARROW expr
                                     { { loc = get_loc (); expr = Lambda ($3, $5, $8) } }
| LET IDENT COLON type_expr EQUAL expr IN expr
                                     { { loc = get_loc (); expr = Let ($2, $4, $6, $8) } }
| LET IDENT LPAREN IDENT COLON type_expr RPAREN COLON type_expr EQUAL expr IN expr
                                     { { loc = get_loc (); expr = LetFun ($2, ($4, $6, $11), $9, $13) } }
| CASE expr OF
  BAR INL LPAREN IDENT COLON type_expr RPAREN ARROW expr
  BAR INR LPAREN IDENT COLON type_expr RPAREN ARROW expr
                                     { { loc = get_loc (); expr = Case ($2, ($7, $9, $12), ($16, $18, $21)) } }

expr_list:
|   expr                             { [$1] }
|   expr SEMICOLON expr_list         { $1 :: $3 }


type_expr:
| BOOL                               { Bool  }
| INTTYPE                            { Int  }
| UNITTYPE                           { Unit  }
| type_expr ARROW type_expr          { Arrow ($1, $3)}
| type_expr MUL type_expr            { Product ($1, $3)}
| type_expr ADD type_expr            { Sum ($1, $3)}
| type_expr REF                      { Ref $1 }
| LPAREN type_expr RPAREN            { $2 }
