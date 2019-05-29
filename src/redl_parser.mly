%{

  open Redl_ast

  let node value =
    {
      value;
      loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ();
    }

%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENTIFIER STRING
%token LPAR RPAR LBRACE RBRACE COLON SEMI EQUAL
%token COMMAND
%token EOF

%type <Redl_ast.file> file
%start file
%%

file:
| COMMAND identifier parameters EQUAL statement file
  { Command_definition (node { name = $2; parameters = $3; body = $5 }) :: $6 }
| statement file
  { Statement $1 :: $2 }
| EOF
  { [] }

identifier:
| IDENTIFIER
  { node $1 }

parameters:
| LPAR identifier COLON identifier RPAR parameters
  { ($2, $4) :: $6 }
|
  { [] }

statement:
| SEMI LBRACE statements RBRACE
  { $3 }
| identifier arguments SEMI
  { node (Command ($1, $2)) }

statements:
| statement statements
  { node (Sequence ($1, $2)) }
| statement
  { $1 }

arguments:
| simple_expression arguments
  { $1 :: $2 }
|
  { [] }

simple_expression:
| INT
  { node (Int $1) }
| FLOAT
  { node (Float $1) }
| STRING
  { node (String $1) }
| IDENTIFIER
  { node (Variable $1) }
| LPAR simple_expression arguments RPAR
  { node (Apply ($2, $3)) }
