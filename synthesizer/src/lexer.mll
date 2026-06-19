{
  open Parser
}

rule token = parse
| "#" [^ '\n']*            { token lexbuf }      (* Ignore comments *)
| [ ' ' '\t' '\r' '\n' ]   { token lexbuf }      (* Skip whitespace *)
| "\"" [^ '"' ]* "\"" as s {
    STRING (String.sub s 1 (String.length s - 2))
  }
| "+"                      { ADD }
| "-"                      { SUB }
| "*"                      { MUL }
| "/"                      { DIV }
| "=="                     { EQ }
| ">="                     { GE }
| ">"                      { GT }
| "<="                     { LE }
| "<"                      { LT }
| "and"                    { AND }
| "or"                     { OR }
| "not"                    { NOT }
| "("                      { LPAREN }
| ")"                      { RPAREN }
| "["                      { LBRACKET }
| "]"                      { RBRACKET }
| ","                      { COMMA }
| ":"                      { COLON }
| "="                      { DECL }
| "True"                   { BOOL true }
| "False"                  { BOOL false }
| "bool"                   { BOOL_TYPE }
| "int"                    { INT_TYPE }
| "float"                  { FLOAT_TYPE }
| "str"                    { STRING_TYPE }
| "Optional"               { OPTION_TYPE }
| "List"                   { LIST_TYPE }
| "fold"                   { FOLD }
| "filter"                 { FILTER }
| "insert"                 { INSERT }
| "match"                  { MATCH }
| "case"                   { CASE }
| "None"                   { NULL }
| "lambda"                 { LAMBDA }
| "fix"                    { FIX }
| "if"                     { IF }
| "else"                   { ELSE }
| ['0'-'9']+ as digits     { INT (int_of_string digits) }
| ['0'-'9']+ "." ['0'-'9']* as digits
                           { FLOAT (float_of_string digits) }
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id
                           { IDENT id }
| eof                      { EOF }
| _                        { failwith ("Unexpected character: " ^ (Lexing.lexeme lexbuf)) }
