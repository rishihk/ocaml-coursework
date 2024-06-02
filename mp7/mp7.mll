{
open Common;;
}

(* Assign names to commonly-used regular expressions in this part of the code, 
   to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let startingComment = "(*"
let endingComment = "*)"
let hexadecimalValue =  "0x"(numeric|['a' - 'f'])+
let singleLineComment = "//"([^'\n']*)
let binaryValue =  "0b"['0'-'1']+
let floatingPointNumber = numeric+('.')numeric*('e'numeric+)?
let identifier = lowercase+(letter|[''']|numeric|['_'])*
let multiLineComment = startingComment([^'\n']*)endingComment

let identation = lowercase+(letter|[''']|numeric|['_'])*
let hex =  "0x"(numeric|['a' - 'f'])+

rule token = parse

   (* Whitespace handler *)

   | [' ' '\t' '\n'] { token lexbuf }
   | eof             { EOF } 

   (* Operators and symbols *)

   | '~'             { NEG }
   | '+'             { PLUS }
   | '-'             { MINUS }
   | '*'             { TIMES }
   | '/'             { DIV } 
   | "+."            { DPLUS }
   | "-."            { DMINUS }
   | "*."            { DTIMES }
   | "/."            { DDIV }
   | '^'             { CARAT}
   | '<'             { LT }
   | '>'             { GT }
   | "<="            { LEQ }
   | ">="            { GEQ }
   | '='             { EQUALS }
   | "<>"            { NEQ }

   (* Special characters and compound symbols *)

   | '|'             { PIPE }
   | "->"            { ARROW }
   | ';'             { SEMI }
   | ";;"            { DSEMI }
   | "::"            { DCOLON }
   | '@'             { AT }
   | "[]"            { NIL }

   (* Keywords for control structures and declarations *)

   | "let"          { LET }
   | "rec"          { REC }
   | "and"          { AND }
   | "end"          { END }
   | "in"           { IN }
   | "if"           { IF }
   | "then"         { THEN }
   | "else"         { ELSE }
   | "fun"          { FUN }
   | "mod"          { MOD }
   | "raise"        { RAISE }
   | "try"          { TRY } 
   | "with"         { WITH }

   (* Logical operators *)

   | "not"          { NOT }
   | "&&"           { LOGICALAND }
   | "||"           { LOGICALOR }

   (* Brackets, punctuation, and special values *)

   | '['            { LBRAC }
   | ']'            { RBRAC }
   | '('            { LPAREN }
   | ')'            { RPAREN }
   | ','            { COMMA }
   | '_'            { UNDERSCORE }
   | "true"         { TRUE }
   | "false"        { FALSE }
   | "()"           { UNIT }

   (* Numerical values and identifiers *)

   | floatingPointNumber as f   { FLOAT (float_of_string f) }
   | hexadecimalValue as h      { INT (int_of_string h) }
   | binaryValue as b           { INT (int_of_string b) }
   | numeric+ as i              { INT (int_of_string i) }
   | identifier as i            { IDENT i }

   (* Comments *)

   | singleLineComment          { token lexbuf}
   | startingComment [^'\n']* endingComment { token lexbuf}
   | "\""                       { parseString "" lexbuf}

   (* Strings *)

   and parseString str = parse
   | "\""            { STRING str }
   | "\\"            { parseEscaped str lexbuf }
   | _ as c          { parseString (str ^ (String.make 1 c) ) lexbuf }

   (* Handles escaped characters within strings *)

   and parseEscaped escStr = parse 
   | "\\"            { parseString (escStr ^ "\\") lexbuf }
   | "\'"            { parseString (escStr ^ "\'") lexbuf }
   | "\""            { parseString (escStr ^ "\"") lexbuf }
   | "t"             { parseString (escStr ^ "\t") lexbuf }
   | "n"             { parseString (escStr ^ "\n") lexbuf }
   | "r"             { parseString (escStr ^ "\r") lexbuf }
   | "b"             { parseString (escStr ^ "\b") lexbuf }
   | " "             { parseString (escStr ^ " ") lexbuf }
   | (['0' - '1']['0' - '9']['0' - '9'] as ch) { parseString (escStr ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
   | (['2']['0' - '4']['0' - '9'] as ch) { parseString (escStr ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
   | (['2']['5']['0' - '5'] as ch) { parseString (escStr ^ (String.make 1 (char_of_int (int_of_string ch)))) lexbuf } 
   | "\n"           { parseWhitespace escStr lexbuf }

   (* Manages whitespace within strings for proper parsing *)

   and parseWhitespace wsStr = parse
   [' ' '\t']       { parseWhitespace wsStr lexbuf }
   | "\""           { STRING wsStr }
   | "\\"           { parseEscaped wsStr lexbuf }
   | _ as c         { parseString (wsStr ^ (String.make 1 c)) lexbuf }

   


{(* do not modify this function: *)

 let lextest s = token (Lexing.from_string s)

   let get_all_tokens s =
      let buffer = Lexing.from_string (s ^ "\n") in
      let rec gatherTokens () =
         match token buffer with
         | EOF -> []
         | currentToken -> 
               let restTokens = gatherTokens () in
               currentToken :: restTokens
      in
      gatherTokens ()

   let try_get_all_tokens s =
      try 
         let tokensResult = get_all_tokens s in
         let successResult = Some (tokensResult), true in
         successResult
      with 
      | Failure "unmatched open comment" -> 
         let failureOpenComment = None, true in
         failureOpenComment
      | Failure "unmatched closed comment" -> 
         let failureClosedComment = None, false in
         failureClosedComment
 }

