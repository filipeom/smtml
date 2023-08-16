{
open Lexing
open Parser

exception SyntaxError of string

let keywords =
  let tbl = Hashtbl.create 180 in
  Array.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [| ("int" , INT_TYPE)
     ; ("real", REAL_TYPE)
     ; ("bool", BOOL_TYPE)
     ; ("str" , STR_TYPE)
     ; ("i32" , BV32_TYPE)
     ; ("i64" , BV64_TYPE)
     ; ("f32" , FP32_TYPE)
     ; ("f64" , FP64_TYPE)
     ; ("int.neg", INT_NEG)
     ; ("int.add", INT_ADD)
     ; ("int.sub", INT_SUB)
     ; ("int.div", INT_DIV)
     ; ("int.mul", INT_MUL)
     ; ("int.rem", INT_REM)
     ; ("int.eq", INT_EQ)
     ; ("int.ne", INT_NE)
     ; ("int.lt", INT_LT)
     ; ("int.le", INT_LE)
     ; ("int.gt", INT_GT)
     ; ("int.ge", INT_GE)
     (* ; ("int.to_string", CVTOP (Int ToString)) *)
     (* ; ("int.of_string", CVTOP (Int OfString)) *)
     (* ; ("int.reinterpret_real", CVTOP (Int ReinterpretReal)) *)
     ; ("real.neg", REAL_NEG)
     ; ("real.abs", REAL_ABS)
     ; ("real.sqrt", REAL_SQRT)
     ; ("real.ceil", REAL_CEIL)
     ; ("real.floor", REAL_FLOOR)
     ; ("real.add", REAL_ADD)
     ; ("real.sub", REAL_SUB)
     ; ("real.div", REAL_DIV)
     ; ("real.mul", REAL_MUL)
     ; ("real.min", REAL_MIN)
     ; ("real.max", REAL_MAX)
     ; ("real.eq", REAL_EQ)
     ; ("real.ne", REAL_NE)
     ; ("real.lt", REAL_LT)
     ; ("real.le", REAL_LE)
     ; ("real.gt", REAL_GT)
     ; ("real.ge", REAL_GE)
     (* ; ("real.reinterpret_int", CVTOP (Real ReinterpretInt)) *)
     (* ; ("real.to_string", CVTOP (Real ToString)) *)
     (* ; ("real.of_string", CVTOP (Real OfString)) *)
     ; ("bool.not", BOOL_NOT)
     ; ("bool.and", BOOL_AND)
     ; ("bool.or", BOOL_OR)
     ; ("bool.xor", BOOL_XOR)
     ; ("bool.eq", BOOL_EQ)
     ; ("bool.ne", BOOL_NE)
     ; ("bool.ite", BOOL_ITE)
     ; ("str.len", STR_LEN)
     ; ("str.trim", STR_TRIM)
     (* ; ("str.nth", STR_NTH) *)
     ; ("str.++", STR_CONCAT)
     ; ("str.sub", STR_SUBSTR)
     ; ("str.eq", STR_EQ)
     ; ("str.ne", STR_NE)
     ; ("i32.not", I32_NOT)
     ; ("i32.neg", I32_NEG)
     ; ("i32.add", I32_ADD)
     ; ("i32.sub", I32_SUB)
     ; ("i32.div_s", I32_DIV)
     ; ("i32.div_u", I32_DIVU)
     ; ("i32.and", I32_AND)
     ; ("i32.or", I32_OR)
     ; ("i32.xor", I32_XOR)
     ; ("i32.mul", I32_MUL)
     ; ("i32.shl", I32_SHL)
     ; ("i32.shr_s", I32_SHR)
     ; ("i32.shr_u", I32_SHRU)
     ; ("i32.rem_s", I32_REM)
     ; ("i32.rem_u", I32_REMU)
     ; ("i32.rotl", I32_ROTL)
     ; ("i32.rotr", I32_ROTR)
     ; ("i32.eq", I32_EQ)
     ; ("i32.ne", I32_NE)
     ; ("i32.lt_u", I32_LTU)
     ; ("i32.lt_s", I32_LT)
     ; ("i32.le_u", I32_LEU)
     ; ("i32.le_s", I32_LE)
     ; ("i32.gt_u", I32_GTU)
     ; ("i32.gt_s", I32_GT)
     ; ("i32.ge_u", I32_GEU)
     ; ("i32.ge_s", I32_GE)
     (* ; ("i32.to_bool", CVTOP (I32 ToBool)) *)
     (* ; ("i32.of_bool", CVTOP (I32 OfBool)) *)
     (* ; ("i32.trunc_f32_s",  CVTOP (I32 TruncSF32)) *)
     (* ; ("i32.trunc_f32_u", CVTOP (I32 TruncUF32)) *)
     (* ; ("i32.trunc_f64_s", CVTOP (I32 TruncSF64)) *)
     (* ; ("i32.trunc_f64_u", CVTOP (I32 TruncUF64)) *)
     (* ; ("i32.reinterpret_f32", CVTOP (I32 ReinterpretFloat)) *)
     (* ; ("i32.wrap_i64", CVTOP (I32 WrapI64)) *)
     ; ("i64.not", I64_NOT)
     ; ("i64.neg", I64_NEG)
     ; ("i64.add", I64_ADD)
     ; ("i64.sub", I64_SUB)
     ; ("i64.div_s", I64_DIV)
     ; ("i64.div_u", I64_DIVU)
     ; ("i64.and", I64_AND)
     ; ("i64.or", I64_OR)
     ; ("i64.xor", I64_XOR)
     ; ("i64.mul", I64_MUL)
     ; ("i64.shl", I64_SHL)
     ; ("i64.shr_s", I64_SHR)
     ; ("i64.shr_u", I64_SHRU)
     ; ("i64.rem_s", I64_REM)
     ; ("i64.rem_u", I64_REMU)
     ; ("i64.rotl", I64_ROTL)
     ; ("i64.rotr", I64_ROTR)
     ; ("i64.eq", I64_EQ)
     ; ("i64.ne", I64_NE)
     ; ("i64.lt_u", I64_LTU)
     ; ("i64.lt_s", I64_LT)
     ; ("i64.le_u", I64_LEU)
     ; ("i64.le_s", I64_LE)
     ; ("i64.gt_u", I64_GTU)
     ; ("i64.gt_s", I64_GT)
     ; ("i64.ge_u", I64_GEU)
     ; ("i64.ge_s", I64_GE)
     (* ; ("i64.trunc_f32_s",  CVTOP (I64 TruncSF32)) *)
     (* ; ("i64.trunc_f32_u", CVTOP (I64 TruncUF32)) *)
     (* ; ("i64.trunc_f64_s", CVTOP (I64 TruncSF64)) *)
     (* ; ("i64.trunc_f64_u", CVTOP (I64 TruncUF64)) *)
     (* ; ("i64.reinterpret_f64", CVTOP (I64 ReinterpretFloat)) *)
     (* ; ("i64.extend_i32_s", CVTOP (I64 ExtendSI32)) *)
     (* ; ("i64.extend_i32_u", CVTOP (I64 ExtendUI32)) *)
     (* ; ("f32.neg", UNARY (F32 Neg)) *)
     (* ; ("f32.abs", UNARY (F32 Abs)) *)
     (* ; ("f32.sqrt", UNARY (F32 Sqrt)) *)
     (* ; ("f32.nearest",UNARY (F32 Nearest) ) *)
     (* ; ("f32.is_nan", UNARY (F32 IsNan)) *)
     (* ; ("f32.add", BINARY (F32 Add)) *)
     (* ; ("f32.sub", BINARY (F32 Sub)) *)
     (* ; ("f32.mul", BINARY (F32 Mul)) *)
     (* ; ("f32.div", BINARY (F32 Div)) *)
     (* ; ("f32.min", BINARY (F32 Min)) *)
     (* ; ("f32.max", BINARY (F32 Max)) *)
     (* ; ("f32.rem", BINARY (F32 Rem)) *)
     (* ; ("f32.eq", RELOP (F32 Eq)) *)
     (* ; ("f32.ne", RELOP (F32 Ne)) *)
     (* ; ("f32.lt", RELOP (F32 Lt)) *)
     (* ; ("f32.le", RELOP (F32 Le)) *)
     (* ; ("f32.gt", RELOP (F32 Gt)) *)
     (* ; ("f32.ge", RELOP (F32 Ge)) *)
     (* ; ("f32.convert_i32_s", CVTOP (F32 ConvertSI32)) *)
     (* ; ("f32.convert_i32_u", CVTOP (F32 ConvertUI32)) *)
     (* ; ("f32.convert_i64_s", CVTOP (F32 ConvertSI32)) *)
     (* ; ("f32.demote_f64", CVTOP (F32 DemoteF64)) *)
     (* ; ("f32.reinterpret_i32", CVTOP (F32 ReinterpretInt)) *)
     (* ; ("f64.neg", UNARY (F64 Neg)) *)
     (* ; ("f64.abs", UNARY (F64 Abs)) *)
     (* ; ("f64.sqrt", UNARY (F64 Sqrt)) *)
     (* ; ("f64.nearest",UNARY (F64 Nearest) ) *)
     (* ; ("f64.is_nan", UNARY (F64 IsNan)) *)
     (* ; ("f64.add", BINARY (F64 Add)) *)
     (* ; ("f64.sub", BINARY (F64 Sub)) *)
     (* ; ("f64.mul", BINARY (F64 Mul)) *)
     (* ; ("f64.div", BINARY (F64 Div)) *)
     (* ; ("f64.min", BINARY (F64 Min)) *)
     (* ; ("f64.max", BINARY (F64 Max)) *)
     (* ; ("f64.rem", BINARY (F64 Rem)) *)
     (* ; ("f64.eq", RELOP (F64 Eq)) *)
     (* ; ("f64.ne", RELOP (F64 Ne)) *)
     (* ; ("f64.lt", RELOP (F64 Lt)) *)
     (* ; ("f64.le", RELOP (F64 Le)) *)
     (* ; ("f64.gt", RELOP (F64 Gt)) *)
     (* ; ("f64.ge", RELOP (F64 Ge)) *)
     (* ; ("f64.convert_i32_s", CVTOP (F64 ConvertSI32)) *)
     (* ; ("f64.convert_i32_u", CVTOP (F64 ConvertUI32)) *)
     (* ; ("f64.convert_i64_s", CVTOP (F64 ConvertSI32)) *)
     (* ; ("f64.promote_f32", CVTOP (F64 PromoteF32)) *)
     (* ; ("f64.reinterpret_i64", CVTOP (F64 ReinterpretInt)) *)
     ; ("assert", ASSERT)
     ; ("check-sat", CHECK_SAT)
     ; ("declare-fun", DECLARE_FUN)
     ; ("get-model", GET_MODEL)
    |];
  tbl

let error msg = raise (SyntaxError msg)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let character = ['a'-'z' 'A'-'Z']
let numeral = '0' | '-'? [ '1'-'9' ] digit*
let decimal = numeral '.' '0'* numeral
let hexadec = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary = "#b" ('0' | '1')+
let bool = "true" | "false"

let symbols = ['~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']
let symbol = (character | symbols) (character | digit | symbols)*
(* TODO: Quoted symbols: |symbol| *)

rule token = parse
  | '(' { LPAREN }
  | ')' { RPAREN }

  | numeral as s { NUM (Core.Int.of_string s) }
  | decimal as s { DEC (Core.Float.of_string s) }
  | bool as s { BOOL (Core.Bool.of_string s) }
  | hexadec { failwith "TODO: Lexer(hexadec)" }
  | binary { failwith "TODO: Lexer(binary)" }
  | '"' { string (Buffer.create 17) lexbuf }

  | symbol as x { try Hashtbl.find keywords x with Not_found -> SYMBOL x }

  | ';' { comment lexbuf }
  | white { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }

  | _ { error ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

and string buf = parse
  | '"' { STR (Buffer.contents buf) }
  | '"' '"' { Buffer.add_char buf '"'; string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | eof { error "nonterminated string" }
  | _ { error ("illegal string char: " ^ Lexing.lexeme lexbuf) }
