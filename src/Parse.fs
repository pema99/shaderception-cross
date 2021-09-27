module Parse

#nowarn "40"
open System
open System.Globalization
open Combinator
open Util

// === AST ===
type Operator =
  | Plus    | PlusEq
  | Minus   | MinusEq
  | Star    | StarEq
  | Slash   | SlashEq
  | Not     | NotEq
  | Greater | GreaterEq
  | Less    | LessEq
  | PlusPlus
  | MinusMinus
  | Equal
  | And
  | Or

type Block = Stmt list

and Stmt =
  | Scope of Block
  | Let of string * string option * Expr
  | If of Expr * Stmt * Stmt option
  | While of Expr * Stmt
  | For of Stmt * Expr * Stmt * Stmt
  | Fun of string * string list * Stmt
  | Return of Expr

and Expr = 
  | Literal of float
  | Var of string
  | Swizzle of Expr * string
  | BinOp of Expr * Operator * Expr
  | UnOp of Operator * Expr
  | Call of string * Expr list

// === Helpers ===
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let isNumeric c = (c >= '0' && c <= '9')
let isAlphaNumeric c = isAlpha c || isNumeric c || (c = '_')
let mkString = List.toArray >> String
let whitespaceP = many (oneOf [' '; '\r'; '\n'; '\t']) *> just ()
let whitespacedP p = between whitespaceP p whitespaceP
let parens p = between (one '(') p (one ')')

// === Operator parsing ===
let operatorP = com {
  let! l = item
  let! r = look
  match l, r with
  | '+', '+' -> return! item *> just PlusPlus
  | '+', '=' -> return! item *> just PlusEq
  | '-', '-' -> return! item *> just MinusMinus
  | '-', '=' -> return! item *> just MinusEq
  | '*', '=' -> return! item *> just StarEq
  | '/', '=' -> return! item *> just SlashEq
  | '!', '=' -> return! item *> just NotEq
  | '>', '=' -> return! item *> just GreaterEq
  | '<', '=' -> return! item *> just LessEq
  | '=', '=' -> return! item *> just Equal
  | '&', '&' -> return! item *> just And
  | '|', '|' -> return! item *> just Or
  | '+', _ -> return Plus
  | '-', _ -> return Minus
  | '*', _ -> return Star
  | '/', _ -> return Slash
  | '!', _ -> return Not
  | '>', _ -> return Greater
  | '<', _ -> return Less
  | _ -> return! fail()
}
let specificOperatorP op =
  guard ((=) op) operatorP
  |> attempt
  |> whitespacedP

// === Identifier parsing ===
let identP = 
  eatWhile1 isAlphaNumeric
  |>> mkString
  |> whitespacedP
let keywordP target = 
  guard ((=) target) identP
  |> attempt

// === Expressions and statements ===
let exprP, exprPImpl = declParser()
let stmtP, stmtPImpl = declParser()

let groupP = parens exprP
let blockP =
  many stmtP
  <* (guard ((=) '}') look |> delete <|> eof)
let scopeP = 
  between (one '{') blockP (one '}')
  |> whitespacedP
  |>> Scope

// === Expression parsing ===
let swizzleP = 
  one '.' *> eatWhile1 (fun x -> Seq.contains x "xyzwrgba")
  |>> List.toArray |>> String
let swizzledP p = 
  attempt (p 
  <+> swizzleP
  |>> fun (e, i) -> Swizzle (e, i))
  <|> p
let varP = swizzledP (identP |>> Var)
let callP = 
  identP
  <+> parens (sepBy exprP (one ','))
  |>> Call
let literalP = 
  eatWhile (fun x -> isNumeric x || x = '.')
  |>> mkString
  >>= fun s -> let (succ, num) =
                 Double.TryParse (s, NumberStyles.Any, CultureInfo.InvariantCulture)
               if succ then num |> Literal |> just
               else fail()

let unOpP = 
  (specificOperatorP Plus <|> specificOperatorP Minus <|> specificOperatorP Not)
  <+> exprP // TODO: technically should be term
  |>> UnOp

let specificBinOpP op =
  specificOperatorP op
  *> just (curry <| fun (l, r) -> BinOp (l, op, r))
let chooseBinOpP = List.map (specificBinOpP) >> choice

let termP = swizzledP (groupP <|> attempt callP <|> literalP <|> varP <|> unOpP)
let mulDivP = chainL1 termP (chooseBinOpP [Star; Slash])
let addSubP = chainL1 mulDivP (chooseBinOpP [Plus; Minus])
let comparisonP = chainL1 addSubP (chooseBinOpP [GreaterEq; LessEq; Greater; Less; NotEq; Equal])
let boolOpP = chainL1 comparisonP (chooseBinOpP [And; Or])
exprPImpl := whitespacedP boolOpP

// === Statement parsing ===
let mathAssignP op impl ident swizzle =
  specificOperatorP op *> exprP
  |>> fun e ->
    match swizzle with
    | Some v -> Let (ident, swizzle, BinOp (Swizzle (Var ident, v), impl, e))
    | None -> Let (ident, None, BinOp (Var ident, impl, e))
let incrAssignP op impl ident swizzle =
  specificOperatorP op *> (
    match swizzle with
    | Some v -> Let (ident, None, BinOp (Swizzle(Var ident, v), impl, Literal 1.0)) 
    | None -> Let (ident, None, BinOp (Var ident, impl, Literal 1.0)) 
    |> just)
let assignmentContP (ident, swizzle) =
      incrAssignP PlusPlus Plus ident swizzle
  <|> incrAssignP MinusMinus Minus ident swizzle
  <|> mathAssignP PlusEq Plus ident swizzle
  <|> mathAssignP MinusEq Minus ident swizzle
  <|> mathAssignP StarEq Star ident swizzle
  <|> mathAssignP SlashEq Slash ident swizzle
  <|> (one ('=') *> exprP |>> fun e -> Let (ident, swizzle, e))
let assignmentP =
  opt (keywordP "let" <|> keywordP "set") *> identP
  <+> opt swizzleP
  >>= assignmentContP

let returnP = 
  attempt (keywordP "return" *> exprP <* one ';')
  <|> exprP 
  |>> Return
let whileP =
  keywordP "while" *> parens exprP
  <+> scopeP
  |>> While
let funP =
  keywordP "fun" *> identP
  <+> parens (sepBy identP (one ','))
  <+> scopeP
  |>> fun ((name, parms), body) -> Fun (name, parms, body)
let forP =
  keywordP "for" *> parens
    (assignmentP <* one ';'
    <+> exprP <* one ';'
    <+> assignmentP)
  <+> scopeP
  |>> fun (((a, b), c), d) -> For (a, b, c, d)
let ifP, ifPImpl = declParser()
ifPImpl :=
  keywordP "if" *> parens exprP
  <+> scopeP
  <+> opt (keywordP "else" *> scopeP <|> ifP)
  |>> fun ((cond, body), alt) -> If (cond, body, alt)

stmtPImpl :=
  (funP <|> ifP <|> whileP <|> forP <|> attempt (assignmentP <* one ';') <|> returnP)
  |> whitespacedP