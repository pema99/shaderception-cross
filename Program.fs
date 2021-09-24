#nowarn "40"

open System
open System.Globalization
open Combinator
open Common
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
  | Let of string * Expr
  | If of Expr * Stmt * Stmt option
  | While of Expr * Stmt
  | For of Stmt * Expr * Stmt * Stmt
  | Fun of string * string list * Stmt
  | Return of Expr

and Expr = 
  | Literal of float
  | Var of string
  | BinOp of Expr * Operator * Expr
  | UnOp of Operator * Expr
  | Call of string * Expr list

// === Helpers ===
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let isNumeric c = (c >= '0' && c <= '9')
let isAlphaNumeric c = isAlpha c || isNumeric c || (c = '.') || (c = '_')
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
let varP = identP |>> Var
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

let termP = groupP <|> attempt callP <|> literalP <|> varP <|> unOpP
let mulDivP = chainL1 termP (chooseBinOpP [Star; Slash])
let addSubP = chainL1 mulDivP (chooseBinOpP [Plus; Minus])
let comparisonP = chainL1 addSubP (chooseBinOpP [GreaterEq; LessEq; Greater; Less; NotEq; Equal])
let boolOpP = chainL1 comparisonP (chooseBinOpP [And; Or])
exprPImpl := whitespacedP boolOpP

// === Statement parsing ===
let mathAssignP op impl ident =
  specificOperatorP op *> exprP
  |>> fun e -> Let (ident, BinOp (Var ident, impl, e))
let incrAssignP op impl ident =
  specificOperatorP op *> 
  (just <| Let (ident, BinOp (Var ident, impl, Literal 1.0)))
let assignmentContP ident =
      incrAssignP PlusPlus Plus ident
  <|> incrAssignP MinusMinus Minus ident
  <|> mathAssignP PlusEq Plus ident
  <|> mathAssignP MinusEq Minus ident
  <|> mathAssignP StarEq Star ident
  <|> mathAssignP SlashEq Slash ident
  <|> (one ('=') *> exprP |>> fun e -> Let (ident, e))
let assignmentP =
  opt (keywordP "let" <|> keywordP "set") *> identP 
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

// TODO: swizzles
stmtPImpl :=
  (funP <|> ifP <|> whileP <|> forP <|> attempt (assignmentP <* one ';') <|> returnP)
  |> whitespacedP

let dataSrc = 
  //System.IO.File.ReadAllText("test.psl")
  "fun smin(d1, d2, k)
{
    let h = clamp(0.5 + 0.5 * (d2 - d1) / k, 0.0, 1.0);
    lerp(d2, d1, h) - k * h * (1.0 - h)
}

fun map(p)
{
    let d1 = length(p) - 0.3;
    let d2 = length(p + float3(0.2, sin(time())*0.3, 0)) - 0.3;
    smin(d1, d2, 0.05)
}

fun march(ro, rd)
{
    let t = 0;
    let i = 0;
    while (i < 15)
    {
        let dist = map(ro + t * rd);
        let t = t + dist;
        let i = i + 1;
    }
    t
}

let p = 2.0 * (uv() - 0.5);
let ro = float3(0.0, 0.0, -1.0);
let rd = normalize(float3(p, p, 1));

let d = march(ro, rd);
if (d < 1)
{
    let hit = ro + d * rd;
    let a = map(hit+float3(0.01, 0, 0)) - map(hit-float3(0.01, 0, 0));
    let b = map(hit+float3(0, 0.01, 0)) - map(hit-float3(0, 0.01, 0));
    let c = map(hit+float3(0, 0, 0.01)) - map(hit-float3(0, 0, 0.01));
    normalize(float3(a, b, c)) * 0.5 + 0.5
}
else
{
    0
}"
  |> mkMultiLineParser

let result, state = blockP dataSrc
match result with
| Success v -> printfn "Result: %A\n\nState: %A" v state 
| _ -> printfn "Error: %A" state