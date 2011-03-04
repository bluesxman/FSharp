open System

type Token =
    | Plus
    | Minus
    | Star
    | Slash
    | Literal of string
    | OpenParen

type Expr =
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Assoc of Expr
    | Number of float

type Symbol =
    | E
    | O
    | L


let lexer (code:string) = [Plus]

let parser code = 
    let rec parse tokens stack =
        let tok :: ttail = tokens
        let top :: stail = stack
        match (tok, top) with
        | _,_ when tok = top -> parse ttail, stail
        | L, E -> parse tok (E :: L
    
