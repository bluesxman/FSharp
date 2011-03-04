
let xform1 state v =
    match state with
    | hd :: [] -> ([hd+1], v*v)
    | _ -> ([1], v*v)

let xform2 state v =
    match state with
    | last :: count :: [] -> ([v; count+1], v+last)
    | _ -> ([v; 1], v)  // catches []

let transforms = [xform1; xform2]

let apply transforms states v =
    let applyOne (newStates, vi) xform state =
        let (newState, newVal) = xform state vi
        (newState::newStates, newVal)
    let (newStates, newVal) = List.fold2 applyOne ([], v) transforms states
    (List.rev newStates, newVal)

let rec simulate transforms states startVal maxIter =
    let rec recurse curStates curVal i =
        if i = maxIter
        then (curStates, curVal)
        else
            let (newStates, newVal) = apply transforms curStates curVal
            recurse newStates newVal (i+1)
    recurse states startVal 0

let combine (f:'a->'a) (g:'a->'a) =
    fun x -> f (g x)

let ident = fun x -> x


let foo = combine ident sin

let f x = sin x
let g x = cos x
let h x = x |> f |> g


type Token =
    | Plus
    | Minus
    | Star
    | Slash
    | OpenParen
    | ClosedParen
    | Number

type Expr =
    | BinaryOp of Expr * Token.Plus * Expr
    | Assoc of Token * Expr * Token
    | Token




