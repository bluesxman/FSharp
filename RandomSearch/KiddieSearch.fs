module CleverAlgorithms.RandomSearch

open System

let makeRandSeq qty min max = // Just think if I imported this!!!
    let rand = new Random()
    seq {for i in 1..qty -> rand.Next(min, max)}

let cost i = i * i * i

let search (space:int[]) samples =
    let pick best i = if cost space.[i] < cost best then space.[i] else best
    let randIndexes = makeRandSeq samples 0 space.Length
    Seq.fold pick space.[Seq.head randIndexes] randIndexes

let printSearch space samples =
    printf "Search space = %A\nminimum found = %d\n" space (search space samples)

let testFirst = [|-5..5|]
let testLast = [|5..-1..-5|]
let testMid = List.toArray ([0..5] @ -5 :: [-4..-1])
let searchSpace = Seq.toArray (makeRandSeq 10 -5 5)

printSearch testFirst 10000
printSearch testLast 10000
printSearch testMid 10000
printSearch searchSpace 3
