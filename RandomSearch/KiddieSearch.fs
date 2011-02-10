open System

let makeRands qty min max = // Just think if I imported this!!!
    let rand = new Random()
    seq {for i in 1..qty -> rand.Next(min, max)}

let searchSpace = Seq.toArray (makeRands 10 -5 5)

let cost i = i * i * i

let search (space:int[]) samples =
    let pick best i = if cost space.[i] < cost best then space.[i] else best
    let rands = makeRands samples 0 space.Length
    Seq.fold pick space.[Seq.head rands] rands

printf "Search space = %A\nminimum found = %d\n" searchSpace (search searchSpace 3)