open System

let timeStep = 0.1

let createTimes start finish =
    [(start + timeStep) .. timeStep .. finish]

let normalize lower upper x =
    (x - lower) / (upper - lower)

let denormalize lower upper x =
    x * (upper - lower) + lower

let pio2 = Math.PI / 2.0
let pi = Math.PI

let sinusoid amp freq phase theta =
    amp * sin (freq * theta + phase)

let peaking x =
    sinusoid 1.0 1.0 (x * pio2) 0.0

let falling x =
    sinusoid 1.0 1.0 (pio2 + x * pio2) 0.0

let rising x =
    1.0 + sinusoid 1.0 1.0 (3.0 * pio2 + x * pio2) 0.0

let bottoming x =
    1.0 + sinusoid 1.0 1.0 (pi + x * pio2) 0.0

let tempGen curve endTemp endTime (tempList, startTemp, startTime) =
    let timeToTemp time =
        curve (normalize startTime endTime time) |> denormalize startTemp endTemp
    let temps =  createTimes startTime endTime |> List.map timeToTemp
    (tempList @ temps, endTemp, endTime)

let start (temp) =
    ([], temp, 0.0)

let finish (tempList, startTemp, startTime) =
    tempList

let rise = tempGen rising
let fall = tempGen peaking
let peak = tempGen peaking
let bottom = tempGen rising
let cool = tempGen (fun x -> x)
let heat = cool
let maintain = tempGen (fun x -> 1.0)




// START HERE

let temps = 
    start       70.0 
    |> maintain 70.0  4.5
    |>     heat 72.0  5.0
    |> maintain 72.0  6.5
    |>     rise 86.0 13.0
    |>     peak 90.0 16.0
    |>     cool 76.0 17.5
    |> maintain 76.0 22.0
    |>     cool 70.0 23.0
    |> maintain 70.0 24.0
    |>   finish

let searchSpace = List.toArray temps

let shcSearch (space:float[]) maxIter =
    let dist = 40
    let cost x = -space.[x]
    let rand = new Random()
    let bound (a, b) = 
        ((if a < 0 then 0 else a), (if b >= space.Length then space.Length - 1 else b))
    let randNear x = 
        let (low, hi) = bound(x - dist, x + dist)
        rand.Next(low, hi)
    let randNeighbor best =
        let candidate = randNear best
        if cost candidate < cost best then candidate else best
    let rec explore best count =
        if count = maxIter 
        then best
        else explore (randNeighbor best) (count + 1)
    explore (searchSpace.Length / 2) 0


let hotTime = (float (shcSearch searchSpace 20) + 1.0) * timeStep  // in hours