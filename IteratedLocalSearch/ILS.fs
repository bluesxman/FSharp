﻿open System

// a function that is the sum of two sine waves of different frequencies
let sineSpace xFreq yFreq theta =
    sin (xFreq * theta) + sin (yFreq * theta)

// Generates a function that returns a random number in the neighborhood of a 
// a center.  The distance from the center is with +/- the radius.
let radiusRandGen () =
    let rand = new Random()
    fun center radius -> (rand.NextDouble() * 2.0 * radius) + center - radius

// Builds a list of the last 3 elements to be recorded
let record history x =
    match history with
    | a :: b :: c :: tail -> x :: a :: b :: []
    | _ -> x :: history

// Finds a number within a radius of 'best' but far from the elements
// in history
let perturbBase rng radius best history =
    let proximity x =
        let calc prox i = 
            let tmp = radius - abs (x - i)
            if tmp < 0.0 then 0.0 else tmp
        history |> List.fold calc 0.0

    let rec next furthest fProx i =
        if fProx = 0.0 || i = 0
        then furthest
        else 
            let candidate = rng best radius
            let cProx = proximity candidate
            if cProx < fProx
            then next candidate cProx (i-1)
            else next furthest fProx (i-1)

    let first = rng best radius
    next first (proximity first) 10

// searches randomly in the area of the best candidate
let hillClimb rng radius maxIter start cost =
    let randNeighbor best =
        let candidate = rng best radius
        if cost candidate < cost best then candidate else best
    let rec explore best count =
        if count = maxIter 
        then best
        else explore (randNeighbor best) (count + 1)
    explore start 0


let iterativeLocal perturb local maxIter start cost =
    let rec search best history i =
        if i = maxIter
        then best
        else 
            let candidate = local (perturb best history) cost
            let nextHistory = record history candidate
            let nextBest = if cost candidate < cost best then candidate else best
            search nextBest nextHistory (i+1)
    search start [] 0

// Build it
let searchSpace = sineSpace 0.123 0.836
let localRadius = 0.2
let perturbRadius = 1.0
let localIter = 1000
let mainIter = 100

let cost x = -searchSpace x
let rng = radiusRandGen ()
let perturb = perturbBase rng perturbRadius
let local = hillClimb rng localRadius localIter

let search = iterativeLocal perturb local mainIter
let best = search 0.0 cost

printf "best = %f space[x]=%f" best (searchSpace best) 
