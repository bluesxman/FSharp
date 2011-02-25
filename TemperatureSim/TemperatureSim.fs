module CleverAlgorithms.TemperatureSim


let STEP_SIZE = 0.01 // in seconds
let AMBIENT_EFFECT = 1.0 * STEP_SIZE
let COOLER_EFFECT = 50.0 * STEP_SIZE

let simulate influences startTemp stepSize duration goalTemp =
    let rec run time systemTemp temps =
        let applyInfluences currentTime =
            let change total influence = total + influence systemTemp time
            influences |> List.fold change 0.0
        if time >= duration 
        then temps
        else 
            let newTemp = systemTemp + applyInfluences time
            run (time + stepSize) newTemp (newTemp :: temps)
    run 0 startTemp []

let computeError temps goal =
    let errorAccum error t = error + abs(t - goal)
    temps |> List.fold errorAccum 0.0

let ambient (tempCurve:float[]) stepSize systemTemp time =
    let index = (int)(time / stepSize)
    let ambTemp = tempCurve.[index]
    AMBIENT_EFFECT * (ambTemp - systemTemp)

let fermentation x = x

let pidControl (pGain, iGain, dGain) goalTemp curTemp lastTemp totalError =
    let errorNow = curTemp - goalTemp
    let errorLast = lastTemp - goalTemp
    let deltaError = errorNow - errorLast
    let output = pGain * errorNow + iGain * (errorNow + totalError) + dGain * deltaError
    output