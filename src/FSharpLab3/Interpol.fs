module Interpol

//linear block
let linearInterpolationFunc (x0: float) (y0: float) (x1: float) (y1: float) (x: float) =
    y0 * (x1 - x) / (x1 - x0) + y1 * (x - x0) / (x1 - x0)

let rec doInterpolation xyPairs currentStep incStep =
    match Seq.tryHead xyPairs with
    | None -> Seq.empty
    | Some((x0, y0), (x1, y1)) ->
        let newY = linearInterpolationFunc x0 y0 x1 y1 currentStep
        //printfn "x0=%f, x1=%f, nowx = %f\n" x0 x1 currentStep
        if currentStep >= x0 && currentStep <= x1 then
            let resultSeq = seq { yield (currentStep, newY) }
            Seq.append resultSeq (doInterpolation xyPairs (currentStep + incStep) incStep)
        else
            doInterpolation (Seq.tail xyPairs) currentStep incStep

let linearInterpolation xy (currentStep: float) (incStep: float) =
    doInterpolation (Seq.pairwise xy) currentStep incStep

//newton block
let swapFirstAndLast seq =
    Seq.append (Seq.tail seq) (Seq.singleton (Seq.head seq))

let ignoreLast seq = Seq.take (Seq.length seq - 1) seq

let rec calculateDown xy (currentX: float) =
    match Seq.length xy with
    | 1 -> (currentX - fst (Seq.head xy))
    | _ -> (currentX - fst (Seq.head xy)) * (calculateDown (Seq.tail xy) currentX)

let rec calculateCoefficients xy repeats : float =
    match repeats with
    | 1 -> (snd (Seq.head xy)) / (calculateDown (Seq.tail xy) (fst (Seq.head xy)))
    | _ ->
        let currentCoefficient =
            (snd (Seq.head xy)) / (calculateDown (Seq.tail xy) (fst (Seq.head xy)))

        currentCoefficient + calculateCoefficients (swapFirstAndLast xy) (repeats - 1)

let rec calculateXFunction (xy: seq<float * float>) currentX =
    match Seq.length xy with
    | 1 -> (currentX - fst (Seq.last xy))
    | _ -> (currentX - fst (Seq.last xy)) * (calculateXFunction (ignoreLast xy) currentX)

let rec newtonInterpolationValue xy currentX =
    match Seq.length xy with
    | 1 -> snd (Seq.head xy)
    | _ ->
        let currentCoefficient = calculateCoefficients xy (Seq.length xy)
        let currentXFunction = calculateXFunction (ignoreLast xy) currentX

        currentCoefficient * currentXFunction
        + newtonInterpolationValue (ignoreLast xy) currentX

let rec newtonInterpolationSequence xy currentStep stepIncrement =
    if currentStep < fst (Seq.last xy) then
        let nextValue =
            newtonInterpolationSequence xy (currentStep + stepIncrement) stepIncrement

        let currentValue =
            seq { yield currentStep, newtonInterpolationValue xy currentStep }

        Seq.append currentValue nextValue
    else
        Seq.empty