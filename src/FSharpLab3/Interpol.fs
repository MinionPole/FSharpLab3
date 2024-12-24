module Interpol

//linear block
let linearInterpolationFunc (x0: float) (y0: float) (x1: float) (y1: float) (x: float) =
    y0 * (x1 - x) / (x1 - x0) + y1 * (x - x0) / (x1 - x0)

let prepareXY (x: seq<float>) (y: seq<float>) =
    let xy = Seq.zip x y
    Seq.pairwise xy

let rec doInterpolation xyPairs currentStep incStep =
    match Seq.tryHead xyPairs with
    | None -> Seq.empty
    | Some((x0, y0), (x1, y1)) ->
        let newY = linearInterpolationFunc x0 y0 x1 y1 currentStep
        let resultSeq = seq { yield (currentStep, newY) }

        if currentStep >= x0 && currentStep <= x1 then
            Seq.append resultSeq (doInterpolation xyPairs (currentStep + incStep) incStep)
        else
            Seq.append resultSeq (doInterpolation (Seq.tail xyPairs) currentStep incStep)

let linearInterpolation xy (currentStep: float) (incStep: float) =
    doInterpolation (Seq.pairwise xy) currentStep incStep

//newton block
let swapSeq someSeq =
    Seq.append (Seq.tail someSeq) (Seq.singleton (Seq.head someSeq))

let IgnoreLastSeq someSeq =
    Seq.take (Seq.length someSeq - 1) someSeq

let rec down xy (currentX: float) =
    match Seq.length xy with
    | 1 -> (currentX - fst (Seq.head xy))
    | _ -> (currentX - fst (Seq.head xy)) * (down (Seq.tail xy) currentX)

let rec coefs xy repeats : float =
    match repeats with
    | 1 -> (snd (Seq.head xy)) / (down (Seq.tail xy) (fst (Seq.head xy)))
    | _ ->
        (snd (Seq.head xy)) / (down (Seq.tail xy) (fst (Seq.head xy)))
        + (coefs (swapSeq xy) (repeats - 1))

let rec xFunc (xy: seq<float * float>) currentX =
    match Seq.length xy with
    | 1 -> (currentX - fst (Seq.last xy))
    | _ -> (currentX - fst (Seq.last xy)) * (xFunc (IgnoreLastSeq xy) currentX)

let rec newtonInterpolationFunc xy currentX =
    match Seq.length xy with
    | 1 -> snd (Seq.head xy)
    | _ ->
        let currentCoef = coefs xy (Seq.length xy)
        let currentxFunc = xFunc (IgnoreLastSeq xy) currentX

        (currentCoef) * (currentxFunc)
        + (newtonInterpolationFunc (IgnoreLastSeq xy) currentX)

let rec newtonInterpolation xy currentStep incStep =
    if currentStep < fst (Seq.last xy) then
        let nextValue = newtonInterpolation xy (currentStep + incStep) incStep
        let currnetValue = seq { currentStep, newtonInterpolationFunc xy currentStep }
        Seq.append currnetValue nextValue
    else
        seq { currentStep, newtonInterpolationFunc xy currentStep }
