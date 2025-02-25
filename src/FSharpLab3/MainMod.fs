module Program

open System
open System.IO
open Interpol

type ApproxState =
    { Name: string
      Points: (float * float) seq
      Step: float
      Func: (float * float) seq -> float -> float -> (float * float) seq }

let readPoint () =
    let point = Console.ReadLine().Split(";")
    let x = point[0] |> float
    let y = point[1] |> float
    (x, y)

let seqAdd someSeq point =
    Seq.append someSeq (Seq.singleton point)

let calculateAndPrint approxName someSeq step approx =
    let result = approx someSeq (fst (Seq.head someSeq)) step
    printfn "%s\n" approxName

    for pair in result do
        printfn "%A" pair

let rec doWhileRecursively (states: seq<ApproxState>) =
    for state in states do
        calculateAndPrint state.Name state.Points state.Step state.Func

    let newPoint = readPoint ()

    let newStates =
        seq {
            for state in states do
                yield
                    { state with
                        Points = (seqAdd (Seq.tail state.Points) (newPoint)) }
        }

    doWhileRecursively newStates

[<EntryPoint>]
let main (args) =
    printf "Step is: "
    let step = Console.ReadLine() |> float
    let firstPoint = readPoint ()
    let secondPoint = readPoint ()

    let someSeq =
        seq {
            firstPoint
            secondPoint
        }

    match args with
    | [| "both" |] ->


        let result = linearInterpolation someSeq (fst (Seq.head someSeq)) step
        printfn "%s\n" "Linear"

        for pair in result do
            printfn "%A" pair

        let newPoint = readPoint ()

        let linear =
            { Name = "Linear"
              Points = Seq.tail (seqAdd someSeq newPoint)
              Step = step
              Func = linearInterpolation }

        let newton =
            { Name = "Newton"
              Points = seqAdd someSeq newPoint
              Step = step
              Func = newtonInterpolationSequence }

        doWhileRecursively (
            seq {
                linear
                newton
            }
        )
    | [| "linear" |] ->
        let someSeq = seqAdd (Seq.singleton firstPoint) secondPoint

        let result = linearInterpolation someSeq (fst (Seq.head someSeq)) step
        printfn "%s\n" "Linear"

        for pair in result do
            printfn "%A" pair

        let newPoint = readPoint ()

        let linear =
            { Name = "Linear"
              Points = Seq.tail (seqAdd someSeq newPoint)
              Step = step
              Func = linearInterpolation }

        doWhileRecursively (seq { linear })
    | [| "newton" |] ->

        let thirdPoint = readPoint ()
        let someSeq2 = seqAdd someSeq thirdPoint

        let result = newtonInterpolationSequence someSeq2 (fst (Seq.head someSeq2)) step
        printfn "%s\n" "Newton"

        for pair in result do
            printfn "%A" pair

        let newPoint = readPoint ()

        let newton =
            { Name = "Newton"
              Points = seqAdd someSeq2 newPoint
              Step = step
              Func = newtonInterpolationSequence }

        doWhileRecursively (seq { newton })
    | _ -> printfn "unknown arg"

    0
