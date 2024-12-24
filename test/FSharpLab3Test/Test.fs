module InterpolTest

open System
open Xunit
open Interpol
open FsCheck.Xunit
open System.Collections.Generic

let abs x = if x >= 0.0 then x else -x

let compareResult part1 part2 =
    Seq.forall2 (fun pair1 pair2 -> (abs ((snd pair1) - (snd pair2)) <= 0.01)) part1 part2

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``linear test`` () =
    let x =
        seq {
            yield 0.000
            yield 1.571
        }

    let y =
        seq {
            yield 0.000
            yield 1
        }

    let actual = linearInterpolation (Seq.zip x y) 0.00 1.00

    let expected =
        seq {
            (0.0, 0.0)
            (1.0, 0.6365372374)
            (2.0, 1.273074475)
        }

    Assert.True(compareResult expected actual)
