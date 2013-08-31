﻿namespace Jungle.Processes.Tests


open NUnit.Framework
open FsUnit
open Chain
open JungleScheduler

[<TestFixture>]
type ProcessesTests() =

    member this.instantiateGenerator() = 
        let x = input [10]
        let y = input [10]
        let p = generator {
            let! x = x
            let! y = y
            return Seq.zip x y |> Seq.map (*)
        }    

        evaluate p |> should equal [20]

      