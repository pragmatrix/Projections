namespace Jungle.Processes.Tests


open NUnit.Framework
open FsUnit
open Chain
open Scheduler

[<TestFixture>]
type ProcessesTests() =

    member this.instantiateAndProject() = 
        let x = signal [10]
        let y = signal [10]
        let p = projector {
            let! x = x
            let! y = y
            return Seq.zip x y |> Seq.map (*)
        }    

        project p |> should equal [20]

      