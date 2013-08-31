namespace Jungle.Processes.Tests

open NUnit.Framework
open FsUnit
open Chain

[<TestFixture>]
type ChainTests() = 

    [<Test>]
    member this.produceConsumeTest() =
        let ch = Chain.empty()
        let consumer1 = ch
        let producer = ch.produce 1
        consumer1.consumeAll() |> fst |> should equal [1]

    [<Test>]
    member this.doubleProduceConsumeTest() =
        let ch = Chain.empty()
        let consumer1 = ch
        let producer = ch.produce 1
        let producer = producer.produce 2
        consumer1.consumeAll() |> fst |> should equal [1; 2]

    [<Test>]
    member this.doubleProduceDoubleConsumeTest() =
        let ch = Chain.empty()
        let consumer1 = ch
        let producer = ch.produce 1
        let cons = consumer1.consumeAll() 
        cons |> fst |> Seq.toArray |> should equal [1]
        let producer = producer.produce 2
        cons |> snd |> fun c -> c.consumeAll() |> fst |> should equal [2]

    [<Test>]
    member this.twoConsumersAtDifferentPositions() =
        let ch = Chain.empty()
        let consumer1 = ch
        let consumer2 = ch
        let producer = ch.produce 1
        let cons = consumer1.consumeAll() 
        cons |> fst |> Seq.toArray |> should equal [1]
        let producer = producer.produce 2
        cons |> snd |> fun c -> c.consumeAll() |> fst |> should equal [2]
        consumer2.consumeAll() |> fst |> should equal [1;2]

    [<Test>]
    member this.ifNothingConsumedConsumeAllReturnsEmpty() =
        let ch = Chain.empty()
        let consumer1 = ch
        consumer1.consumeAll() |> fst |> should equal []

    [<Test>]
    member this.ifAllConsumedConsumeAllReturnsEmpty() =
        let ch = Chain.empty()
        let producer = ch.produce 1
        let cons = ch.consumeAll() 
        cons |> fst |> should equal [1]
        cons |> snd |> fun c -> c.consumeAll() |> fst |> should equal []
        