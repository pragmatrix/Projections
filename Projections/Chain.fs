module Chain

open System.Collections
open System.Collections.Generic

(*
    A one producer / multiple consumer value chain
*)

type IChain = interface
    end

type Chain<'a> = { mutable next: ('a * Chain<'a>) option }
    with
        // the canonical IEnumerable is returning all values until the end of the chain
        interface IEnumerable<'a> 
            with 
                member this.GetEnumerator() : IEnumerator<'a> =
                    let gen = this.links() |> Seq.map fst
                    gen.GetEnumerator()
                member this.GetEnumerator() : IEnumerator =
                    (this :> IEnumerable<'a>).GetEnumerator() :> IEnumerator

        interface IChain

        // Adds a value and returns the new end of the chain.
        member this.produce value =
            assert (this.isEnd)
            let newEnd = Chain<'a>.empty()
            this.next <- Some (value, newEnd)
            newEnd

        // returns all the values and the end link
        member this.consumeAll() = 
            let a = this.links() |> Seq.toArray
            if a.Length = 0 then
                Seq.empty, this
            else
                a |> Array.unzip |> fst |> Seq.ofArray, a.[a.Length-1] |> snd

        static member empty() = { next = None }

        // returns values alongside their next-links
        member private this.links() =
            this.next |> Seq.unfold (fun link -> 
                match link with
                | Some ((v, n) as l) -> Some (l, n.next)
                | None -> None
                )

        member private this.isEnd = this.next.IsNone
