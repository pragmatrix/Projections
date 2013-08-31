module JungleProcesses

open JungleScheduler
open Chain

open System.Collections.Generic
open System.Linq

type Revision = uint64
type RevisionRange = Revision * Revision // inclusive

type IProjector =
    // invalidates this process and all referrers and and returns the lowest transaction count for this process.
    abstract member invalidate : IScheduler -> unit
    abstract member addReferrer : IProjector -> unit
    abstract member removeReferrer : IProjector -> unit

type IProjector<'a> =
    inherit IProjector
    // evaluates this process and notifies if the values have been produced.
    abstract member evaluate : IScheduler -> (unit -> unit) -> unit
    abstract member values : Chain<'a>


type DependencyRecord = IProjector * IChain

type Dependencies = Dictionary<IProjector, IChain>
type Referrers = HashSet<IProjector>

type EvaluationContext = Dependencies * IScheduler
type EvaluationResult<'a> = { values: 'a list; dependencies: DependencyRecord list }

type Evaluator<'a> = EvaluationContext -> (EvaluationResult<'a> -> unit) -> unit

type Dependency = IChain * IProjector

type DiscreteProcess<'a>(_evaluator : Evaluator<'a>) =

    // our dependencies and their chain links
    let mutable _dependencies : Dependencies = Dependencies()
    // our referrers, the processes that depend on us
    let mutable _referrers : Referrers = Referrers()
    // the begin of our value chain, the first value in case we are valid, set to _end when we are invalid.
    let mutable _begin : Chain<'a> = Chain.empty()
    // the end of our value chain.
    let mutable _end : Chain<'a> = _begin
    let mutable _valid : bool = false

    interface IProjector<'a>
        with 
            member this.invalidate scheduler = this.invalidate scheduler
            member this.evaluate scheduler res = this.evaluateAsync scheduler res
            member this.values = _begin
            member this.addReferrer referrer = _referrers.Add referrer |> ignore
            member htis.removeReferrer referrer = _referrers.Remove referrer |> ignore

    member private this.evaluateAsync (scheduler : IScheduler) res =
        scheduler.schedule (fun () -> this.evaluate scheduler res)

    member private this.evaluate scheduler evalRes = 
        if _valid then evalRes()
        else
        _evaluator (_dependencies, scheduler) (fun res ->
            assert (not _valid)
            let newValues = res.values
            let dependencyRecords = res.dependencies

            _begin <- _end
            _end <- newValues |> List.fold (fun ch v -> ch.produce(v)) _begin

            _dependencies <- dependencyRecords.ToDictionary(fst, snd)
            dependencyRecords |> Seq.map fst |> Seq.iter (fun dep -> dep.addReferrer this)

            _valid <- true
            evalRes() )

    member private this.invalidate (scheduler : IScheduler) =
        if not _valid then () 
        else

        _begin <- _end // forget old values

        _dependencies.Keys |> Seq.iter (fun dep -> dep.removeReferrer this)
        if _referrers.Count <> 0 then
            let referrersCopy = _referrers.ToArray()
            scheduler.schedule (fun () ->
                // we need to copy referrers because subsequent invalidation may change the list
                referrersCopy |> Seq.iter (fun r -> r.invalidate scheduler))

        _valid <- false


type ProcessM<'a> = 
    | ProcessEvaluate of Evaluator<'a>
    | ProcessYield of 'a

type Dictionary<'k,'v> with
    member this.TryGet(k : 'k) : 'v option =
        match this.TryGetValue k with
        | true, v -> Some v
        | false, _ -> None

type ProjectorBuilder() = 
    member this.Bind (dependency: IProjector<'a>, cont: 'a seq -> ProcessM<'b>) : ProcessM<'b> =
        fun (context : EvaluationContext) resultCallback ->
            let dependencies = fst context
            let scheduler = snd context
            dependency.evaluate scheduler (fun () ->
                let dep = dependency :> IProjector
                let values, newDependency = 
                    match dependencies.TryGet dep with
                    | Some chain -> 
                        let values, newChain = (chain :?> Chain<'a>).consumeAll()
                        values, (dep, newChain :> IChain)
                    | None -> Seq.empty, (dep, dependency.values :> IChain)

                let next = cont values
                match next with
                | ProcessEvaluate evalRest ->
                    evalRest context (fun res -> resultCallback { res with dependencies = newDependency :: res.dependencies } )
                | ProcessYield value -> 
                    resultCallback { values = [value]; dependencies = newDependency :: [] }
                )

        |> ProcessEvaluate
        
    member this.Yield (value: 'b) : ProcessM<'b> =
        ProcessYield value

let projector = new ProjectorBuilder()
