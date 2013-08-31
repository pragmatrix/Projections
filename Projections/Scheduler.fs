module JungleScheduler

open System.Collections.Generic

type IScheduler =
    abstract member schedule : (unit -> unit) -> unit
    abstract member run : unit -> unit


type Scheduled<'a> = 
    | Schedule of (IScheduler -> ('a -> unit) -> unit)
    | Value of 'a

type Scheduler() =
    let _queue = new Queue<unit -> unit>()
    
    interface IScheduler
        with
            member this.schedule f = _queue.Enqueue f
            member this.run() =
                while (_queue.Count <> 0) do
                    _queue.Dequeue()()

type ScheduledBuilder() =
    member this.Bind (value : 'a, cont : 'a -> Scheduled<'b>) : Scheduled<'b> =
        fun (scheduler:IScheduler) f ->
            let r = cont value
            match r with
            | Schedule s -> 
                let x = (fun () -> s scheduler f)
                scheduler.schedule x
            | Value v -> f v
        |> Schedule

    member this.Return value : Scheduled<'a> =
        Value value


type scheduled = SchedulerBuilder