open System
open System.IO
open System.Reflection

(* Use reflection to get the day problem number and function to run it. *)
let get_day_problems =
    let assembly = Assembly.GetExecutingAssembly()
    let day_problems =
        Seq.initInfinite(fun x -> x + 1)
        |> Seq.map (fun d ->
            let dt = assembly.GetType($"Day{d}")
            if isNull dt then
                None
            else
                let m = dt.GetMethod("run")
                if not (isNull m) then
                    Some((d, (fun() -> m.Invoke(dt, Array.empty) |> ignore)))
                else
                    None
        )
        |> Seq.takeWhile Option.isSome
        |> Seq.collect Option.toList
        |> List.ofSeq

    day_problems

(* Execute the problem with logging. *)
let execute (name: string) (f: unit -> unit) =
    Console.WriteLine($"----------------------------------------")
    Console.WriteLine($"Running {name}")
    let start_time = DateTime.UtcNow
    f()
    let end_time = DateTime.UtcNow
    let exec_t = (end_time - start_time).TotalMilliseconds
    Console.WriteLine($"Completed {name} - {exec_t} ms.")

[<EntryPoint>]
let main args =
    for (d, df) in get_day_problems do
        execute $"Day {d}" df
    0