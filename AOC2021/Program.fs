open System
open System.IO
open System.Reflection

(* Use reflection to get the day problem number and function to run it. *)
let get_problems =
    let assembly = Assembly.GetExecutingAssembly()

    let get_problems_for_day d =
        let m = assembly.GetType($"Day{d}")
        if not (isNull m) then
            [1; 2;]
            |> List.map (fun pn -> (pn, m.GetMethod($"run_part{pn}")))
            |> List.filter (fun (pn, _method) -> not (isNull _method))
            |> List.map (fun (pn, _method) ->
                (d, pn, (fun() -> _method.Invoke(m, Array.empty) |> ignore))
            )
        else
            List.empty

    let all_problems =
        { 1 .. 25 }
        |> Seq.collect get_problems_for_day
        |> List.ofSeq 

    all_problems

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
    for (d, pn, f) in get_problems do
        execute $"Day {d} - Part {pn}" f
    0