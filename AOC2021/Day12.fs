module Day12

(*
--- Day 12: Passage Pathing ---

With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting out of this cave anytime soon is by finding a path yourself. Not just a path - the only way to know if you've found the best path is to find all of them.

Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining caves (your puzzle input). For example:

start-A
start-b
A-c
A-b
b-d
A-end
b-end

This is a list of how all of the caves are connected. You start in the cave named start, and your destination is the cave named end. An entry like b-d means that cave b is connected to cave d - that is, you can move between them.

So, the above cave system looks roughly like this:

    start
    /   \
c--A-----b--d
    \   /
     end

Your goal is to find the number of distinct paths that start at start, end at end, and don't visit small caves more than once. There are two types of caves: big caves (written in uppercase, like A) and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave more than once, but big caves are large enough that it might be worth visiting them multiple times. So, all paths you find should visit small caves at most once, and can visit big caves any number of times.

Given these rules, there are 10 paths through this example cave system:

start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end

(Each line in the above list corresponds to a single path; the caves visited by that path are listed in the order they are visited and separated by commas.)

Note that in this cave system, cave d is never visited by any path: to do so, cave b would need to be visited twice (once on the way to cave d and a second time when returning from cave d), and since cave b is small, this is not allowed.

Here is a slightly larger example:

dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc

The 19 paths through it are as follows:

start,HN,dc,HN,end
start,HN,dc,HN,kj,HN,end
start,HN,dc,end
start,HN,dc,kj,HN,end
start,HN,end
start,HN,kj,HN,dc,HN,end
start,HN,kj,HN,dc,end
start,HN,kj,HN,end
start,HN,kj,dc,HN,end
start,HN,kj,dc,end
start,dc,HN,end
start,dc,HN,kj,HN,end
start,dc,end
start,dc,kj,HN,end
start,kj,HN,dc,HN,end
start,kj,HN,dc,end
start,kj,HN,end
start,kj,dc,HN,end
start,kj,dc,end

Finally, this even larger example has 226 paths through it:

fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW

How many paths through this cave system are there that visit small caves at most once?

*)

open System
open System.Collections.Generic
open System.IO


module Caves =

    type caves = {
        connections: Map<string, Set<string>>;
    }

    let empty_caves =
        { connections = Map.empty; }: caves


    let is_big_cave (c: string) =
        Char.IsUpper(c, 0)

    let add_connection (c1: string) (c2: string) (cs: caves) =

        let add_to_set (k: string) (c: string) (m: Map<string, Set<string>>) =
            let s = Map.tryFind k m |> Option.defaultValue Set.empty
            let ns = Set.add c s
            Map.add k ns m
        
        let new_conns =
            cs.connections
            |> add_to_set c1 c2
            |> add_to_set c2 c1

        { connections = new_conns; }: caves

    let load file_name: caves =
        let parse_line (line: string) =
            let parts = line.Split('-')
            (parts[0], parts[1])

        let cs =
            File.ReadLines file_name
            |> Seq.map parse_line
            |> Seq.fold (fun cs (c1, c2) -> add_connection c1 c2 cs) empty_caves

        cs

    let rec _all_paths (path: list<string>) (path_validator: list<string> -> bool) (cs: caves): list<list<string>> =
        let last_node = List.head path
        if last_node = "end" then
            [path]
        else
            let next_paths =
                Map.find last_node cs.connections
                |> Set.map (fun n -> n :: path)
                |> Set.filter path_validator

            let all_paths =
                next_paths
                |> Seq.map (fun np -> _all_paths np path_validator cs )
                |> Seq.fold (fun all_paths paths -> List.append paths all_paths) []
            all_paths

    let check_path_p1 (path: list<string>) =
        match path with
            | n :: path -> is_big_cave n || not (List.contains n path)
            | [] -> true

    let check_path_p2 (path: list<string>) =
        let is_valid_count (pair: KeyValuePair<string, int>) =
            if is_big_cave pair.Key then
                true
            else if pair.Key = "start" then
                pair.Value <= 1
            else
                pair.Value <= 2

        let all_path_counts_valid (counts: Map<string, int>) =
            counts |> Seq.tryFind (not << is_valid_count) |> Option.isNone

        let only_1_small_cave_multiple_visits (counts: Map<string, int>) =
            let small_cave_multi_visits =
                counts
                |> Seq.filter (fun p -> not (is_big_cave p.Key) && p.Value >= 2)
                |> Seq.length
            small_cave_multi_visits <= 1

        let node_counts = Seq.countBy id path |> Map.ofSeq
        let valid =
            all_path_counts_valid node_counts &&
            only_1_small_cave_multiple_visits node_counts

        valid

    let all_paths_p1 cs =
        _all_paths ["start"] check_path_p1 cs

    let all_paths_p2 cs =
        _all_paths ["start"] check_path_p2 cs

let run_part1() =
    let file_name = "../../../day_12_1.txt"
    let cs = Caves.load file_name

    let paths = Caves.all_paths_p1 cs
    let answer = List.length paths

    printfn $"Day 12 Part 1"
    printfn $"    Answer: {answer}"

(*
--- Part Two ---

After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.

Now, the 36 possible paths through the first example above are:

start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end

The slightly larger example above now has 103 paths through it, and the even larger example now has 3509 paths through it.

Given these new rules, how many paths through this cave system are there?

*)

let run_part2() =
    let file_name = "../../../day_12_1.txt"
    let cs = Caves.load file_name

    // TODO: It would be more efficient to maintain a small cave visit count context
    // while searching paths.

    let paths = Caves.all_paths_p2 cs
    let answer = List.length paths

    printfn $"Day 12 Part 2"
    printfn $"    Answer: {answer}"

let run() =
    run_part1()
    run_part2()