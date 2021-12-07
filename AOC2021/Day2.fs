module Day2

(*
--- Day 2: Dive! ---

Now, you need to figure out how to pilot this thing.

It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:

    forward X increases the horizontal position by X units.
    down X increases the depth by X units.
    up X decreases the depth by X units.

Note that since you're on a submarine, down and up affect your depth, and so they have the opposite result of what you might expect.

The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's going. For example:

forward 5
down 5
forward 8
up 3
down 8
forward 2

Your horizontal position and depth both start at 0. The steps above would then modify them as follows:

    forward 5 adds 5 to your horizontal position, a total of 5.
    down 5 adds 5 to your depth, resulting in a value of 5.
    forward 8 adds 8 to your horizontal position, a total of 13.
    up 3 decreases your depth by 3, resulting in a value of 2.
    down 8 adds 8 to your depth, resulting in a value of 10.
    forward 2 adds 2 to your horizontal position, a total of 15.

After following these instructions, you would have a horizontal position of 15 and a depth of 10. (Multiplying these together produces 150.)

Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?

*)

open System
open System.IO

type direction =
    | Up = 1
    | Down = 2
    | Forward = 3

type movement = {
    direction : direction;
    amount : int;
}

let direction_names = dict[
    "up", direction.Up;
    "down", direction.Down;
    "forward", direction.Forward
]

let direction_parse name = direction_names[name]

let movement_parse (line: string) =
    let tokens = line.Split(" ")
    { direction = direction_parse tokens[0]; amount = Int32.Parse tokens[1] }

type position = {
    horizontal : int;
    depth : int;
}

module Position =
    let initial = { horizontal = 0; depth = 0; }

    let move (p: position) (m: movement) =
        match m.direction with
            | direction.Up      -> { p with depth = p.depth - m.amount }
            | direction.Down    -> { p with depth = p.depth + m.amount }
            | direction.Forward -> { p with horizontal = p.horizontal + m.amount }

    let move_all (p: position) (ms: list<movement>) =
        List.fold move p ms

let run_day2_part1 () =
    let file_name = "../../../day_02_1.txt"
    
    let movements =
        File.ReadLines file_name
        |> Seq.toList
        |> Seq.map movement_parse
        |> Seq.toList
    
    let final_position =
        movements
        |> Position.move_all Position.initial
    
    let answer = final_position.depth * final_position.horizontal

    printfn $"Final Position: {final_position}"
    printfn $"Answer: {answer}"