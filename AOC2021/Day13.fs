module Day13

(*
--- Day 13: Transparent Origami ---

You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

Congratulations on your purchase! To activate this infrared thermal imaging
camera system, please enter the code found on page 1 of the manual.

Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5

The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........

Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........

Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........

Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....

Because this is a vertical line, fold left:

#####
#...#
#...#
#...#
#####
.....
.....

The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on your transparent paper?

*)

open System
open System.IO
open System.Text.RegularExpressions

type Fold =
    | Horizontal of int
    | Vertical of int

type Config = {
    points: Set<int * int>;
    folds: list<Fold>;
}

module PositionMap =

    type position_map = {
        points: Set<int * int>;
        width: int;
        height: int;
    }

    let create (points: Set<int * int>) =
        let x_max = points |> Seq.map (fun (x, y) -> x) |> Seq.max
        let y_max = points |> Seq.map (fun (x, y) -> y) |> Seq.max
        { points = points; width = x_max + 1; height = y_max + 1; }

    let render (pm: position_map) =
        let render_pos x y =
            if Set.contains (x, y) pm.points then "#" else " "

        let get_row y =
            String.Join("", { 0 .. pm.width - 1 } |> Seq.map (fun x -> render_pos x y))

        String.Join("\n", { 0 .. pm.height - 1 } |> Seq.map get_row)

    let get_transform max_v v =
        let v_offset = v - Math.Abs(max_v - 1 - v)
        let v_offset = -Math.Min(v_offset, 0)

        let new_v (_v: int) =
            let nv = v - Math.Abs(_v - v) + v_offset
            nv

        let new_max = v + v_offset

        (new_max, new_v)

    let apply_horizontal_fold (pm: position_map) (fold_pos: int) =
        let (new_height, new_y) = get_transform pm.height fold_pos

        let new_points =
            pm.points |> Set.map (fun (x, y) ->
                (x, new_y y)
            )

        { pm with points = new_points; height = new_height; }

    let apply_vertical_fold (pm: position_map) (fold_pos: int) =
        let (new_width, new_x) = get_transform pm.width fold_pos

        let new_points =
            pm.points |> Set.map (fun (x, y) ->
                (new_x x, y)
            )

        { pm with points = new_points; width = new_width; }

    let apply_fold (pm: position_map) (f: Fold) =
        match f with
            | Horizontal y -> apply_horizontal_fold pm y
            | Vertical x -> apply_vertical_fold pm x

    let apply_folds (pm: position_map) (folds: seq<Fold>) =
        Seq.fold apply_fold pm folds

let point_regex = new Regex("(\d+),(\d+)")
let fold_regex = new Regex("fold along ([xy])=(\d+)")

let load_file file_name =
    let add_line (config: Config) (line: string) =
        if line = "" then
            config
        else
            let point_m = point_regex.Match(line)
            if point_m.Success then
                let x = int point_m.Groups[1].Value
                let y = int point_m.Groups[2].Value
                { config with points = Set.add (x, y) config.points }
            else
                let fold_m = fold_regex.Match(line)
                if fold_m.Success then
                    let direction = fold_m.Groups[1].Value
                    let pos = int fold_m.Groups[2].Value
                    let fold =
                        if direction = "x" then Vertical pos
                        else Horizontal pos
                    { config with folds = fold :: config.folds }
                else
                    config

    let config =
        File.ReadLines file_name
        |> Seq.fold add_line { points = Set.empty; folds = List.empty; }

    // Reverse the folds.
    { config with folds = List.rev config.folds }

let run_part1() =
    let file_name = "../../../inputs/day_13_1.txt"
    let config = load_file file_name
    let pm = PositionMap.create config.points

    let new_pm = PositionMap.apply_folds pm (Seq.take 1 config.folds)
    let answer = Set.count new_pm.points

    printfn $"Answer: {answer}"

(*
--- Part Two ---

Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?

*)
let run_part2() =
    let file_name = "../../../inputs/day_13_1.txt"
    let config = load_file file_name
    let pm = PositionMap.create config.points

    let new_pm = PositionMap.apply_folds pm config.folds
    let answer = PositionMap.render new_pm

    printfn $"Answer:"
    printfn $"{answer}"