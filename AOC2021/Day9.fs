module Day9

(*
--- Day 9: Smoke Basin ---

These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678

Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?

*)

open System
open System.IO

let read_file file_name =
    File.ReadLines file_name
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map (fun c -> Convert.ToInt32 c - Convert.ToInt32 '0'))
    |> Array.ofSeq

let adjacent_vector = [
    (-1, 0);
    (1, 0);
    (0, -1);
    (0, 1);
]

let is_low_point (m: int[][]) (x, y) =
    let p = m[y][x]
    
    let lowest_adjacent =
        adjacent_vector
        |> Seq.map (fun (dx, dy) -> (x+dx, y+dy))
        |> Seq.filter (fun (_x, _y) ->
            _x >= 0 &&
            _y >= 0 &&
            _x < Array.length m[0] &&
            _y < Array.length m
        )
        |> Seq.map (fun (_x, _y) -> m[_y][_x])
        |> Seq.min

    p < lowest_adjacent

let get_low_points (m: int[][]) =
    let indices = { 0 .. (Array.length m - 1) } |> Seq.collect (fun y -> 
        { 0 .. (Array.length m[0] - 1) } |> Seq.map (fun x -> (x, y))
    )

    let low_points =
        indices
        |> Seq.filter (is_low_point m)
        |> Array.ofSeq

    low_points

let run_part1() =
    let file_name = "../../../inputs/day_09_1.txt"
    let input = read_file file_name

    let low_points = get_low_points input
    let answer = low_points |> Array.sumBy (fun (x, y) -> input[y][x] + 1)

    printfn $"Answer: {answer}"

(*
--- Part Two ---

Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678

The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678

The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678

Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.
*)

let rec get_basin_positions (m: int[][]) (positions: Set<(int * int)>) (x, y) =
    if Set.contains (x, y) positions then
        positions
    else
        let positions = Set.add (x, y) positions
        let adjacent_positions =
            adjacent_vector
            |> Seq.map (fun (dx, dy) -> (x+dx, y+dy))
            |> Seq.filter (fun (_x, _y) ->
                _x >= 0 &&
                _y >= 0 &&
                _x < Array.length m[0] &&
                _y < Array.length m &&
                m[_y][_x] <> 9 &&
                m[_y][_x] >= m[y][x]
            )
        adjacent_positions |> Seq.fold (get_basin_positions m) positions

let run_part2() =
    let file_name = "../../../inputs/day_09_1.txt"
    let input = read_file file_name

    let low_points = get_low_points input
    let basins = low_points |> Array.map (get_basin_positions input Set.empty)

    let answer =
        basins
        |> Array.map Set.count
        |> Array.sortDescending
        |> Array.take 3
        |> Array.fold (*) 1

    printfn $"Answer: {answer}"