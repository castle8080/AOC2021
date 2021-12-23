module Day7

(*
--- Day 7: The Treachery of Whales ---

A giant whale has decided your submarine is its next meal, and it's much faster than you are. There's nowhere to run!

Suddenly, a swarm of crabs (each in its own tiny submarine - it's too deep for them otherwise) zooms in to rescue you! They seem to be preparing to blast a hole in the ocean floor; sensors indicate a massive underground cave system just beyond where they're aiming!

The crab submarines all need to be aligned before they'll have enough power to blast a large enough hole for your submarine to get through. However, it doesn't look like they'll be aligned before the whale catches you! Maybe you can help?

There's one major catch - crab submarines can only move horizontally.

You quickly make a list of the horizontal position of each crab (your puzzle input). Crab submarines have limited fuel, so you need to find a way to make all of their horizontal positions match while requiring them to spend as little fuel as possible.

For example, consider the following horizontal positions:

16,1,2,0,4,2,7,1,2,14

This means there's a crab with horizontal position 16, a crab with horizontal position 1, and so on.

Each change of 1 step in horizontal position of a single crab costs 1 fuel. You could choose any horizontal position to align them all on, but the one that costs the least fuel is horizontal position 2:

    Move from 16 to 2: 14 fuel
    Move from 1 to 2: 1 fuel
    Move from 2 to 2: 0 fuel
    Move from 0 to 2: 2 fuel
    Move from 4 to 2: 2 fuel
    Move from 2 to 2: 0 fuel
    Move from 7 to 2: 5 fuel
    Move from 1 to 2: 1 fuel
    Move from 2 to 2: 0 fuel
    Move from 14 to 2: 12 fuel

This costs a total of 37 fuel. This is the cheapest possible outcome; more expensive outcomes include aligning at position 1 (41 fuel), position 3 (39 fuel), or position 10 (71 fuel).

Determine the horizontal position that the crabs can align to using the least fuel possible. How much fuel must they spend to align to that position?

*)
open System
open System.IO

let read_crab_positions file_name =
    let line = File.ReadLines file_name |> Seq.head
    line.Split(',')
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.map Convert.ToInt32
    |> Array.ofSeq

let normal_fuel_spend crab_positions (n: int) =
    Array.sumBy (fun p -> Math.Abs (p - n)) crab_positions 
    
let rec get_best_position crab_positions fuel_spend_alg =
    let min_pos = Array.min crab_positions
    let max_pos = Array.max crab_positions     
    let fuel_spend = fuel_spend_alg crab_positions

    let rec _get_best n_left n_right =
        if n_left = n_right then
            (n_left, fuel_spend n_left)
        else if n_right - n_left = 1 then
            let cost_left = fuel_spend n_left
            let cost_right = fuel_spend n_right
            if cost_left < cost_right then
                (n_left, cost_left)
            else
                (n_right, cost_right)
        else
            let n_pos = (n_left + n_right) / 2
            let cost_pos = fuel_spend n_pos
            let cost_pos_l = fuel_spend (n_pos - 1)
            let cost_pos_r = fuel_spend (n_pos + 1)
            if cost_pos <= cost_pos_l && cost_pos <= cost_pos_r then
                (n_pos, cost_pos)
            else if cost_pos_l < cost_pos then
                _get_best n_left n_pos
            else
                _get_best n_pos n_right

    _get_best min_pos max_pos

let run_part1() =
    let file_name = "../../../day_07_1.txt"
    let crab_positions = read_crab_positions file_name

    let (pos, fuel_cost) = get_best_position crab_positions normal_fuel_spend
    printfn $"Best Position: {pos}"
    printfn $"Fuel Cost: {fuel_cost}"

(*
--- Part Two ---

The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab engineering?

As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.

As each crab moves, moving further becomes more expensive. This changes the best horizontal position to align them all on; in the example above, this becomes 5:

    Move from 16 to 5: 66 fuel
    Move from 1 to 5: 10 fuel
    Move from 2 to 5: 6 fuel
    Move from 0 to 5: 15 fuel
    Move from 4 to 5: 1 fuel
    Move from 2 to 5: 6 fuel
    Move from 7 to 5: 3 fuel
    Move from 1 to 5: 10 fuel
    Move from 2 to 5: 6 fuel
    Move from 14 to 5: 45 fuel

This costs a total of 168 fuel. This is the new cheapest possible outcome; the old alignment position (2) now costs 206 fuel instead.

Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route! How much fuel must they spend to align to that position?

*)

let exp_fuel_spend crab_positions (n: int) =
    crab_positions |> Array.sumBy (fun p ->
        let delta = Math.Abs (p - n)
        (delta + 1) * delta / 2
    )

let run_part2() =
    let file_name = "../../../day_07_1.txt"
    let crab_positions = read_crab_positions file_name

    let (pos, fuel_cost) = get_best_position crab_positions exp_fuel_spend
    printfn $"Best Position: {pos}"
    printfn $"Fuel Cost: {fuel_cost}"