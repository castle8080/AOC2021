module Day4

(*
--- Day 4: Giant Squid ---

You're already almost 1.5km (almost a mile) below the surface of the ocean,
already so deep that you can't see any sunlight. What you can see, however,
is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers.
Numbers are chosen at random, and the chosen number is marked on all boards
on which it appears. (Numbers may not appear on all boards.) If all numbers
in any row or any column of a board are marked, that board wins.
(Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and
the giant squid) pass the time. It automatically generates a random order
in which to draw numbers and a random set of boards (your puzzle input).

For example:

7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no
winners, but the boards are marked as follows (shown here adjacent to each
other to save space):

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are
still no winners:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

Finally, 24 is drawn:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

At this point, the third board wins because it has at least one complete row
or column of marked numbers (in this case, the entire top row is marked:
14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the
sum of all unmarked numbers on that board; in this case, the sum is 188.
Then, multiply that sum by the number that was just called when the board
won, 24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win
first. What will your final score be if you choose that board?

*)

open System
open System.IO
open System.Text.RegularExpressions

type game_state = {
    boards: int[][][];
    number_sets: Set<int>[][];
    number_set_reverse_index: Map<int, list<(int * int * int)>>;
    called_numbers: list<int>;
    winning_boards: Set<int>;
}

module Board =
    let get_column_set (board: int[][]) (c: int) =
        board
        |> Seq.ofArray
        |> Seq.map (fun row -> row[c])
        |> Set.ofSeq

    let get_row_set (board: int[][]) (r: int) =
        board[r] |> Set.ofArray

    let get_all_sets (board: int[][]): Set<int>[] =
        let row_count = Array.length board
        let col_count = Array.length board[0]
        let row_sets = { 0 .. row_count-1 } |> Seq.map (get_row_set board)
        let col_sets = { 0 .. col_count-1 } |> Seq.map (get_column_set board)

        Seq.append row_sets col_sets |> Array.ofSeq

    let score (board: int[][]) (called_numbers: Set<int>) =
        board
        |> Seq.ofArray
        |> Seq.collect Seq.ofArray
        |> Seq.filter (fun n -> not (Set.contains n called_numbers))
        |> Seq.sum

module GameState =

    let _create_number_set_reverse_index (number_sets: Set<int>[][]): Map<int, list<(int * int * int)>> =
        let number_set_detail =
            { 0 .. (Array.length number_sets) - 1 } |> Seq.collect (fun board_id ->
                { 0 .. (Array.length number_sets[board_id]) - 1 } |> Seq.collect (fun ns_idx ->
                    number_sets[board_id][ns_idx] |> Seq.map (fun n ->
                        (n, board_id, ns_idx)
                    )
                )
            )

        let r_index =
            number_set_detail
            |> Seq.groupBy (fun (n, board_id, ns_idx) -> n)
            |> Seq.map (fun (n, info) -> (n, List.ofSeq info))
            |> Map.ofSeq

        r_index

    let create (boards: int[][][]) = 
        let number_sets = boards |> Array.map Board.get_all_sets
        {
            boards = boards;
            number_sets = number_sets;
            number_set_reverse_index = _create_number_set_reverse_index number_sets;
            called_numbers = List.empty;
            winning_boards = Set.empty;
        }

    let _apply_called_number_on_number_set (gs: game_state) (n, board_id, ns_idx) =
        let remaining = Set.remove n (gs.number_sets[board_id][ns_idx])
        let new_num_sets =
            gs.number_sets |> Array.updateAt board_id (
                gs.number_sets[board_id] |> Array.updateAt ns_idx remaining
            ) 
        let new_winning_boards =
            if Set.isEmpty remaining then
                Set.add board_id gs.winning_boards
            else
                gs.winning_boards 
        { gs with number_sets = new_num_sets; winning_boards = new_winning_boards; }

    let call_number (gs: game_state) n =
        let ns_info_list = Map.tryFind n gs.number_set_reverse_index
        let gs = ns_info_list |> Option.fold (List.fold _apply_called_number_on_number_set) gs
        let gs = { gs with called_numbers = n :: gs.called_numbers }
        gs

module BingoGame =

    let get_all_win_events (game_states: seq<game_state>) =
        game_states
        |> Seq.pairwise
        |> Seq.filter (fun (gs_cur, gs_next) -> (Set.count gs_cur.winning_boards) <> (Set.count gs_next.winning_boards))
        |> Seq.map (fun (gs_cur, gs_next) -> (gs_next, gs_next.winning_boards - gs_cur.winning_boards))

    let play_all (boards: int[][][]) (numbers: int[]) =
        let gs = GameState.create boards
        let all_game_states =
            numbers
            |> Seq.ofArray
            |> Seq.scan GameState.call_number gs
        all_game_states

    let play (boards: int[][][]) (numbers: int[]) =
        play_all boards numbers
        |> Seq.tryFind (fun gs -> not (Set.isEmpty gs.winning_boards))

    let parse (lines: seq<string>) =
        let num_split_re = new Regex(@"[\s,]+")

        let parse_num_line line =
            num_split_re.Split(line)
            |> Array.filter (String.IsNullOrEmpty >> not)
            |> Array.map Convert.ToInt32

        let process_lines (items: int[] list list) (line: int[]) =
            if Array.isEmpty line then
                [] :: items
            else if List.isEmpty items then
                [[line]]
            else
                (line :: (List.head items)) :: (List.tail items)

        let n_lines =
            lines
            |> Seq.map parse_num_line
            |> Seq.fold process_lines List.empty
            |> List.ofSeq
            |> List.rev
            |> List.map (Array.ofList >> Array.rev)

        let numbers = List.head n_lines
        let boards = List.tail n_lines |> Array.ofList

        (numbers[0], boards)

    let load file_name =
        File.ReadLines file_name |> parse

let run_part1 () =
    let file_name = "../../../day_04_1.txt"
    let (numbers, boards) = BingoGame.load file_name
    let result = BingoGame.play boards numbers

    match result with
        | Some(gs) ->
            for winning_board in gs.winning_boards do
                let score = Board.score gs.boards[winning_board] (Set.ofList gs.called_numbers)
                let answer = score * gs.called_numbers.Head
                printfn $"Winning Board: {winning_board} - Score: {score}"
                printfn $"sAnswer: {answer}"
        | _ ->
            printfn $"No winner."
    1

(*
--- Part Two ---

On the other hand, it might be wise to try a different strategy: let the giant
squid win.

You aren't sure how many bingo boards a giant squid could play at once, so
rather than waste time counting its arms, the safe thing to do is to figure
out which board will win last and choose that one. That way, no matter which
boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after
13 is eventually called and its middle column is completely marked. If you were
to keep playing until this point, the second board would have a sum of unmarked
numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?
    
*)
let run_part2 () =
    let file_name = "../../../day_04_1.txt"
    let (numbers, boards) = BingoGame.load file_name
    let win_events = BingoGame.get_all_win_events (BingoGame.play_all boards numbers) |> Array.ofSeq

    let (last_gs, last_win) = Array.last win_events
    for winning_board in last_win do
        let score = Board.score (boards[winning_board]) (last_gs.called_numbers |> Set.ofList)
        let answer = score * last_gs.called_numbers.Head
        printfn $"Last Winning Board: {winning_board} -> Score: {score} -> Round: {List.length last_gs.called_numbers}"
        printfn $"Answer: {answer}"
    1