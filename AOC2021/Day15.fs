module Day15

(*
--- Day 15: Chiton ---

You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581

The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?

*)

open System
open System.IO

open FSharpx.Collections

let load_file file_name =
    File.ReadLines file_name
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))
    |> Array.ofSeq

let adj_vectors = [
    (0, 1);
    (-1, 0); (1, 0);
    (0, -1);
]

let find_path (m: int[][]) =
    let height = Array.length m
    let width = Array.length m[0]
    let end_pos = (width - 1, height - 1)

    let get_adjacent (x, y) cur_path cur_score (scores_to: Map<(int * int), int>) =
        adj_vectors
        |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
        |> Seq.filter (fun (nx, ny) ->
            nx >= 0 && nx < width &&
            ny >= 0 && ny < height
        )
        |> Seq.filter (fun p -> not(Map.containsKey p scores_to))
        |> Seq.map (fun (nx, ny) ->
            let cost = m[ny][nx]
            (cur_score + cost, (nx, ny) :: cur_path)
        )
        |> List.ofSeq

    let rec _find_path (paths: Heap<int * list<int * int>>) (scores_to: Map<(int * int), int>) =
        let (cur_score, cur_path) = Heap.head paths
        let cur_pos = List.head cur_path
        let p_score = Map.tryFind cur_pos scores_to

        if Option.isSome p_score && (Option.get p_score) <= cur_score then
            // Skip this node
            _find_path (Heap.tail paths) scores_to
        elif cur_pos = end_pos then
            (cur_score, cur_path)
        else
            let adj_pos = get_adjacent cur_pos cur_path cur_score scores_to
            let new_paths = List.fold (fun paths p -> Heap.insert p paths) paths adj_pos
            _find_path new_paths (Map.add cur_pos cur_score scores_to)
    
    let initial_paths = (Heap.insert (0, [(0, 0)]) (Heap.empty false))
    _find_path initial_paths Map.empty

let run_part1() =
    let file_name = "../../../inputs/day_15_1.txt"
    let m = load_file file_name

    let (score, path) = find_path m
    printfn $"score: {score}"

(*
--- Part Two ---

Now that you know how to find low-risk paths in the cave, you can try to find your way out.

The entire cave is actually five times larger in both dimensions than you thought; the area you originally scanned is just one tile in a 5x5 tile area that forms the full map. Your original map tile repeats to the right and downward; each time the tile repeats to the right or downward, all of its risk levels are 1 higher than the tile immediately up or left of it. However, risk levels above 9 wrap back around to 1. So, if your original map had some position with a risk level of 8, then that same position on each of the 25 total tiles would be as follows:

8 9 1 2 3
9 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7

Each single digit above corresponds to the example position with a value of 8 on the top-left tile. Because the full map is actually five times larger in both dimensions, that position appears a total of 25 times, once in each duplicated tile, with the values shown above.

Here is the full five-times-as-large version of the first example above, with the original map in the top left corner highlighted:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479

Equipped with the full map, you can now find a path from the top left corner to the bottom right corner with the lowest total risk:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479

The total risk of this path is 315 (the starting position is still never entered, so its risk is not counted).

Using the full map, what is the lowest total risk of any path from the top left to the bottom right?

*)

let all_positions (height: int) (width: int) =
    { 0 .. height - 1} |> Seq.collect (fun y ->
        { 0 .. width - 1 } |> Seq.map (fun x -> (y, x))
    )

let to_sparse (m: int[][]) =
    all_positions (Array.length m) (Array.length m[0])
    |> Seq.map (fun (y, x) -> ((y, x), m[y][x]))
    |> Map.ofSeq

let from_sparse (height: int) (width: int) (sm: Map<int * int, int>): int[][] =
    { 0 .. height - 1}
    |> Seq.map (fun y ->
        { 0 .. width - 1}
        |> Seq.map (fun x -> Map.find (y, x) sm)
        |> Array.ofSeq
    )
    |> Array.ofSeq

let expand_map (m: int[][]) =
    let sm = to_sparse m
    let width = Array.length m[0]
    let height = Array.length m
    let x_width = width * 5
    let x_height = height * 5

    // This is a case where the mutable version to expand the map
    // would be much simpler.

    // Gets a value with updated cache
    let rec get_value (sm: Map<int * int, int>) y x: int * Map<int * int, int> =
        match Map.tryFind (y, x) sm with
            | Some(v) -> (v, sm)
            | None ->
                let ref_pos = [
                    (y, x - width);
                    (y - height, x);
                ]
                let (max_ref, sm) =
                    ref_pos
                    |> Seq.filter (fun (ny, nx) -> nx >= 0 && ny >= 0)
                    |> Seq.fold (fun (max_v, sm) (ny, nx) ->
                        let (ref_v, sm) = get_value sm ny nx
                        (Math.Max(max_v, ref_v), sm)
                    ) (0, sm)
                let v = max_ref + 1
                let v = if v > 9 then 1 else v
                let sm = Map.add (y, x) v sm
                (v, sm)

    // Fill out all positions
    let sm =
        all_positions x_height x_width
        |> Seq.fold (fun sm (y, x) -> get_value sm y x |> snd) sm

    // Back to 2D array
    let xm = from_sparse x_height x_width sm

    xm

let run_part2() =
    let file_name = "../../../inputs/day_15_1.txt"
    let m = load_file file_name
    let xm = expand_map m

    let (score, path) = find_path xm
    printfn $"score: {score}"