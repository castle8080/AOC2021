module Day14

(*
--- Day 14: Extended Polymerization ---

The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.

The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.

For example:

NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C

The first line is the polymer template - this is the starting point of the process.

The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.

So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:

    The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
    The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
    The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.

Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.

After the first step of this process, the polymer becomes NCNBCHB.

Here are the results of a few steps using the above rules:

Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.

Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

*)

open System
open System.IO
open System.Text.RegularExpressions

let seq_infinite (item: 'a) (f: 'a -> 'a): seq<'a> =
    Seq.append (Seq.singleton item) (Seq.unfold (fun item ->
        let next_item = f item
        Some(next_item, next_item)
    ) item)

let re_template = new Regex("^[A-Za-z]+$")
let re_mapping = new Regex("([A-Za-z])([A-Za-z])\s+->\s+([A-Za-z])")

let apply_mappings (mappings: Map<(char * char), char>) (text: string) =
    let with_mapped =
        text.ToCharArray()
        |> Seq.pairwise
        |> Seq.collect (fun (c1, c2) ->
            match (Map.tryFind (c1, c2) mappings) with
                | Some(mc) -> [c1; mc]
                | None -> [c1]
        )
    new string(Seq.append with_mapped [text.Chars (text.Length - 1)] |> Array.ofSeq)

let read_file file_name =
    
    let apply_line ((template: string), (mappings: Map<(char * char), char>))  (line: string) =
        if line = "" then
            (template, mappings)
        else
            let mr_template = re_template.Match line
            if mr_template.Success then
                (line, mappings)
            else
                let mr_mapping = re_mapping.Match line
                if mr_mapping.Success then
                    let c1 = mr_mapping.Groups[1].Value.Chars 0
                    let c2 = mr_mapping.Groups[2].Value.Chars 0
                    let c_ins = mr_mapping.Groups[3].Value.Chars 0
                    (template, Map.add (c1, c2) c_ins mappings)
                else
                    (template, mappings)

    File.ReadLines file_name |> Seq.fold apply_line ("", Map.empty)

let run_part1() =
    let file_name = "../../../inputs/day_14_1.txt"
    let (template, mappings) = read_file file_name

    let all_templates = seq_infinite template (apply_mappings mappings)
    let final_template = all_templates |> Seq.take 11 |> Seq.last

    let char_counts =
        final_template.ToCharArray()
        |> Seq.countBy id
        |> Array.ofSeq
        |> Array.sortBy snd

    let answer = (snd char_counts[Array.length char_counts - 1]) - (snd char_counts[0])
    printfn $"{answer}"
(*
--- Part Two ---

The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.

In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.

Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

*)

type count_calculation_cache = Map<(char * char * int), Map<char, int64>>

let seq_count_i64 (items: seq<'a>): Map<'a, int64> =
    Seq.fold (fun m item ->
        let c = Map.tryFind item m |> Option.defaultValue 0L
        Map.add item (c + 1L) m
    ) Map.empty items

let combine_counts (m1: Map<'a, int64>) (m2: Map<'a, int64>) =
    Seq.append (Map.keys m1) (Map.keys m2)
    |> Seq.map (fun k ->
        let r1 = Map.tryFind k m1 |> Option.defaultValue 0L
        let r2 = Map.tryFind k m2 |> Option.defaultValue 0L
        (k, r1 + r2)
    )
    |> Map.ofSeq

let rec calculate_pair_counts
    (cache: count_calculation_cache)
    (mappings: Map<(char * char), char>)
    (c1: char, c2: char, step: int) =

    match (Map.tryFind (c1, c2, step) cache) with
        | Some(r) ->
            (r, cache)
        | None ->
            let c_mid = Map.tryFind (c1, c2) mappings
            if step = 0 || Option.isNone c_mid then
                let r = [c1; c2] |> seq_count_i64
                let cache = Map.add (c1, c2, step) r cache
                (r, cache)
            else
                let c_mid = Option.get c_mid
                let (left_r, cache) = calculate_pair_counts cache mappings (c1, c_mid, step - 1)
                let (right_r, cache) = calculate_pair_counts cache mappings (c_mid, c2, step - 1)
                let r = combine_counts left_r right_r
                let r = Map.add c_mid ((Map.find c_mid r) - 1L) r
                let cache = Map.add (c1, c2, step) r cache
                (r, cache)

let calculate_counts (steps: int) (template: string) (mappings: Map<(char * char), char>) =
    
    let process_pair
        ((counts: Map<char, int64>), (cache: count_calculation_cache))
        (c1: char, c2: char) =
        let (r, cache) = calculate_pair_counts cache mappings (c1, c2, steps)
        let r = combine_counts counts r
        let r = Map.add c2 ((Map.find c2 r) - 1L) r
        (r, cache)
    
    if template.Length = 0 then
        Map.empty
    else
        let (r, cache) =
            template.ToCharArray()
            |> Array.pairwise
            |> Array.fold process_pair (Map.empty, Map.empty)

        let last_c = template.Chars (template.Length - 1)
        let r = Map.add last_c ((Map.find last_c r) + 1L) r
        r

let run_part2() =
    // This version of the code can also be used for part 1.
    let file_name = "../../../inputs/day_14_1.txt"
    let (template, mappings) = read_file file_name

    let char_counts =
        calculate_counts 40 template mappings
        |> Seq.map (fun pair -> (pair.Key, pair.Value))
        |> Array.ofSeq
        |> Array.sortBy snd

    let answer = (snd char_counts[Array.length char_counts - 1]) - (snd char_counts[0])
    printfn $"{answer}"
