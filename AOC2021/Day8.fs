module Day8

(*
--- Day 8: Seven Segment Search ---

You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.

As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.

The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)

So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.

For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.

For example, here is what you might see in a single entry in your notes:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf

(The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)

Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.

Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.

For now, focus on the easy digits. Consider this larger example:

be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
fgae cfgab fg bagce

Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).

In the output values, how many times do digits 1, 4, 7, or 8 appear?

*)

open System
open System.IO

let sort_characters (s: string) =
    new string(s.ToCharArray() |> Array.sort)

(*
  I implemted finding the mappings for wiring by creating a signature
  of a wiring configuration which is not based on a wire id.
  A wire is given a description which is the sorted list of wires needed for
  each digit the wire is part of. The digits are then given a signature which
  is all of the wire signatures sorted.

  This signature ends up being unique for each digit. Take an 2 wiring configuration
  and convert to the signatures and join by the signatures to get a mapping.

*)

let get_wiring_signatures (wirings: string[]) =
    wirings
    |> Seq.collect (fun wiring ->
        wiring |> Seq.map (fun w -> (w, wiring.Length))
    )
    |> Seq.groupBy (fun (w, count) -> w)
    |> Seq.map (fun (w, counts) ->
        let c_sig =
            counts
            |> Seq.map snd
            |> List.ofSeq
            |> List.sort
            |> List.map string
            |> (fun items -> System.String.Join(',', items))
        (w, c_sig)
    )
    |> Map.ofSeq

let get_digit_signatures_from_wiring_signatures (w_sigs: Map<char, string>) (wirings: string[]) =
    wirings
    |> Array.map (fun wiring ->
        wiring.ToCharArray()
        |> Seq.map (fun w -> w_sigs[w])
        |> List.ofSeq
        |> List.sort
        |> (fun items -> System.String.Join(';', items))
    )

let get_digit_signatures (wirings: string[]) =
    get_digit_signatures_from_wiring_signatures (get_wiring_signatures wirings) wirings

let create_digit_mapping (wiring: string[]) (digit_signatures: string[]) (new_wiring: string[]) =
    let nw_digit_signatures = get_digit_signatures new_wiring
    let nw_wiring_sorted = new_wiring |> Array.map sort_characters
    let n_sig_to_n_wiring = Seq.zip nw_digit_signatures nw_wiring_sorted |> Map.ofSeq

    let mappings =
        Seq.zip3
            (digit_signatures |> Array.map (fun _sig -> n_sig_to_n_wiring[_sig]))
            wiring
            (seq { 0 .. (Array.length wiring - 1) })
        |> Seq.map (fun (n_wiring, wiring, n) -> (n_wiring, (n_wiring, wiring, n)))
        |> Map.ofSeq

    mappings

let get_output_values (wiring: string[]) (n_wiring: string[]) (output: string[]) =
    let sigs = get_digit_signatures wiring
    let n_sigs = get_digit_signatures n_wiring
    let n_lookup = create_digit_mapping wiring sigs n_wiring

    let find_mapping (n_wiring: string) =
        Map.find (sort_characters n_wiring) n_lookup

    output |> Array.map find_mapping

let parse_line (line: string) =
    let info =
        line.Split('|')
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map (fun s ->
            s.Split(' ')
            |> Array.map (fun s -> new string(s.Trim().ToCharArray() |> Array.sort))
        )
        |> Array.ofSeq
    (info[0], info[1])

let parse_file file_name =
    File.ReadLines file_name
    |> Seq.map parse_line
    |> Array.ofSeq

let digit_standard_wiring = [|
    "abcefg";
    "cf";
    "acdeg";
    "acdfg";
    "bcdf";
    "abdfg";
    "abdefg";
    "acf";
    "abcdefg";
    "abcdfg"; 
|]

let run_part1() = 
    printfn $"Day 8 Part 1"

    let file_name = "../../../day_08_1.txt"
    let wiring_data = parse_file file_name

    let n_counts =
        wiring_data
        |> Seq.collect (fun (n_wiring, output) -> get_output_values digit_standard_wiring n_wiring output)
        |> Seq.map (fun (w, nw, n) -> n)
        |> Seq.countBy id
        |> Map.ofSeq

    let answer =
        [1; 4; 7; 8;]
        |> List.map (fun n -> n_counts[n])
        |> List.sum

    printfn $"    Answer: {answer}"

(*
--- Part Two ---

Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf

After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc

So, the unique signal patterns would correspond to the following digits:

    acedgfb: 8
    cdfbe: 5
    gcdfa: 2
    fbcad: 3
    dab: 7
    cefabd: 9
    cdfgeb: 6
    eafb: 4
    cagedb: 0
    ab: 1

Then, the four digits of the output value can be decoded:

    cdfeb: 5
    fcadb: 3
    cdfeb: 5
    cdbaf: 3

Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

    fdgacbe cefdb cefbgd gcbe: 8394
    fcgedb cgb dgebacf gc: 9781
    cg cg fdcagb cbg: 1197
    efabcd cedba gadfec cb: 9361
    gecf egdcabf bgf bfgea: 4873
    gebdcfa ecba ca fadegcb: 8418
    cefg dcbef fcge gbcadfe: 4548
    ed bcgafe cdgba cbgef: 1625
    gbdfcae bgc cg cgb: 8717
    fgae cfgab fg bagce: 4315

Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?

*)

let run_part2() = 
    printfn $"Day 8 Part 2"

    let file_name = "../../../day_08_1.txt"
    let wiring_data = parse_file file_name

    let digits_to_num (digits: int[]) = 
        Array.fold (fun t n -> t * 10 + n) 0 digits

    let outputs =
        wiring_data
        |> Seq.map (fun (n_wiring, output) -> get_output_values digit_standard_wiring n_wiring output)
        |> Seq.map (Array.map (fun (nw, w, n) -> n))
        |> Seq.map digits_to_num
        |> Array.ofSeq

    let answer = Array.sum outputs

    printfn $"    Answer: {answer}"

let run() =
    run_part1() |> ignore
    run_part2() |> ignore


