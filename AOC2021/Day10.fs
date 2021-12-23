module Day10

(*
--- Day 10: Syntax Scoring ---

You ask the submarine to determine the best route out of the deep-sea cave, but it only replies:

Syntax error in navigation subsystem on line: all of them

All of them?! The damage is worse than you thought. You bring up a copy of the navigation subsystem (your puzzle input).

The navigation subsystem syntax is made of several lines containing chunks. There are one or more chunks on each line, and chunks contain zero or more other chunks. Adjacent chunks are not separated by any delimiter; if one chunk stops, the next chunk (if any) can immediately start. Every chunk must open and close with one of four legal pairs of matching characters:

    If a chunk opens with (, it must close with ).
    If a chunk opens with [, it must close with ].
    If a chunk opens with {, it must close with }.
    If a chunk opens with <, it must close with >.

So, () is a legal chunk that contains no other chunks, as is []. More complex but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and even (((((((((()))))))))).

Some lines are incomplete, but others are corrupted. Find and discard the corrupted lines first.

A corrupted line is one where a chunk closes with the wrong character - that is, where the characters it opens and closes with do not form one of the four legal pairs listed above.

Examples of corrupted chunks include (], {()()()>, (((()))}, and <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its presence causes the whole line to be considered corrupted.

For example, consider the following navigation subsystem:

[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]

Some of the lines aren't corrupted, just incomplete; you can ignore these lines for now. The remaining five lines are corrupted:

    {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
    [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
    [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
    [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
    <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

Stop at the first incorrect closing character on each corrupted line.

Did you know that syntax checkers actually have contests to see who can get the high score for syntax errors in a file? It's true! To calculate the syntax error score for a line, take the first illegal character on the line and look it up in the following table:

    ): 3 points.
    ]: 57 points.
    }: 1197 points.
    >: 25137 points.

In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal ] was found once (57 points), an illegal } was found once (1197 points), and an illegal > was found once (25137 points). So, the total syntax error score for this file is 6+57+1197+25137 = 26397 points!

Find the first illegal character in each corrupted line of the navigation subsystem. What is the total syntax error score for those errors?

*)

open System
open System.IO

type chunk_type = {
    opener: char;
    closer: char;
    score: int;
}

type chunk_type_config = {
    chunk_types: chunk_type list;
    opener_index: Map<char, chunk_type>;
    closer_index: Map<char, chunk_type>;
}

module ChunkType =
    let create (opener, closer, score) =
        { opener = opener; closer = closer; score = score; } : chunk_type

module ChunkTypeConfig =
    let create (chunk_types: (char * char * int) list) =
        let cts = List.map ChunkType.create chunk_types

        let o_index =
            cts
            |> Seq.map (fun ct -> (ct.opener, ct))
            |> Map.ofSeq

        let c_index =
            cts
            |> Seq.map (fun ct -> (ct.closer, ct))
            |> Map.ofSeq

        { chunk_types = cts; opener_index = o_index; closer_index = c_index; }

    let get_chunk_type c config =
        Map.tryFind c config.opener_index
        |> Option.orElseWith (fun () -> Map.tryFind c config.closer_index)

    let get_score c config =
        (get_chunk_type c config |> Option.get).score

    let is_opener c config =
        Map.containsKey c config.opener_index

    let is_closer c config =
        Map.containsKey c config.closer_index

    let get_expected_opener c config =
        Map.tryFind c config.closer_index
        |> Option.map (fun ct -> ct.opener)

    let validate (line: string) config =
        let process_item (stack, errors) c =
            if is_opener c config then
                (c :: stack, errors)
            elif is_closer c config then
                let expected = get_expected_opener c config |> Option.get
                if List.isEmpty stack then
                    (stack, c :: errors)
                elif (List.head stack) <> expected then
                    (stack, c :: errors)
                else
                    (List.tail stack, errors)
            else
                // unknown character
                (stack, c :: errors)

        let (remaining_open, errors) =
            line.ToCharArray()
            |> Seq.fold process_item ([], [])
        
        // What is needed to complete.
        let completion =
            remaining_open
            |> List.map (fun c -> (Map.find c config.opener_index).closer)

        (errors, completion)

    let standard_config =
        create [
            ('(', ')', 3);
            ('[', ']', 57);
            ('{', '}', 1197);
            ('<', '>', 25137);
        ]

let read_lines file_name =
    File.ReadLines file_name |> Array.ofSeq

let run_part1() =
    let file_name = "../../../day_10_1.txt"
    let lines = read_lines file_name
    
    let validations =
        lines
        |> Seq.map (fun line -> ChunkTypeConfig.validate line ChunkTypeConfig.standard_config)

    let answer =
        validations
        |> Seq.map (fun (errors, completion) ->
            if List.isEmpty errors then
                0
            else
                ChunkTypeConfig.get_score (List.last errors) ChunkTypeConfig.standard_config
        )
        |> Seq.sum

    printfn $"Answer: {answer}"

(*
--- Part Two ---

Now, discard the corrupted lines. The remaining lines are incomplete.

Incomplete lines don't have any incorrect characters - instead, they're missing some closing characters at the end of the line. To repair the navigation subsystem, you just need to figure out the sequence of closing characters that complete all open chunks in the line.

You can only use closing characters (), ], }, or >), and you must add them in the correct order so that only legal pairs are formed and all chunks end up closed.

In the example above, there are five incomplete lines:

    [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
    [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
    (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
    {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
    <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.

Did you know that autocomplete tools also have contests? It's true! The score is determined by considering the completion string character-by-character. Start with a total score of 0. Then, for each character, multiply the total score by 5 and then increase the total score by the point value given for the character in the following table:

    ): 1 point.
    ]: 2 points.
    }: 3 points.
    >: 4 points.

So, the last completion string above - ])}> - would be scored as follows:

    Start with a total score of 0.
    Multiply the total score by 5 to get 0, then add the value of ] (2) to get a new total score of 2.
    Multiply the total score by 5 to get 10, then add the value of ) (1) to get a new total score of 11.
    Multiply the total score by 5 to get 55, then add the value of } (3) to get a new total score of 58.
    Multiply the total score by 5 to get 290, then add the value of > (4) to get a new total score of 294.

The five lines' completion strings have total scores as follows:

    }}]])})] - 288957 total points.
    )}>]}) - 5566 total points.
    }}>}>)))) - 1480781 total points.
    ]]}}]}]}> - 995444 total points.
    ])}> - 294 total points.

Autocomplete tools are an odd bunch: the winner is found by sorting all of the scores and then taking the middle score. (There will always be an odd number of scores to consider.) In this example, the middle score is 288957 because there are the same number of scores smaller and larger than it.

Find the completion string for each incomplete line, score the completion strings, and sort the scores. What is the middle score?

*)

let run_part2() =
    let file_name = "../../../day_10_1.txt"
    let lines = read_lines file_name
    
    let completion_scores = Map[
        ')', 1;
        ']', 2;
        '}', 3;
        '>', 4;
    ]

    let score_completion_char (score: bigint) (c: char) =
        score * (bigint 5) + bigint(Map.find c completion_scores)

    let score_remaining (completion: char list) =
        completion
        |> List.fold score_completion_char 0

    let validations =
        lines
        |> Seq.map (fun line -> ChunkTypeConfig.validate line ChunkTypeConfig.standard_config)

    let scores =
        validations
        |> Seq.filter (fun (errors, completion) -> List.isEmpty errors)
        |> Seq.map (fun (errors, completion) -> score_remaining completion)
        |> Array.ofSeq
        |> Array.sort

    let answer = Array.get scores ((Array.length scores) / 2)

    printfn $"Answer: {answer}"