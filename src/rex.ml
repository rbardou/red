(* Regular expression engine. *)

(* We execute several "threads" in parallel, which all read the input
   at the same pace (when we read one input, all threads execute until
   a "Match" instruction is executed on this input).

   If two threads are in the same state, we can merge them, because
   they will always read the input in the same way from this state.
   So we only keep one thread in each state.

   But there are two difficulties:
   - epsilon-transitions;
   - semantic actions.

   For epsilon-transitions, we can't just test whether there is a
   thread which is in the same state already. We need to test whether
   a thread *has already been* in the same state during the current
   input.

   For semantic actions, when we merge two threads into one, we need
   to make sure that only the semantic actions of one of the threads
   are executed. So we can only execute semantic actions if there is
   only one thread which is running; otherwise, each thread has to
   carry a list of pending actions to execute once the thread becomes
   the only one.

   Also, we need to choose which semantic actions to execute when
   merging two threads with different semantic actions. To this end,
   each thread has a priority (encoded by simply being first in the
   list of threads). Priorities allow to encode things like greedy
   vs. non-greedy operators or branch priorities. *)

(* End of line is not represented as "\n" in case we want to handle "\r\n" for instance. *)

type 'position action = 'position -> unit

(* Absolute address. *)
type address = int

(* Mark indexes. *)
type mark = int

(* We don't care about overflows in [compare], we won't manipulate big numbers. *)
module Int = struct type t = int let compare = (-) end
module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

(* Offset to add to current instruction address. *)
type relative_address = int

type 'result instruction =
  (* Matching characters. *)
  | Match_one of Character.t
  | Match_set of Character.Set.t
  | Match_any_character
  | Match_any (* includes end of file and end of line *)
  | Match_not_one of Character.t
  | Match_not_set of Character.Set.t
  | Match_eol (* end of line *)
  | Match_eof (* end of file *)
  | Match_eol_or_eof (* end of line or end of file *)

  (* Epsilon-transitions. *)
  | Jump of relative_address (* continue somewhere else *)
  | Fork of relative_address * relative_address (* replace by two threads, first one with higher priority *)
  | Fail (* stop current thread *)
  | Mark of mark (* save current position in a mark *)
  | Accept of 'result

type 'result program = 'result instruction array

(**** Program Constructors and Combinators ****)

module Make =
struct

  let one character: _ program = [| Match_one character |]
  let set set: _ program = [| Match_set set |]
  let any_character (): _ program = [| Match_any_character |]
  let any (): _ program = [| Match_any |]
  let not_one character: _ program = [| Match_not_one character |]
  let not_set set: _ program = [| Match_not_set set |]
  let eol (): _ program = [| Match_eol |]
  let eof (): _ program = [| Match_eof |]
  let eol_or_eof (): _ program = [| Match_eol_or_eof |]
  let jump offset: _ program = [| Jump offset |]
  let fork offset_1 offset_2: _ program = [| Fork (offset_1, offset_2) |]
  let fail (): _ program = [| Fail |]
  let mark mark: _ program = [| Mark mark |]
  let accept (type r) (result: r): r program = [| Accept result |]

  let range (a: char) (b: char): _ program = set (Character.Set.ascii_range a b)

  let seq (type r) (a: r program) (b: r program) = Array.append a b
  let seql (type r) (l: r program list) = Array.concat l

  (* Match a sequence of characters, from a UTF-8 encoded string. *)
  let utf8 (s: string): _ program =
    Utf8.split_runes s
    |> List.map (fun c -> Match_one c)
    |> Array.of_list

  let branch (type r) (a: r program) (b: r program): r program =
    seql [
      fork 1 (Array.length a + 2);
      a;
      jump (Array.length b + 1);
      b;
    ]

  let branchl (type r) (l: r program list): r program =
    match l with
      | [] ->
          fail ()
      | [ branch ] ->
          branch
      | head :: tail ->
          List.fold_left branch head tail

  (* Match zero or one instance. *)
  (* Greedy, i.e. if matching the option is unnecessary but possible, match it. *)
  let option (type r) (a: r program): r program =
    seql [
      fork 1 (Array.length a + 1);
      a;
    ]

  (* Match zero or one instance. *)
  (* Non-greedy, i.e. if matching the option is unnecessary, do not match it. *)
  let option_ng (type r) (a: r program): r program =
    seql [
      fork (Array.length a + 1) 1;
      a;
    ]

  (* Match several instances, or none. *)
  (* Greedy, i.e. match as many instances as possible. *)
  let star (type r) (a: r program): r program =
    let la = Array.length a in
    seql [
      fork 1 (la + 1);
      a;
      fork (- la) 1;
    ]

  (* Match several instances, or none. *)
  (* Non-greedy, i.e. match as few instances as possible. *)
  let star_ng (type r) (a: r program): r program =
    let la = Array.length a in
    seql [
      fork (la + 1) 1;
      a;
      fork 1 (- la);
    ]

  (* Match several instances, at least one. *)
  (* Greedy, i.e. match as many instances as possible. *)
  let plus (type r) (a: r program): r program =
    let la = Array.length a in
    seql [
      a;
      fork (- la) 1;
    ]

  (* Match several instances, at least one. *)
  (* Non-greedy, i.e. match as few instances as possible (but always at least one). *)
  let plus_ng (type r) (a: r program): r program =
    let la = Array.length a in
    seql [
      a;
      fork 1 (- la);
    ]

end

(**** Program Execution ****)

type 'position marks = 'position Int_map.t

type ('position, 'result) thread =
  {
    marks: 'position marks;
    next: address;
    accept: 'result option;
  }

type ('position, 'result) state =
  {
    program: 'result program;
    threads: ('position, 'result) thread list;
  }

let start program =
  if Array.length program > 0 then
    {
      program;
      threads = [ { marks = Int_map.empty; next = 0; accept = None } ];
    }
  else
    {
      program;
      threads = [];
    }

type input =
  | Character of Character.t
  | Eol
  | Eof

type ('position, 'result) status =
  | Failed
  | Pending
  | Matched of { result: 'result; marks: 'position marks }

(* Test whether a regular expression is done. *)
let status (type p) (type r) (state: (p, r) state): (p, r) status =
  match state.threads with
    | [] ->
        Failed
    | { accept = None } :: _ ->
        Pending
    | { accept = Some result; marks } :: _ ->
        Matched { result; marks }

(* Read one input.
   Return the new state and the list of actions that can be executed right now.
   These actions will not be returned again for next inputs. *)
let feed (type p) (type r) (state: (p, r) state) (position: p) (input: input): (p, r) state =

  (* Set of addresses that we should not execute immediately again because another thread
     at this address was already executed. If it was executed before, it had higher
     priority: the new thread just disappears. *)
  let executed_addresses = ref Int_set.empty in

  (* Set of addresses that we should not execute in the next step because another thread
     at this address will already be executed in the next step, and thus has higher
     priority. *)
  let next_addresses = ref Int_set.empty in

  (* List of threads to execute in the next call to [feed], in reverse priority order. *)
  let next_threads = ref [] in

  (* Add a thread to [next_threads], so that it is executed in the next call to [feed],
     but only if no other thread has been added with the same address already. *)
  let spawn (thread: (p, r) thread) =
    if not (Int_set.mem thread.next !next_addresses) then (
      next_addresses := Int_set.add thread.next !next_addresses;
      next_threads := thread :: !next_threads;
    )
  in

  let rec execute (thread: (p, r) thread) =
    (* Only execute if the thread address has not been already executed. *)
    if not (Int_set.mem thread.next !executed_addresses) then (
      executed_addresses := Int_set.add thread.next !executed_addresses;

      match state.program.(thread.next) with
        | Match_one character ->
            (
              match input with
                | Character input when Character.equals input character ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_set set ->
            (
              match input with
                | Character input when Character.Set.mem input set ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_any_character ->
            (
              match input with
                | Character _ ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_any ->
            spawn { thread with next = thread.next + 1 }
        | Match_not_one character ->
            (
              match input with
                | Character input when not (Character.equals input character) ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_not_set set ->
            (
              match input with
                | Character input when not (Character.Set.mem input set) ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_eol ->
            (
              match input with
                | Eol ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_eof ->
            (
              match input with
                | Eof ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Match_eol_or_eof ->
            (
              match input with
                | Eol | Eof ->
                    spawn { thread with next = thread.next + 1 }
                | _ ->
                    ()
            )
        | Jump offset ->
            execute { thread with next = thread.next + offset }
        | Fork (offset_1, offset_2) ->
            execute { thread with next = thread.next + offset_1 };
            execute { thread with next = thread.next + offset_2 }
        | Fail ->
            ()
        | Mark mark ->
            let marks = Int_map.add mark position thread.marks in
            execute { thread with next = thread.next + 1; marks }
        | Accept result ->
            (* Thread will loop forever in this accept state. *)
            spawn { thread with accept = Some result }
    )
  in

  List.iter execute state.threads;
  { state with threads = List.rev !next_threads }

(**** Test ****)

(* let () = *)
(*   let rex = *)
(*     Make.( *)
(*       branchl [ *)
(*         seql [ *)
(*           plus (range 'a' 'z'); *)
(*           plus (range '0' '9'); *)
(*           mark 0; *)
(*           accept `identifier_and_integer; *)
(*         ]; *)
(*         seql [ *)
(*           plus (range 'a' 'z'); *)
(*           mark 0; *)
(*           accept `identifier; *)
(*         ]; *)
(*         seql [ *)
(*           plus (range '0' '9'); *)
(*           mark 0; *)
(*           accept `integer; *)
(*         ]; *)
(*         seql [ *)
(*           plus (one " "); *)
(*           mark 0; *)
(*           accept `blank; *)
(*         ]; *)
(*       ] *)
(*     ) *)
(*   in *)
(*   let state = start rex in *)
(*   let state = feed state 0 (Character "a") in *)
(*   let state = feed state 1 (Character "b") in *)
(*   let state = feed state 2 (Character "c") in *)
(*   (\* let state = feed state 3 (Character " ") in *\) *)
(*   (\* let state = feed state 4 (Character "1") in *\) *)
(*   (\* let state = feed state 5 (Character "2") in *\) *)
(*   let state = feed state 6 Eof in *)
(*   ( *)
(*     match status state with *)
(*       | Failed -> print_endline "Failed" *)
(*       | Pending -> print_endline "Pending" *)
(*       | Matched { result; marks } -> *)
(*           match result with *)
(*             | `identifier -> Printf.printf "identifier (%d)\n" (Int_map.find 0 marks) *)
(*             | `identifier_and_integer -> Printf.printf "identifier_and_integer (%d)\n" (Int_map.find 0 marks) *)
(*             | `integer -> Printf.printf "integer (%d)\n" (Int_map.find 0 marks) *)
(*             | `blank -> Printf.printf "blank (%d)\n" (Int_map.find 0 marks) *)
(*   ); *)
(*   exit 0 *)
