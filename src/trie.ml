type word = Character.t list

type t =
  {
    (* Number of times the word from the root to this node appears. *)
    count: int;
    (* Longest word in this tree. *)
    longest: int;
    children: t Character.Map.t;
  }

let empty = { count = 0; longest = 0; children = Character.Map.empty }

let is_empty (trie: t) =
  trie.count = 0 && Character.Map.is_empty trie.children

let make count children =
  let longest = Character.Map.fold (fun _ child acc -> max child.longest acc) children 0 + 1 in
  {
    count;
    longest;
    children;
  }

let rec add (word: word) (trie: t): t =
  match word with
    | [] ->
        make (trie.count + 1) trie.children
    | head :: tail ->
        let child =
          match Character.Map.find head trie.children with
            | exception Not_found ->
                empty
            | child ->
                child
        in
        let child = add tail child in
        make trie.count (Character.Map.add head child trie.children)

let rec remove (word: word) (trie: t): t =
  match word with
    | [] ->
        make (max 0 (trie.count - 1)) trie.children
    | head :: tail ->
        match Character.Map.find head trie.children with
          | exception Not_found ->
              trie
          | child ->
              let child = remove tail child in
              if is_empty child then
                make trie.count (Character.Map.remove head trie.children)
              else
                make trie.count (Character.Map.add head child trie.children)

let rec get (word: word) (trie: t): t =
  match word with
    | [] ->
        trie
    | head :: tail ->
        match Character.Map.find head trie.children with
          | exception Not_found ->
              empty
          | child ->
              get tail child

let get_count (word: word) (trie: t) =
  (get word trie).count

let foreach ?(prefix = []) (trie: t) (f: word -> int -> unit) =
  let rec iter prefix_rev trie =
    if trie.count > 0 then f (List.rev prefix_rev) trie.count;
    let iter_child character child =
      iter (character :: prefix_rev) child
    in
    Character.Map.iter iter_child trie.children
  in
  iter (List.rev prefix) trie

let to_list (trie: t): (word * int) list =
  let result = ref [] in
  foreach trie (fun word count -> result := (word, count) :: !result);
  !result

let best_for_autocompletion (trie: t): word =
  let rec get acc trie =
    let longest_child =
      let get_best character child acc =
        match acc with
          | None ->
              Some (character, child)
          | Some (_, previous) ->
              if child.longest > previous.longest then
                Some (character, child)
              else
                acc
      in
      Character.Map.fold get_best trie.children None
    in
    match longest_child with
      | None ->
          List.rev acc
      | Some (character, child) ->
          get (character :: acc) child
  in
  get [] trie
