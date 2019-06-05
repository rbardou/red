type word = Character.t list

type t =
  {
    (* Number of times the word from the root to this node appears. *)
    count: int;
    children: t Character.Map.t;
  }

let empty = { count = 0; children = Character.Map.empty }

let is_empty (trie: t) =
  trie.count = 0 && Character.Map.is_empty trie.children

let rec add (word: word) (trie: t): t =
  match word with
    | [] ->
        { trie with count = trie.count + 1 }
    | head :: tail ->
        let child =
          match Character.Map.find head trie.children with
            | exception Not_found ->
                empty
            | child ->
                child
        in
        let child = add tail child in
        { trie with children = Character.Map.add head child trie.children }

let rec remove (word: word) (trie: t): t =
  match word with
    | [] ->
        { trie with count = max 0 (trie.count - 1) }
    | head :: tail ->
        match Character.Map.find head trie.children with
          | exception Not_found ->
              trie
          | child ->
              let child = remove tail child in
              if is_empty child then
                { trie with children = Character.Map.remove head trie.children }
              else
                { trie with children = Character.Map.add head child trie.children }

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
    (* For now, we just take *a* word.
       TODO: is it a good idea to take the longest word? or the most used word? *)
    match Character.Map.min_binding trie.children with
      | exception Not_found ->
          List.rev acc
      | character, child ->
          get (character :: acc) child
  in
  get [] trie
