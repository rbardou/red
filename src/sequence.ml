type color = Red | Black | Double_black

(* Red-black tree.

   The [int] is the number of values stored in the tree, so that we can
   find a value given its index.

   TODO: it might be simpler to have the color be encoded in the edge
   instead of in the node, i.e. have [Node] take two colors, one for
   left and one for right. Root would implicitely be black. *)
type 'a t =
  | Leaf
  | Double_black_leaf
  | Node of color * int * 'a * 'a t * 'a t

let show_color color =
  match color with
    | Red -> "Red"
    | Black -> "Black"
    | Double_black -> "Double_black"

let rec show show_value sequence =
  match sequence with
    | Leaf ->
        "Leaf"
    | Double_black_leaf ->
        "Double_black_leaf"
    | Node (color, count, value, left, right) ->
        Printf.sprintf "Node (%s, %d, %s, %s, %s)"
          (show_color color) count (show_value value)
          (show show_value left) (show show_value right)

let rec pretty pretty_value fmt sequence =
  match sequence with
    | Leaf ->
        Format.fprintf fmt "Leaf"
    | Double_black_leaf ->
        Format.fprintf fmt "Double_black_leaf"
    | Node (color, count, value, left, right) ->
        Format.fprintf fmt "@[<hv>@[<hv 2>Node (@,%s,@ %d,@ %a,@ %a,@ %a@]@,)@]"
          (show_color color) count pretty_value value
          (pretty pretty_value) left (pretty pretty_value) right

let empty = Leaf

let is_empty sequence =
  match sequence with
    | Leaf -> true
    | Double_black_leaf -> true
    | Node (_, count, _, _, _) -> false

let is_not_empty sequence =
  not (is_empty sequence)

let count sequence =
  match sequence with
    | Leaf -> 0
    | Double_black_leaf -> 0
    | Node (_, count, _, _, _) -> count

let node color value left right =
  Node (color, count left + count right + 1, value, left, right)

let red value left right = node Red value left right
let black value left right = node Black value left right

let rec get index sequence =
  match sequence with
    | Leaf
    | Double_black_leaf ->
        (* Note: we're not supposed to [get] a [Double_black_leaf]. *)
        None
    | Node (_, _, value, left, right) ->
        let left_count = count left in
        if index = left_count then
          Some value
        else if index < left_count then
          get index left
        else
          get (index - left_count - 1) right

let rec set index value sequence =
  match sequence with
    | Leaf
    | Double_black_leaf ->
        (* Note: we're not supposed to [set] a [Double_black_leaf]. *)
        Leaf
    | Node (color, this_count, this_value, left, right) ->
        let left_count = count left in
        if index = left_count then
          Node (color, this_count, value, left, right)
        else if index < left_count then
          Node (color, this_count, this_value, set index value left, right)
        else
          let index = index - left_count - 1 in
          Node (color, this_count, this_value, left, set index value right)

let ensure_root_is_black sequence =
  match sequence with
    | Leaf
    | Double_black_leaf ->
        Leaf
    | Node (_, count, value, left, right) ->
        Node (Black, count, value, left, right)

let insert index value sequence =
  let balance color value left right =
    match color, value, left, right with
      | Black, z, Node (Red, _, y, Node (Red, _, x, a, b), c), d
      | Black, z, Node (Red, _, x, a, Node (Red, _, y, b, c)), d
      | Black, x, a, Node (Red, _, z, Node (Red, _, y, b, c), d)
      | Black, x, a, Node (Red, _, y, b, Node (Red, _, z, c, d)) ->
          red y (black x a b) (black z c d)
      | Black, x, a, b ->
          black x a b
      | Red, x, a, b ->
          red x a b
      | Double_black, _, _, _ ->
          (* Can only happen when removing values. *)
          assert false
  in

  let rec add index value sequence =
    match sequence with
      | Leaf ->
          Node (Red, 1, value, Leaf, Leaf)

      | Double_black_leaf ->
          (* Insertion does not create double-black nodes. *)
          assert false

      | Node (color, this_count, this_value, left, right) ->
          let left_count = count left in
          if index = left_count then
            balance
              color
              value
              left
              (add 0 this_value right)
          else if index < left_count then
            balance
              color
              this_value
              (add index value left)
              right
          else
            balance
              color
              this_value
              left
              (add (index - left_count - 1) value right)
  in

  (* Ensure that the root is black. *)
  ensure_root_is_black (add index value sequence)

let prepend value sequence =
  insert 0 value sequence

let append value sequence =
  insert (count sequence) value sequence

let one value =
  insert 0 value empty

let remove index sequence =
  let increase_blackness color =
    match color with
      | Red ->
          Black
      | Black ->
          Double_black
      | Double_black ->
          (* [sequence] is supposed to not have double-black nodes. *)
          assert false
  in

  (* Removing a node happens in two phases:
     - first we go down to extract a leaf and put it in place of
       the actual node to be removed, possibly introducing a double-black
       node (which just means that we need to rebalance the parent because
       this part of the tree, if the double-black were black, would have
       one less black height than its sibling);
     - then we rebalance as we go up, to remove the double-black node
       (which can propagate to the top). *)

  (* Given a node which was [color, value, left, right], knowing that
     a node was just removed from [left] or [right] (which thus can be
     double-black), make a node ensuring that there is no double-black.

     Only one of [left] or [right] can be double-black. *)
  let rec balance color value left right =
    (* Let's call "this" the double-black node, if any;
       let's call "sibling" its brother;
       and let's call "parent" their parent.

       Case 1: sibling is black, and has a red child; we cannot reduce
       sibling's blackness to red because it has a red child, but we can
       transform the tree to rebalance.

       Case 2: sibling is black, and has no red child; we can reduce
       blackness of this and sibling, and increase blackness of parent
       to compensate.

       Case 3: sibling is red; we cannot reduce its blackness, but it has
       two black children so we can transform the tree to obtain one of
       the previous cases. *)
    match left, right with
      | (Leaf | Node ((Black | Red), _, _, _, _)),
        (Leaf | Node ((Black | Red), _, _, _, _)) ->
          (* No double-black node: nothing to rebalance. *)
          node color value left right

(*
 color(value)
   /     \
DBL       B(rv)           =>                      color(rlv)
         /    \                                 /         \
      R(rlv)   rr                        B(value)          B(rv)
     /      \                           /        \        /     \
   rll      rlr                        L         rll     rlr    rr
*)
      | Double_black_leaf,
        Node (Black, _, rv, Node (Red, _, rlv, rll, rlr), rr) ->
          (* Case 1: transform. *)
          node color rlv (black value Leaf rll) (black rv rlr rr)

      | Node (Double_black, _, lv, ll, lr),
        Node (Black, _, rv, Node (Red, _, rlv, rll, rlr), rr) ->
          (* Case 1: transform. *)
          node color rlv (black value (black lv ll lr) rll) (black rv rlr rr)

(*
 color(value)                                color(rv)
   /     \                                    /     \
DBL       B(rv)           =>           B(value)    B(rrv)
         /    \                        /    \     /      \
       rl     R(rrv)                   L    rl   rrl     rrr
             /      \
           rrl      rrr
*)
      | Double_black_leaf,
        Node (Black, _, rv, rl, Node (Red, _, rrv, rrl, rrr)) ->
          (* Case 1: transform. *)
          node color rv (black value Leaf rl) (black rrv rrl rrr)

      | Node (Double_black, _, lv, ll, lr),
        Node (Black, _, rv, rl, Node (Red, _, rrv, rrl, rrr)) ->
          (* Case 1: transform. *)
          node color rv (black value (black lv ll lr) rl) (black rrv rrl rrr)

(*
             color(value)
               /     \
           B(lv)     DBL      =>            color(lv)
          /    \                             /   \
      R(llv)   lr                       B(llv)    B(value)
     /      \                           /   \       /   \
   lll      llr                       lll  llr     lr    L
*)
      | Node (Black, _, lv, Node (Red, _, llv, lll, llr), lr),
        Double_black_leaf ->
          (* Case 1: transform. *)
          node color lv (black llv lll llr) (black value lr Leaf)

      | Node (Black, _, lv, Node (Red, _, llv, lll, llr), lr),
        Node (Double_black, _, rv, rl, rr) ->
          (* Case 1: transform. *)
          node color lv (black llv lll llr) (black value lr (black rv rl rr))

(*
         color(value)
        /            \                     color(lrv)
      B(lv)          DBL       =>          /        \
     /     \                           B(lv)       B(value)
   ll      R(lrv)                     /     \       /     \
          /      \                   ll    lrl     lrr     L
         lrl    lrr
*)
      | Node (Black, _, lv, ll, Node (Red, _, lrv, lrl, lrr)),
        Double_black_leaf ->
          (* Case 1: transform. *)
          node color lrv (black lv ll lrl) (black value lrr Leaf)

      | Node (Black, _, lv, ll, Node (Red, _, lrv, lrl, lrr)),
        Node (Double_black, _, rv, rl, rr) ->
          (* Case 1: transform. *)
          node color lrv (black lv ll lrl) (black value lrr (black rv rl rr))

      | Double_black_leaf, Node (Black, rc, rv, rl, rr) ->
          (* Case 2: increase blackness of parent. *)
          node (increase_blackness color) value
            Leaf (Node (Red, rc, rv, rl, rr))

      | Node (Black, lc, lv, ll, lr), Double_black_leaf ->
          (* Case 2: increase blackness of parent. *)
          node (increase_blackness color) value
            (Node (Red, lc, lv, ll, lr)) Leaf

      | Node (Double_black, lc, lv, ll, lr), Node (Black, rc, rv, rl, rr) ->
          (* Case 2: increase blackness of parent. *)
          node (increase_blackness color) value
            (Node (Black, lc, lv, ll, lr)) (Node (Red, rc, rv, rl, rr))

      | Node (Black, lc, lv, ll, lr), Node (Double_black, rc, rv, rl, rr) ->
          (* Case 2: increase blackness of parent. *)
          node (increase_blackness color) value
            (Node (Red, lc, lv, ll, lr)) (Node (Black, rc, rv, rl, rr))

(*
       B(value)                       B(rv)
      /        \                     /     \
    DB(left)  R(rv)      =>    R(value)    rr
             /     \           /     \
            rl     rr      DB(left)   rl

color = Black because right child is red.
rl is black, so the subtree starting at R(value) is in case 1 or 2.
*)
      | (Double_black_leaf | Node (Double_black, _, _, _, _)),
        Node (Red, _, rv, rl, rr) ->
          (* Case 3: sibling is red, transform to reduce to previous case. *)
          (* Only case where the new left can be double-black is if it
             was recolored (i.e. case 2 was applied to it), in which
             case its root R(value) becomes black, not
             double-black. So new left cannot be double-black, and
             there is no need to balance left and rr. *)
          black rv (balance Red value left rl) rr

(*
         B(value)                       B(lv)
        /        \                     /     \
     R(lv)     DB(right)     =>       ll    R(value)
    /     \                                 /     \
   ll     lr                               lr    DB(right)
*)
      | Node (Red, _, lv, ll, lr),
        (Double_black_leaf | Node (Double_black, _, _, _, _)) ->
          (* Case 3: sibling is red, transform to reduce to previous case. *)
          black lv ll (balance Red value lr right)

      (* Cases where left and right have different black heights. *)
      | Node (Double_black, _, _, _, _), (Leaf | Double_black_leaf)
      | Double_black_leaf, (Leaf | Node (Double_black, _, _, _, _))
      | Leaf, (Double_black_leaf | Node (Double_black, _, _, _, _))
      (* Cases where left and right both have double black nodes. *)
      | Node (Double_black, _, _, _, _), Node (Double_black, _, _, _, _)
      | Double_black_leaf, Double_black_leaf
        -> assert false (* impossible *)
  in

  let increase_node_blackness sequence =
    match sequence with
      | Leaf ->
          Double_black_leaf
      | Double_black_leaf ->
          (* [sequence] is supposed to not have double-black nodes. *)
          assert false
      | Node (color, count, value, left, right) ->
          Node (increase_blackness color, count, value, left, right)
  in

  (* Remove and return removed value.
     Requires [index] to be valid in [sequence]. *)
  let rec extract index sequence =
    match sequence with
      | Leaf ->
          (* [index] is supposed to be valid in [sequence]. *)
          assert false
      | Double_black_leaf ->
          (* We did not modify [left] yet. *)
          assert false
      | Node (color, this_count, value, left, right) ->
          let left_count = count left in
          if index = left_count then
            match left with
              | Leaf ->
                  (
                    match color with
                      | Red ->
                          (* Removing a red node is free. *)
                          right, value
                      | Black ->
                          (* We'll need to rebalance. *)
                          increase_node_blackness right, value
                      | Double_black ->
                          (* We did not modify [left] yet. *)
                          assert false
                  )
              | Double_black_leaf ->
                  (* We did not modify [left] yet. *)
                  assert false
              | Node _ ->
                  (* Extract the right-most value from [left] to put
                     it in place of [value]. *)
                  let left, extracted_value = extract (index - 1) left in
                  balance color extracted_value left right, value
          else if index < left_count then
            let left, extracted_value =
              extract index left
            in
            balance color value left right, extracted_value
          else
            let right, extracted_value =
              extract (index - left_count - 1) right
            in
            balance color value left right, extracted_value
  in

  if index < 0 || index >= count sequence then
    sequence
  else
    let sequence, _ = extract index sequence in
    (* Sequence has no parent, so replacing double black by black is okay. *)
    ensure_root_is_black sequence

let of_array ?(ofs = 0) ?len array =
  (* [of_sub_array ofs len bd] builds a sequence of black depth [bd]
     (the black depth of a tree as the number of black nodes in a path from the root to a leaf)
     from the values of [array] from [ofs] to [ofs + len - 1].

     [len] must be > 0.
     If [len] = 2 ^ K - 1, then [bd] must be K or K + 1.
     If 2 ^ (K - 1) <= [len] < 2 ^ K - 1, then [bd] must be K. *)
  let rec of_sub_array ofs len bd =
    if len = 1 then
      (* [len] = 2 ^ 1 - 1, so [bd] is 1 or 2. *)
      if bd = 1 then
        red array.(ofs) Leaf Leaf
      else
        black array.(ofs) Leaf Leaf

    else if len = 2 then
      (* 2 ^ 1 <= [len] < 2 ^ 2 - 1, so [bd] is 2. *)
      black array.(ofs + 1) (red array.(ofs) Leaf Leaf) Leaf

    else
      (* [len] = 1 + [a] + [b], with [a] and [b] >= 1 *)
      let a_plus_b = len - 1 in
      let a = a_plus_b / 2 in
      let b = a_plus_b - a in

      let left = of_sub_array ofs a (bd - 1) in
      let right = of_sub_array (ofs + a + 1) b (bd - 1) in

      (* Let's prove that bd - 1 is an okay black depth for both a and b,
         so that Black (left, right) has black depth bd.

         If len is odd, a = b = len - 1 / 2.

         If a = 2 ^ K - 1, bd - 1 must be K or K + 1.
         len = 1 + 2 * (2 ^ K - 1) = 2 ^ (K + 1) - 1.
         So bd must be K + 1 or K + 2.
         So bd - 1 is OK for a and b.

         Otherwise, 2 ^ (K - 1) <= a < 2 ^ K - 1 and bd - 1 must be K.
         So 1 + 2 ^ K <= len < 2 ^ (K + 1) - 1.
         So bd = K + 1.
         So bd - 1 is OK for a and b.

         If len is even, a = (len - 2) / 2, and b = a + 1.

         If a = 2 ^ K - 1, bd - 1 must be K or K + 1.
         len = 2 ^ (K + 1).
         So bd = K + 2.
         So bd - 1 is OK for a.
         b = 2 ^ K = 2 ^ (bd - 2), so bd - 1 is also OK for b.

         Otherwise, 2 ^ (K - 1) <= a < 2 ^ K - 1 and bd - 1 must be K.
         So 2 + 2 ^ K <= len < 2 ^ (K + 1).
         If len = 2 ^ (K + 1) - 1, then len is not even, so this is impossible.
         So 2 + 2 ^ K <= len < 2 ^ (K + 1) - 1.
         So bd = K + 1, and bd - 1 is indeed K.
         So bd - 1 is OK for a.
         We have 2 ^ (K - 1) + 1 <= b < 2 ^ K.
         If b = 2 ^ K - 1, bd - 1 (= K) must be K or K + 1, so bd - 1 is OK for b.
         Else 2 ^ (K - 1) + 1 <= b < 2 ^ K - 1, and db - 1 (= K) must be K.
         So bd - 1 is OK for b. *)
      black array.(ofs + a) left right
  in

  let len = match len with None -> Array.length array - ofs | Some len -> len in
  if len = 0 then
    empty
  else
    (* Find K such that 2 ^ (K - 1) <= len < 2 ^ K. *)
    let rec find_k_from i =
      if len < 1 lsl i then
        i
      else
        find_k_from (i + 1)
    in
    of_sub_array 0 len (find_k_from 1)

let of_list ?ofs ?len list =
  of_array ?ofs ?len (Array.of_list list)

let iter ?(ofs = 0) ?len f sequence =
  let len =
    match len with
      | None ->
          count sequence - ofs
      | Some len ->
          len
  in
  let rec iter first last f sequence =
    if last >= first then
      match sequence with
        | Leaf
        | Double_black_leaf ->
            ()
        | Node (_, _, value, left, right) ->
            let left_count = count left in
            if first < left_count then iter first last f left;
            if first <= left_count && last >= left_count then f value;
            if last > left_count then iter (first - left_count - 1) (last - left_count - 1) f right
  in
  iter ofs (ofs + len - 1) f sequence

let to_list ?ofs ?len sequence =
  let items = ref [] in
  iter ?ofs ?len (fun item -> items := item :: !items) sequence;
  List.rev !items

let rec map f sequence =
  match sequence with
    | Leaf ->
        Leaf
    | Double_black_leaf ->
        Double_black_leaf
    | Node (color, count, value, left, right) ->
        Node (color, count, f value, map f left, map f right)

let rec map_sub first last f sequence =
  if last < first then
    sequence
  else
    match sequence with
      | Leaf ->
          Leaf
      | Double_black_leaf ->
          Double_black_leaf
      | Node (color, node_count, value, left, right) ->
          let left_count = count left in
          if last < left_count then
            (* Everything is strictly on the left. *)
            Node (
              color, node_count, value,
              map_sub first last f left,
              right
            )
          else if first > left_count then
            (* Everything is strictly on the right. *)
            Node (
              color, node_count, value,
              left,
              map_sub (first - left_count - 1) (last - left_count - 1) f right
            )
          else
            (* Interval includes current node. *)
            Node (
              color, node_count, f value,
              map_sub first (left_count - 1) f left,
              map_sub 0 (last - left_count - 1) f right
            )

let map_until last f sequence =
  map_sub 0 last f sequence

let map_from first f sequence =
  map_sub first (count sequence - 1) f sequence

let rec fold f acc sequence =
  match sequence with
    | Leaf
    | Double_black_leaf ->
        acc
    | Node (_, _, value, left, right) ->
        let acc = fold f acc left in
        let acc = f acc value in
        fold f acc right

let extract index sequence =
  match get index sequence with
    | None ->
        None
    | Some value ->
        Some (value, remove index sequence)

let rec split index sequence =
  let sequence_count = count sequence in
  let index = min sequence_count index in

  if index < sequence_count / 2 then
    (* Extract the left-hand side. *)
    let left = ref empty in
    let right = ref sequence in

    for i = 0 to index - 1 do
      match extract 0 !right with
        | None ->
            assert false (* impossible *)
        | Some (extracted_value, remaining_sequence) ->
            left := insert i extracted_value !left;
            right := remaining_sequence
    done;

    !left, !right

  else
    (* Extract the right-hand side. *)
    let left = ref sequence in
    let right = ref empty in

    for i = index to sequence_count - 1 do
      match extract (count !left - 1) !left with
        | None ->
            assert false (* impossible *)
        | Some (extracted_value, remaining_sequence) ->
            right := insert 0 extracted_value !right;
            left := remaining_sequence
    done;

    !left, !right

(* TODO: we don't need to build the right-hand side. *)
let split_left index sequence =
  fst (split index sequence)

(* TODO: we don't need to build the left-hand side. *)
let split_right index sequence =
  snd (split index sequence)

let concat left right =
  let result = ref left in
  for i = 0 to count right - 1 do
    match get i right with
      | None ->
          assert false (* impossible *)
      | Some value ->
          result := insert (count !result) value !result
  done;
  !result

let sub x1 x2 sequence =
  let rec loop acc x =
    if x > x2 then
      acc
    else match get x sequence with
      | None ->
          acc
      | Some value ->
          loop (append value acc) (x + 1)
  in
  loop empty x1

let insert_sub x sub sequence =
  let result = ref sequence in
  for i = count sub - 1 downto 0 do
    match get i sub with
      | None ->
          assert false (* impossible *)
      | Some value ->
          result := insert x value !result
  done;
  !result

let equals equal_characters a b =
  let character_count = count a in
  if count b <> character_count then
    false
  else
    (* TODO: we could maybe avoid repeating the O(log n) access for each character *)
    let rec loop from =
      if from >= character_count then
        true
      else
        let ca = match get from a with None -> assert false | Some x -> x in
        let cb = match get from b with None -> assert false | Some x -> x in
        if equal_characters ca cb then
          loop (from + 1)
        else
          false
    in
    loop 0

let rec truncate max_count sequence =
  let count = count sequence in
  if count > max_count then
    truncate max_count (remove (count - 1) sequence)
  else
    sequence
