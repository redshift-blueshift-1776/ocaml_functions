type fraction = int * int

let print_fraction (f : fraction) : unit =
  print_int(fst f); print_string("/"); print_endline(string_of_int(snd f))
  
let frac_to_float (f : fraction) : float =
  (float_of_int (fst f)) /. (float_of_int (snd f))

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let simplify_fraction (f : fraction) : fraction =
  let d : int = gcd (fst f) (snd f) in
  if ((snd f) / d > 0) then ((fst f) / d, (snd f) / d)
  else (-(fst f) / d, -(snd f) / d)
  
let multiply_fraction (f1 : fraction) (f2 : fraction) : fraction =
  let f3 : fraction = ((fst f1) * (fst f2), (snd f1) * (snd f2)) in
  simplify_fraction f3
  
let divide_fraction (f1 : fraction) (f2 : fraction) : fraction =
  let f3 : fraction = ((fst f1) * (snd f2), (snd f1) * (fst f2)) in
  simplify_fraction f3
  
let add_fraction (f1 : fraction) (f2 : fraction) : fraction =
  let f3 : fraction = ((fst f1) * (snd f2) + (fst f2) * (snd f1),
  (snd f1) * (snd f2)) in
  simplify_fraction f3
  
let subtract_fraction (f1 : fraction) (f2 : fraction) : fraction =
  let f3 : fraction = ((fst f1) * (snd f2) - (fst f2) * (snd f1),
  (snd f1) * (snd f2)) in
  simplify_fraction f3
  
let compare_fraction (f1 : fraction) (f2 : fraction) : int =
  let f3 : fraction = subtract_fraction f1 f2 in
  fst f3
  
;;print_endline("Fractions functions test")
;;print_fraction(multiply_fraction (1, 2) (2, 3))
;;print_fraction(multiply_fraction (4, 2) (1, 3))
;;print_fraction(multiply_fraction (-4, 2) (1, 3))
;;print_fraction(simplify_fraction (6, 2))
;;print_fraction(simplify_fraction (6, 24))
;;print_fraction(simplify_fraction (-6, 2))
;;print_fraction(simplify_fraction (6, -2))
;;print_fraction(add_fraction (1, 2) (2, 3))
;;print_fraction(add_fraction (1, 2) (1, 6))
;;print_fraction(divide_fraction (1, 2) (2, 3))
;;print_fraction(divide_fraction (4, 2) (1, 3))
;;print_fraction(divide_fraction (-4, 2) (1, 3))
;;print_fraction(subtract_fraction (1, 2) (2, 3))
;;print_fraction(subtract_fraction (1, 2) (1, 6))
;;print_endline(string_of_int(compare_fraction (1, 2) (1, 6)))
;;print_endline(string_of_int(compare_fraction (1, 2) (2, 3)))
;;print_endline("")

let average (li : int list) : fraction =
  let rec loop (li : int list) (l : int) (tot : int) : int * int =
    begin match li with
    | [] -> (l, tot)
    | f::r -> loop r (l + 1) (tot + f)
    end in
  let x : int * int = loop li 0 0 in
  (snd x, fst x)
  
let list_max (li : int list) : int =
  let rec loop (li : int list) (m : int) =
    begin match li with
    | [] -> m
    | f::r -> loop r (max f m)
    end in
  loop li min_int
  
let reverse_list (li : 'a list) : 'a list =
  let rec loop (li : 'a list) (result : 'a list) =
    begin match li with
    | [] -> result
    | f::r -> loop r (f::result)
    end in
  loop li []
  
let int_list : int list = [0;23;27;34;23;-1;31;39;25]

let print_int_list (li : int list) : unit =
  let rec loop (li : int list) : unit =
    begin match li with
    | [] -> ()
    | [f] -> print_int f
    | f::r -> print_int f; print_string(";"); loop r
    end in
  print_string "[";
  loop li;
  print_endline "]"

;;print_endline("List functions test")
;;print_int_list int_list
;;print_fraction(average int_list)
;;print_endline(string_of_float(frac_to_float(average int_list)))
;;print_endline(string_of_int(list_max int_list))
;;print_endline("")

let rec fold (func : 'a -> 'b -> 'b) (bc : 'b) (li : 'a list) : 'b =
  begin match li with
  | [] -> bc
  | f::r -> func f (fold func bc r)
  end

(*This fold runs in the opposite direction, going from left to right.
the previous one goes from right to left.*)
let fold_tr (func : 'a -> 'b -> 'b) (bc : 'b) (li : 'a list) : 'b =
  let rec loop (func : 'a -> 'b -> 'b) (bc : 'b)
    (li : 'a list) (result : 'b) : 'b =
    begin match li with
    | [] -> result
    | f::r -> loop func bc r (func f result)
    end in
  loop func bc li bc

;;print_endline("List fold test")  
;;print_endline (string_of_int(fold (fun x acc -> x + acc) 0 int_list))
;;print_endline (string_of_int(fold_tr (fun x acc -> x + acc) 0 int_list))
;;print_endline("")

let transform (func : 'a -> 'b) (li : 'a list) : 'b list =
  reverse_list (fold_tr (fun x acc -> (func x)::acc) [] li)
  
;;print_endline("List transform test")
let int_list2: int list = transform (fun x -> x * x) int_list
;;print_int_list int_list2
;;print_int_list (reverse_list int_list2)
;;print_endline("")
  
let average (li : int array) : fraction =
  let l : int = Array.length li in
  let x : int ref = {contents = 0} in
  let tot : int ref = {contents = 0} in
  while !x < l do
    tot := !tot + li.(!x);
    incr x
  done;
  (tot.contents, l)
  
let arr1 = Array.of_seq (List.to_seq int_list)

let print_int_array (arr : int array) : unit =
  let rec loop (i : int) : unit =
    if i < Array.length arr then (print_int arr.(i);
      if i != Array.length arr - 1 then
      print_string ";" else () ; loop (i+1))
    else () in
  print_string "[";
  loop 0;
  print_endline "]"

;;print_endline("Array functions test")
;;print_int_array arr1
;;print_fraction(average arr1)
;;print_endline(string_of_float(frac_to_float(average arr1)))
;;print_endline("")
  
type team_score = string * int * int

let compare_team_scores (t1 : team_score) (t2 : team_score) : int =
  begin match t1 with
  | (a, b, c) -> let f1 : fraction = (b, c) in
    begin match t2 with
    | (d, e, f) -> let f2 : fraction = (e, f) in
      compare_fraction f1 f2
    end
  end

type match_score =
  | PL of string * int * string * int * int
  | MM of string * int * string * int * int
  | VT of string * int * string * int * int
  | VTS of string * int * string * int * int
  
let print_team_score (ts : team_score) : unit =
  begin match ts with
  | (a, b, c) -> print_string(a ^ ": " ^ (string_of_int b)
    ^ "/" ^ (string_of_int c))
  end;
  print_endline("")
  
let print_team_score_list (li : team_score list) : unit =
  let rec loop (li : team_score list) : unit =
    begin match li with
    | [] -> ()
    | [f] -> print_team_score f
    | f::r -> print_team_score f; loop r
    end in
  loop li
  
let print_match_score (ms : match_score) : unit =
  begin match ms with
  | PL (a, b, c, d, e) -> print_string(a ^ ": " ^ (string_of_int b)
    ^ ", " ^ c ^ ": " ^ (string_of_int d) ^ ", Max: " ^ (string_of_int e))
  | MM (a, b, c, d, e) -> print_string(a ^ ": " ^ (string_of_int b)
    ^ ", " ^ c ^ ": " ^ (string_of_int d) ^ ", Max: " ^ (string_of_int e))
  | VT (a, b, c, d, e) -> print_string(a ^ ": " ^ (string_of_int b)
    ^ ", " ^ c ^ ": " ^ (string_of_int d) ^ ", Max: " ^ (string_of_int e))
  | VTS (a, b, c, d, e) -> print_string(a ^ ": " ^ (string_of_int b)
    ^ ", " ^ c ^ ": " ^ (string_of_int d) ^ ", Max: " ^ (string_of_int e))
  end;
  print_endline("")
  
let match_score_to_team_scores (ms : match_score)
: team_score * team_score =
  begin match ms with
  | PL (a, b, c, d, e) -> ((a, b, e), (c, d, e))
  | MM (a, b, c, d, e) -> ((a, b, e), (c, d, e))
  | VT (a, b, c, d, e) -> ((a, b, e), (c, d, e))
  | VTS (a, b, c, d, e) -> ((a, b, e), (c, d, e))
  end
  
let thirty_twenty : match_score = PL ("Cougars", 30, "Gators", 20, 40)
let finals : match_score = VT ("Cougars", 13, "Churchmen", 9, 30)
let shipley_b : match_score = VTS ("Shipley B", 2, "Gators", 13, 30)
let the_comeback : match_score = MM ("Cougars", 39,
"Ladue Horton Watkins", 37, 40)
let tied_match : match_score = VTS ("Blue Devils", 29, "Quakers", 29, 40)
  
let determine_winner (ms : match_score) : string =
  let scores : team_score * team_score = match_score_to_team_scores ms in
  let x : int = compare_team_scores (fst scores) (snd scores) in
  if x = 0 then "Tied" else
    if x > 0 then begin match (fst scores) with
      | (a, b, c) -> a
      end else begin match (snd scores) with
      | (a, b, c) -> a
      end
  
;;print_endline("Type test")
;;print_match_score thirty_twenty
;;print_match_score finals
;;print_match_score shipley_b
;;print_match_score the_comeback
let scores : team_score * team_score =
match_score_to_team_scores thirty_twenty
;;print_team_score(fst scores)
;;print_team_score(snd scores)
;;print_endline(string_of_int(compare_team_scores (fst scores) (snd scores)))
let scores : team_score * team_score =
match_score_to_team_scores shipley_b
;;print_team_score(fst scores)
;;print_team_score(snd scores)
;;print_endline(string_of_int(compare_team_scores (fst scores) (snd scores)))
;;print_endline(determine_winner thirty_twenty)
;;print_endline(determine_winner shipley_b)
;;print_endline(determine_winner tied_match)
;;print_endline("")
  
let stalin_sort (li : int list) : int list * int list =
  let rec loop (li : int list) (result : int list)
  (gulag : int list) (x : int) : int list * int list =
    begin match li with
    | [] -> (result, gulag)
    | f::r -> if f >= x then loop r (f::result) gulag f
      else loop r result (f::gulag) x
    end in
  begin match li with
  | [] -> ([],[])
  | f::r -> (reverse_list (fst (loop li [] [] f)),
    reverse_list (snd (loop li [] [] f)))
  end

let merge (li1 : int list) (li2 : int list) : int list =
  let rec loop (li1 : int list) (li2 : int list)
  (result : int list) : int list =
    begin match (li1, li2) with
    | ([],[]) -> reverse_list result
    | ([], _) -> (reverse_list result) @ li2
    | (_, []) -> (reverse_list result) @ li1
    | (f1::r1, f2::r2) -> if f1 <= f2 then loop r1 li2 (f1::result)
      else loop li1 r2 (f2::result)
    end in
  loop li1 li2 []

let stalin_merge_sort (li : int list) : int list =
  let x : int list ref = {contents = li} in
  let y : int list list ref = {contents = []} in
  let c : bool ref = {contents = true} in
  while !c do
    let (a,b) = stalin_sort !x in
    x := b; y := a::!y;
    if b = [] then c := false else ()
  done;
  fold_tr (fun x acc -> merge x acc) [] !y
  
;;print_endline("Stalin sort test")
;;print_int_list (fst (stalin_sort int_list))
;;print_int_list (snd (stalin_sort int_list))
;;print_int_list (stalin_merge_sort int_list)
;;print_endline("")

let stalin_sort (li : 'a list) (c : 'a -> 'a -> int) : 'a list * 'a list =
  let rec loop (li : 'a list) (result : 'a list) (gulag : 'a list)
    (x : 'a) (c : 'a -> 'a -> int) : 'a list * 'a list =
    begin match li with
    | [] -> (result, gulag)
    | f::r -> if c f x >= 0 then loop r (f::result) gulag f c
      else loop r result (f::gulag) x c
    end in
  begin match li with
  | [] -> ([],[])
  | f::r -> let ss = loop li [] [] f c in
    (reverse_list (fst ss), reverse_list (snd ss))
  end
  
let merge (li1 : 'a list) (li2 : 'a list) (c : 'a -> 'a -> int) : 'a list =
  let rec loop (li1 : 'a list) (li2 : 'a list)
  (result : 'a list) (c : 'a -> 'a -> int) : 'a list =
    begin match (li1, li2) with
    | ([],[]) -> reverse_list result
    | ([], _) -> (reverse_list result) @ li2
    | (_, []) -> (reverse_list result) @ li1
    | (f1::r1, f2::r2) -> if c f1 f2 <= 0 then loop r1 li2 (f1::result) c
      else loop li1 r2 (f2::result) c
    end in
  loop li1 li2 [] c

let stalin_merge_sort (li : 'a list) (cm : 'a -> 'a -> int) : 'a list =
  let x : 'a list ref = {contents = li} in
  let y : 'a list list ref = {contents = []} in
  let c : bool ref = {contents = true} in
  while !c do
    let (a,b) = stalin_sort !x cm in
    x := b;
    y := a::!y;
    if b = [] then c := false else ()
  done;
  fold_tr (fun x acc -> merge x acc cm) [] !y

let week_one : team_score list = [("Roos", 23, 40);("Owls", 22, 40);
("Bears", 27, 40);("Churchmen", 34, 40);("Tigers", 0, 40);
("Cougars", 35, 40);("Fords", 29, 40);("Rams", 32, 40);("Quakers", 36, 40);
("Gators", 30, 40);("Blue Devils", 22, 40);("Moose", 31, 40)]
let week_five : team_score list = [("Roos", 10, 35);("Owls", 13, 35);
("Bears", 11, 35);("Churchmen", 24, 35);("Tigers", 23, 35);
("Cougars", 29, 35);("Fords", 12, 35);("Rams", 28, 35);("Quakers", 23, 35);
("Gators", 24, 35);("Blue Devils", 12, 35);("Moose", 23, 35)]
let cougars_pl_vt : team_score list = [("Week 1", 35, 40);("Week 2", 33, 40);
("Week 3", 40, 40);("Week 4", 30, 40);("Week 5", 29, 35);("Week 6", 31, 40);
("Week 7", 34, 40);("Week 8", 34, 40);("Week 9", 16, 40);("VT 1", 5, 6);
("VT 2", 26, 30);("VT 3", 17, 30);("VT 4", 5, 6);("VT 5", 13, 30)]
let gators_pl_vt : team_score list = [("Week 1", 30, 40);("Week 2", 29, 40);
("Week 3", 34, 40);("Week 4", 25, 40);("Week 5", 24, 35);("Week 6", 21, 40);
("Week 7", 24, 40);("Week 8", 30, 40);("Week 9", 14, 40);("VT 1", 5, 6);
("VT 2", 20, 30);("VT 3", 13, 30);("VT 4", 5, 6);("VT 5", 5, 30)]

;;print_endline("Comparator test")
;;print_int_list (fst (stalin_sort int_list compare))
;;print_int_list (snd (stalin_sort int_list compare))
;;print_int_list (stalin_merge_sort int_list compare)
;;print_endline("\ninput")
;;print_team_score_list week_one
;;print_endline("\nresult")
;;print_team_score_list (fst (stalin_sort week_one compare_team_scores))
;;print_endline("\ngulag")
;;print_team_score_list (snd (stalin_sort week_one compare_team_scores))
;;print_endline("\nsorted")
;;print_team_score_list (stalin_merge_sort week_one compare_team_scores)
;;print_endline("\ninput")
;;print_team_score_list week_five
;;print_endline("\nresult")
;;print_team_score_list (fst (stalin_sort week_five compare_team_scores))
;;print_endline("\ngulag")
;;print_team_score_list (snd (stalin_sort week_five compare_team_scores))
;;print_endline("\nsorted")
;;print_team_score_list (stalin_merge_sort week_five compare_team_scores)
;;print_endline("\ninput")
;;print_team_score_list cougars_pl_vt
;;print_endline("\nresult")
;;print_team_score_list (fst (stalin_sort cougars_pl_vt compare_team_scores))
;;print_endline("\ngulag")
;;print_team_score_list (snd (stalin_sort cougars_pl_vt compare_team_scores))
;;print_endline("\nsorted")
;;print_team_score_list (stalin_merge_sort cougars_pl_vt compare_team_scores)
;;print_endline("\ninput")
;;print_team_score_list gators_pl_vt
;;print_endline("\nresult")
;;print_team_score_list (fst (stalin_sort gators_pl_vt compare_team_scores))
;;print_endline("\ngulag")
;;print_team_score_list (snd (stalin_sort gators_pl_vt compare_team_scores))
;;print_endline("\nsorted")
;;print_team_score_list (stalin_merge_sort gators_pl_vt compare_team_scores)
;;print_endline("")

type 'a dqnode = {
  v: 'a;
  mutable next: 'a dqnode option;
  mutable prev: 'a dqnode option;
}

type 'a deque = {
  mutable head: 'a dqnode option;
  mutable tail: 'a dqnode option;
}

let alias (x: 'a dqnode) : 'a dqnode list -> bool =
  List.fold_left (fun acc h -> x == h || acc) false

let check_to_tail (n: 'a dqnode) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    begin match curr.next with
    | None -> Some curr
    | Some m ->
       begin match m.prev with
       | None -> None
       | Some mp ->
          if mp != curr || alias curr seen
          then None
          else loop m (curr :: seen)
       end
    end
  in loop n []

let check_to_head (n: 'a dqnode) : 'a dqnode option =
  let rec loop (curr: 'a dqnode) (seen: 'a dqnode list) : 'a dqnode option =
    begin match curr.prev with
    | None -> Some curr
    | Some m ->
        begin match m.next with
        | None -> None
        | Some mn ->
            if mn != curr || alias curr seen
            then None
            else loop m (curr :: seen)
        end
    end
  in loop n []

let valid (d: 'a deque) : bool =
  begin match d.head, d.tail with
  | None, None -> true
  | Some h, Some t ->
      begin match check_to_tail h, check_to_head t with
      | Some n2, Some n1 -> n2 == t && n1 == h
      | _, _ -> false
      end
  | _, _ -> false
  end

let create_deque () : 'a deque =
  {head = None; tail = None}

let is_empty (d: 'a deque) : bool =
  d.head = None

let peek_head (q: 'a deque) : 'a =
  begin match q.head with
  | None -> failwith "peek_head called on empty deque"
  | Some hd -> hd.v
  end

let peek_tail (q: 'a deque) : 'a =
  begin match q.tail with
  | None -> failwith "peek_tail called on empty deque"
  | Some tl -> tl.v
  end

let to_list (q: 'a deque) : 'a list =
  let rec loop (qn: 'a dqnode option) (l: 'a list) : 'a list =
    begin match qn with
    | None -> l
    | Some n -> loop n.prev (n.v::l)
    end
  in
  loop q.tail []
  
let insert_head (x: 'a) (q: 'a deque) : unit =
  let q1 = {v = x; next = q.head; prev = None} in
  begin match q.head with
  | None -> q.head <- Some q1; q.tail <- Some q1
  | Some n -> n.prev <- Some q1; q.head <- Some q1;
  if q.tail = None then (q.tail <- Some q1) else ()
  end

let insert_tail (x: 'a) (q: 'a deque) : unit =
  let q1 = {v = x; next = None; prev = q.tail} in
  begin match q.tail with
  | None -> q.tail <- Some q1; q.head <- Some q1
  | Some n -> n.next <- Some q1; q.tail <- Some q1;
  if q.head = None then (q.head <- Some q1) else ()
  end

let remove_head (q: 'a deque) : 'a =
  begin match q.head with
  | None -> failwith "empty deque"
  | Some n -> 
    begin match n.next with
    | None -> q.head <- None; q.tail <- None;
    | Some nn -> q.head <- Some nn; nn.prev <- None;
    end;
    n.v
  end

let remove_tail (q: 'a deque) : 'a =
  begin match q.tail with
  | None -> failwith "empty deque"
  | Some n -> 
    begin match n.prev with
    | None -> q.tail <- None; q.head <- None;
    | Some nn -> q.tail <- Some nn; nn.next <- None;
    end;
    n.v
  end

let list_to_deque (li : 'a list) : 'a deque =
  let ret : 'a deque = create_deque() in
  let rec loop (li : 'a list) (d : 'a deque) : 'a deque =
    begin match li with
    | [] -> d
    | f::r -> insert_tail f d; loop r d
    end in
  loop li ret

let print_int_deque (q: int deque) : unit =
  let rec loop (qn: int dqnode option) : unit =
    begin match qn with
    | None -> print_string(">")
    | Some n -> print_int n.v; print_string("-"); loop n.next
    end in
  print_string("<-"); loop q.head; print_endline ""

let int_deque = list_to_deque int_list
;;print_int_deque int_deque

let gcd_loop_way a b =
  let x = ref (min a b) in
  let c = ref true in
  while !x > 1 && !c do
    if (a mod !x = 0) && (b mod !x = 0) then
    c := false else decr x
  done;
  !x

;;print_int (gcd_loop_way 21 69)
;;print_endline ""
;;print_int (gcd_loop_way 56 91)
;;print_endline ""
;;print_int (gcd_loop_way 29016 4392)
;;print_endline ""
;;print_int (gcd_loop_way 29015 4392)
;;print_endline ""
let df = 0.69
;;print_float(1. /. exp (-. log df))
;;print_endline ""

(*
type 'a heap_node =
  | None
  | Some of 'a * 'a heap_node * 'a heap_node
  
type 'a heap = {mutable root : 'a heap_node; mutable size : int;
  mutable tail : 'a heap_node}

let create_heap () : 'a heap =
  {root = None; size = 0; tail = None}

let is_empty (h : 'a heap) : bool =
  h.size = 0

let get_root (h : 'a heap) : 'a option =
  begin match h.root with
  | None -> None
  | Some (a, b, c) -> Some a
  end

let heapify (h : 'a heap) (i : int) (c : 'a -> 'a -> int) : unit =
  ()

let insert_heap (h : 'a heap) (v : 'a) (c : 'a -> 'a -> int) : unit =
  ()

let rec echo () =
  print_endline "Input properly formatted data";
  try
  let i = read_line() in
  if i = "Done" then print_endline "Done" else (print_endline i; echo ())
  with e -> print_endline "End of file"
  
;;echo ()
*)

let database : int array array = Array.make_matrix 12 9 0
let teams : string array = Array.make 12 ""
let rec read_data (d : int array array) (t : string array) (i : int) : unit =
  try (
  let l = read_line() in
  let ss = String.split_on_char '\t' l in
  let rec read_data_line (l : string list) (j : int) =
    begin match l with
    | [] -> print_endline("]")
    | f::r -> if j = 0
      then (print_string (f ^ ": ["); Array.set teams i f;
      read_data_line r (j + 1))
      else if j = 1 then (print_string (f); d.(i).(j - 1) <- int_of_string f;
      read_data_line r (j + 1))
      else (print_string (";" ^ f); d.(i).(j - 1) <- int_of_string f;
      read_data_line r (j + 1))
    end in
  read_data_line ss 0;
  if l = "Done" then print_endline "Done" else (print_endline l; 
  read_data d teams (i + 1)))
  with e -> print_endline "End of file"
;;read_data database teams 0
;;Array.iter print_endline teams
;;Array.iter print_int_array database