print_string "Hello, World!"

let rec loop (n : int) : int =
  if n < 1 then 0 else n + loop (n-1)
  
;;print_endline (string_of_int (loop 100))

let int_list : int list = [0;2;-3;3;-2;1;5]

let rec fold (func : 'a -> 'b -> 'b) (bc : 'b) (li : 'a list) : 'b =
  begin match li with
  | [] -> bc
  | f::r -> func f (fold func bc r)
  end

;;print_endline (string_of_int(fold (fun x acc -> x + acc) 0 int_list))

let rec transform (func : 'a -> 'b) (li : 'a list) : 'b list =
  fold (fun x acc -> (func x)::acc) [] li
  
let int_list2: int list = transform (fun x -> x * x) int_list

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
  
;;print_int_list int_list
;;print_int_list int_list2

let gcd_loop_way (a : int) (b : int) : int =
  let x : int ref = {contents = min a b} in
  let c : bool ref = {contents = true} in
  while (x.contents > 1) && c.contents do
    if (a mod x.contents = 0) && (b mod x.contents = 0) then
    c.contents <- false else x.contents <- x.contents - 1
  done;
  x.contents
  
let gcd_rec_way1 (a : int) (b : int) : int =
  let rec loop (a : int) (b : int) (c : int) : int =
  if (c <= 1) then 1 else
    if (a mod c = 0) && (b mod c = 0) then c
    else loop a b (c-1) in
  loop a b (min a b)
  
let gcd_rec_way2 (a : int) (b : int) : int =
  let rec helper (a : int) (b : int) : int =
    if (b = 0) then a else
    helper b (a mod b) in
  helper a b
    
;;print_int (gcd_loop_way 21 69)
;;print_endline ""
;;print_int (gcd_loop_way 56 91)
;;print_endline ""
;;print_int (gcd_loop_way 29016 4392)
;;print_endline ""
;;print_int (gcd_rec_way1 21 69)
;;print_endline ""
;;print_int (gcd_rec_way1 56 91)
;;print_endline ""
;;print_int (gcd_rec_way1 29016 4392)
;;print_endline ""
;;print_int (gcd_rec_way2 21 69)
;;print_endline ""
;;print_int (gcd_rec_way2 56 91)
;;print_endline ""
;;print_int (gcd_rec_way2 29016 4392)
;;print_endline ""

;;print_int (gcd_rec_way2 123 467)
;;print_endline ""

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
  | f::r -> (List.rev (fst (loop li [] [] f)),
    List.rev (snd (loop li [] [] f)))
  end

let merge (li1 : 'a list) (li2 : 'a list) : 'a list =
  let rec loop (li1 : 'a list) (li2 : 'a list)
  (result : 'a list) : 'a list =
    begin match (li1, li2) with
    | ([],[]) -> List.rev result
    | ([], _) -> (List.rev result) @ li2
    | (_, []) -> (List.rev result) @ li1
    | (f1::r1, f2::r2) -> if f1 <= f2 then loop r1 li2 (f1::result)
      else loop li1 r2 (f2::result)
    end in
  loop li1 li2 []

let stalin_merge_sort (li : int list) : int list =
  let x : int list ref = {contents = li} in
  let y : int list list ref = {contents = []} in
  let c : bool ref = {contents = true} in
  while c.contents do
    let (a,b) = stalin_sort x.contents in
    x.contents <- b;
    y.contents <- a::y.contents;
    if b = [] then c.contents <- false else ()
  done;
  y.contents <- List.rev y.contents;
  fold (fun x acc -> merge x acc) [] y.contents
  
;;print_int_list (fst (stalin_sort int_list))
;;print_int_list (snd (stalin_sort int_list))

let int_list3 = [1;2;3;5;6]
let int_list4 = [0;4;7;8]
;;print_int_list (merge int_list3 int_list4)

;;print_endline ("Stalin Merge Sort")
;;print_int_list (stalin_merge_sort int_list)
let int_list5 = [5;2;6;7;1;35;-35;23;10;0;-125;-5]
;;print_int_list (stalin_merge_sort int_list5)

let rec quick_sort = function
  | [] -> []
  | pivot::tail ->
    let left = List.filter (fun x -> x < pivot) tail in
    let right = List.filter (fun x -> x >= pivot) tail in
    quick_sort left @ (pivot::quick_sort right)

;;print_endline ("Quick Sort")
;;print_int_list (quick_sort int_list)
;;print_int_list (quick_sort int_list5)

let arr1 = Array.of_seq (List.to_seq int_list)
let arr2 = Array.of_seq (List.to_seq int_list5)

let selection_sort arr =
  let n = Array.length arr in
  for i = 0 to n - 2 do
    let min_index = ref i in
    for j = i + 1 to n - 1 do
      if arr.(j) < arr.(!min_index) then min_index := j
    done;
    if !min_index <> i then begin
      let temp = arr.(i) in
      arr.(i) <- arr.(!min_index);
      arr.(!min_index) <- temp
    end
  done;
  arr
  
let print_int_array (arr : int array) : unit =
  let rec loop (i : int) : unit =
    if i < Array.length arr then (print_int arr.(i);
    if i != Array.length arr - 1 then
    print_string ";" else () ; loop (i+1))
    else () in
  print_string "[";
  loop 0;
  print_endline "]"
  
;;print_endline ("Selection Sort")
;;print_int_array (selection_sort arr1)
;;print_int_array (selection_sort arr2)