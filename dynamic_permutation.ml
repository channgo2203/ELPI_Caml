type direction =
  L
| R

(* attach direction into an array of elements *)

let attach_direction d a =
  Array.map (fun x -> (d, x)) a

let swap i j a =
  let tmp = a.(j) in
  a.(j) <- a.(i);
  a.(i) <- tmp

(* an element is movable towards its direction if there is a smaller neighbour *)
        
let is_movable i a = 
  let (d, e) = a.(i) in
  match d with
  | L -> if i > 0 && e > (snd a.(i-1)) then true else false
  | R -> if i < Array.length a - 1 && e > (snd a.(i+1)) then true else false

(* return the largest movable element in the array a *)
                                   
let scan_largest_movable a =
  let rec aux largest i =
    if i >= Array.length a then
      largest
    else if not (is_movable i a) then
      aux largest (i + 1)
    else
      let (d1, e1) = a.(i) in
      match largest with
      | None -> aux (Some i) (i + 1)
      | Some j -> let (d2, e2) = a.(j) in
                  aux (if e1 < e2 then largest else (Some i)) (i + 1)
  in
  aux None 0

(* move the element at index i in a, given that it is the largest movable one *)
      
let move i a =
  if is_movable i a then 
    let (d, e) = a.(i) in
    match d with
    | L -> swap (i - 1) i a
    | R -> swap i (i + 1) a
  else
    failwith "not movable"

let flip d =
  match d with
  | L -> R
  | R -> L

let scan_flip_larger x a =
  Array.iteri (fun i (d, e) -> if e > x then a.(i) <- (flip d, e)) a
              
(* create a permutation generator *)
              
let permutation_generator l =
  let a = Array.of_list l |> attach_direction L in
  let next_p = ref (Some l) in
  (* generate a next permutation *)
  let next () =
    let p = !next_p in
    (
      match scan_largest_movable a with
      | None -> next_p := None (* no more permutation *)
      | Some i -> let (d, e) = a.(i) in
                  (
                    move i a;
                    scan_flip_larger e a;
                    next_p := Some (Array.map snd a |> Array.to_list)
                  )
    );
    p
  in
  next

    
let stream_of_permutation_generator l =
  let p = permutation_generator l in
  Stream.from (fun _ -> p ())

open Printf

let _ =
  let p_stream = stream_of_permutation_generator [1;2;3;4;5;6;7;8;9;10] in
  while (true) do
    try
      ( Printf.printf "[ ";
        List.iter (fun x -> Printf.printf "%d " x) (Stream.next p_stream);
        Printf.printf "]\n")
    with Stream.Failure -> (
      Printf.printf "Number of permutations: %d\n" (Stream.count p_stream); exit 0)
  done
    
                                           
              
              
    
    
      
    
                                   
