let swap i j a =
  try
    let tmp = a.(j) in
    a.(j) <- a.(i);
    a.(i) <- tmp; a
  with
    _ -> raise (Invalid_argument "out-of-range")
               
(* auxiliary function *)
(* a[0 .. smaller - 1] : smaller region
   a[smaller .. equivalent - 1] : equivalent region
   a[equivalent .. larger] : unclassified region
   a[larger + 1 ... size - 1] : larger region
 *)
let rec partition smaller equivalent larger pivot a =
  if equivalent <= larger then
    begin
      try
        if a.(equivalent) < pivot then
          (* put a.(equivalent) to the smaller region *)
          partition (smaller+1) (equivalent+1) larger pivot (swap smaller equivalent a)
        else
          if a.(equivalent) = pivot then
            (* move to the next element in the unclassified region *)
            partition smaller (equivalent+1) larger pivot a
          else
            (* move a.(equivalent) to the larger region *)
            partition smaller equivalent (larger+1) pivot (swap equivalent larger a)
      with
        _ -> raise (Invalid_argument "out-of-range")
    end
  else
    a

let dutch_partition pivot_index a =
  try
    partition 0 0 (pred (Array.length a)) (a.(pivot_index)) a
  with
    _ -> raise (Invalid_argument "out-of-range")

               
               

