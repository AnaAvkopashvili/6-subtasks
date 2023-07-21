(*Problem 1*)

(*a*)
let swap (a, b) = (b, a)
(* let f1 lst = List.rev(List.fold_left (fun acc x -> (swap x :: acc)) [] lst) *)

let f1 acc lst = List.rev((fun acc x -> (swap x :: acc)) (List.rev (acc)) lst);;

(*b*)
(* let index_fold x lst =
    let (_, index) = List.fold_left (fun (found, index) el ->
      if found then (found, index) else
        if el = x then (true, index) else (false, index + 1)
    ) (false, 0) lst in
    if index = List.length lst then raise (Failure "Not Found") else index

let evens_odds lst = List.fold_left (fun (evens, odds) x -> if (index_fold x lst) mod 2 = 0 then ((x :: evens), odds) else (evens, (x :: odds)))([], []) lst
let reversed_evens_list lst = List.fold_left (fun acc x -> x :: acc) [] (fst (evens_odds lst))
let reversed_odds_list lst = List.fold_left (fun acc x -> x :: acc) [] (snd (evens_odds lst))

let f2 lst = 
if List.length(lst) mod 2 = 1 then
   (fst (evens_odds lst)) @ (reversed_odds_list lst)
else 
   (snd (evens_odds lst)) @ (reversed_evens_list lst) *)

let f2  acc x = if (List.length acc) mod 2 = 0 then x :: acc  else acc @ [x] 

(*c*)
let f3 helper_fun (k, v) = fun n -> if n = k then v else helper_fun n

(*Poblem 2*)
(*a*)

let map_tr f l = 
    let rec helper f l acc = match l with
      | [] -> acc
      | h :: t -> helper f t ((f h) :: acc)
in helper f (List.rev l) []

(*b*)
let replicate_tr n x = 
  let rec helper n x acc = 
    if n < 1 then acc else helper (n - 1) x (x :: acc)
in helper n x []

(*Problem 3*)

(* we do not need to use Lazy.force in this code, because the 
t argument is already a function that returns the next cell in the list.*)

type 'a custom_llist = (unit -> 'a custom_cell) and 
'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)

let rec map_over_custom_llist f lst = fun() -> match lst () with
  | NilC -> NilC
  | ConsC (h, t) -> ConsC (f h, map_over_custom_llist f t)

  
(*The Lazy.force function takes a lazy value as its argument, 
   and evaluates it to produce its value.*)

type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 
'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)

let rec map_over_ocaml_llist f lst =  match Lazy.force lst with
  | NilO -> lazy NilO
  | ConsO(h, t) -> lazy (ConsO (f h, map_over_ocaml_llist f t))


(*problem 4*)

(*custom list*)

let rec merge_custom_llists l1 l2 = fun() -> match l1(), l2() with
  | NilC, NilC -> NilC
  | ConsC (h, t), NilC -> ConsC (h, merge_custom_llists t l2)
  | NilC, ConsC (h, t) -> ConsC (h, merge_custom_llists l1 t)
  | ConsC (h1, t1), ConsC (h2, t2) ->
     if h1 < h2 then ConsC (h1, merge_custom_llists t1 l2)
     else ConsC (h2, merge_custom_llists l1 t2)


 (*ocaml list*)    

let rec merge_ocaml_llists l1 l2 = match Lazy.force l1, Lazy.force l2 with 
  | (NilO, NilO) -> lazy NilO
  | (ConsO (h, t), NilO) -> lazy (ConsO (h, merge_ocaml_llists t (lazy  NilO)))
  | (NilO, ConsO (h, t))  -> lazy (ConsO (h, merge_ocaml_llists t (lazy NilO)))
  | (ConsO (h1, t1), ConsO (h2, t2)) -> if h1 < h2 then lazy (ConsO (h1, merge_ocaml_llists t1 l2)) else lazy (ConsO (h2, merge_ocaml_llists l1 t2))


  (*problem 5*)

  (*custom list*)

  let drop_dupl_custom_llist lst = 
    let rec helper acc lst1 = match lst1 () with 
    |NilC -> acc
    | ConsC (h, t) -> 
      (match acc () with 
      |NilC -> helper (fun () -> ConsC (h, acc)) t
      | ConsC (h2, t2) -> if h <> h2 then helper (fun() -> ConsC (h, acc)) t else helper acc t)
    in
    let rec rev_custom_list lst acc = match lst () with NilC -> acc
    | ConsC (h, t) -> rev_custom_list t (fun () -> ConsC (h, acc))
    in
    rev_custom_list (helper (fun() -> NilC) lst) (fun () -> NilC)

  (*ocaml list*)

  let drop_dupl_ocaml_llist lst = 
    let rec helper acc lst1 = match (Lazy.force (lst1)) with
    | NilO -> acc
    | ConsO (h, t) -> 
      match (Lazy.force acc) with
      | NilO -> helper (lazy (ConsO (h, acc))) t
      | ConsO (h2, t2) -> if h <> h2 then helper (lazy (ConsO (h, acc))) t else helper acc t
    in
    let rec rev_ocaml_list lst acc = match Lazy.force lst with 
      | NilO -> acc
      | ConsO (h, t) -> rev_ocaml_list t (lazy (ConsO (h, acc)))
  in 
  rev_ocaml_list (helper (lazy (NilO)) lst) (lazy(NilO))

  (*problem 6*)

  let rec is_hamming x = 
    if x = 1 then true
    else if x mod 2 = 0 then is_hamming (x / 2)
    else if 
      x mod 3 = 0 then is_hamming (x / 3)
    else if
      x mod 5 = 0 then is_hamming (x / 5)
  else 
    false
  
  
  let rec filter_custom lst =
    let rec lfilter_custom b = function
      | NilC -> NilC
      | ConsC (h, t) ->
          if b h then
            ConsC (h, fun () -> lfilter_custom b (t ()))
          else
            lfilter_custom b (t ())
    in
    lfilter_custom is_hamming lst
  
  let rec nat_custom n = ConsC (n, fun () -> nat_custom (n + 1))
  let hamming_custom = fun() -> filter_custom (nat_custom 1)
  
   (*ocaml list*)    
  
   let rec filter_ocaml lst = 
     let rec lfilter_ocaml b lst = match Lazy.force lst with
       | NilO -> lazy NilO
       | ConsO (h, t) -> 
         if b h then
           lazy (ConsO (h, (lfilter_ocaml b (lazy (Lazy.force t)))))
         else
           lfilter_ocaml b (lazy(Lazy.force t))
     in
     lfilter_ocaml is_hamming lst
   
  let rec nat_ocaml n = ConsO (n, (lazy (nat_ocaml (n + 1))))
  let hamming_ocaml = (filter_ocaml (lazy (nat_ocaml 1)))
