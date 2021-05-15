module STR_set = Set.Make(String);;
let tasks = ref STR_set.empty ;;
let pairs = ref [] ;;
exception Invalid_input;;

try
  while true do
    let a = read_line() in
    let b = read_line() in
    tasks := STR_set.add a !tasks;
    tasks := STR_set.add b !tasks;
    pairs := ((a,b) :: !pairs)
  done
with _ -> ()

let array_1d d =
  let l = ref [] in
  for i = 1 to d do
    l := 0 :: !l;
  done;
  !l

let array_2d d =
  let l = ref [] in
  for i = 1 to d do
    l := (array_1d d) :: !l;
  done;
  !l

let rec func x lst c = match lst with
  | [] -> -1
  | hd::tl -> if ((compare hd x)=0) then c else func x tl (c+1)
let find x lst = func x lst 0;;

let rec replace_1d_rc lst x idx c = match lst with
  | [] -> []
  | hd::tl -> if(idx=c) then x::tl else hd::(replace_1d_rc tl x idx (c+1))
let repl_1d lst x idx = replace_1d_rc lst x idx 0;;

let set_1 lst a b =
  let l = repl_1d (List.nth lst a) 1 b in
  repl_1d lst l a;;

let t_list = STR_set.elements !tasks ;;
let p_list = !pairs
let sorted = List.sort (fun a b -> compare a b) t_list ;;
let size   = List.length t_list;;
let adj_mat= ref [];;
adj_mat := array_2d size;;

List.iter
  (fun (a,b) ->
     let t_1 = find a sorted in
     let t_2 = find b sorted in
     adj_mat:=set_1 !adj_mat t_2 t_1;
  )
  !pairs;;

let get_col lst c =
  let l = ref[] in
  for i=0 to (List.length lst)-1 do
    l := (List.nth (List.nth lst i) c) :: !l;
  done;
  !l;;

let rec has_0 lst = match lst with
  | [] -> false
  | hd::tl -> if hd=0 then true else has_0 tl ;;

let rec has_1 lst = match lst with
  | [] -> false
  | hd::tl -> if hd=1 then true else has_1 tl ;;

let rec no_1 lst = match lst with
  | [] -> true
  | hd::tl -> if hd=1 then false else no_1 tl ;;

let rec has_2 lst = match lst with
  | [] -> false
  | hd::tl -> if hd=2 then true else has_2 tl ;;

let rec all_0 lst = match lst with
  | [] -> true
  | hd::tl -> if hd!=0 then false else all_0 tl ;;

let rec fst_col_rec lst c=
  if (c>= List.length lst) then -1 else
    (let col = get_col lst c in
     if ((no_1 col)&&(has_0 col)) then c else fst_col_rec lst (c+1))

let set_2 lst a b =
  let l = repl_1d (List.nth lst a) 2 b in
  repl_1d lst l a;;

let order = ref [];;

try
  for i=1 to size do
    let tsk = fst_col_rec !adj_mat 0 in
    if tsk=(-1)then raise Invalid_input
    else order:=tsk::!order;
    for i=0 to (size-1) do
      adj_mat := set_2 !adj_mat i tsk;
      adj_mat := set_2 !adj_mat tsk i;
    done;
  done;
  List.iter (fun a -> print_endline(List.nth sorted a)) (List.rev !order)

with _ ->
  print_endline("cycle")
