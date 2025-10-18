(* [01]+ *)
let rec group1 l =
  match l with
  | [] -> false 
  | [x] -> if x = '0' || x = '1' then true else false  
  | x :: rest -> if x = '0' || x = '1' then group1 rest else false  

(* 0?1* *)
let group2 l =
  match l with
  | [] -> true 
  | [x] -> if x='0' || x='1' then true else false
  | '0':: rest -> List.for_all(fun c-> c='1') rest
  | '1':: rest -> List.for_all(fun c-> c='1') rest
  |_-> false

(* 0[01]*0 *)
let group3 l = 
  if List.hd l = '0' && List.hd (List.rev l ) = '0'then 
  match List.length l with
  |x when x>2->  List.for_all(fun c -> c='1' || c='0') l
  |x when x=2 -> true
  |_-> false
else false


(* 0*10*10* *)
let group4 l =
  match l with
  |[] -> false
  |[x;y] -> if x='1' && y='1' then true else false
  |_ -> if List.fold_left (fun acc x -> if x='1' then acc+1 else acc ) 0 l = 2 then true else false


(* (00|11)+ *)
let rec group5 l = 
  match l with
  |[]-> false
  |[x;y] -> if (x='0' && y='0') || (x='1' && y='1') then true else false 
  |x::y::rest -> if (x='0' && y='0') || (x='1' && y='1') then group5 rest else false
  |_-> false

    
                  
let belongsTo l = [group1 l;group2 l;group3 l; group4 l;group5 l]
  
