(* Bosi Cheng *)
(* A53271697 *)


let extract (a,b)=b;;

let pair (x,(a,b))=
  if x=a then true else false;;

let rec assoc (d,k,l)=
  match l with
  []->d
  |h::t -> if pair (k,h) then extract h else assoc (d,k,t);;

let removeDuplicates l=
  let rec rem l acc=
  match l with
  [] -> List.rev acc
  |h::t when List.mem h acc->rem t acc
  |h::t->rem t (h::acc)
  in rem l [];;

let rec wwhile (f,b) = 
  let (res, bo) = f b in
  if bo then wwhile(f, res) else res;;

let fixpoint (f,b) = 
  wwhile (( fun temp-> let b = (f temp) in
    (b, b != temp)), b);;

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
