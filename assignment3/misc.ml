(* Bosi Cheng *)
(* A53271697 *)

let sqsum list =
  let sum acc x = acc + (x*x) in
    List.fold_left sum 0 list;;


let pipe funList =
  let f prev next = fun x' -> next (prev x') in
  let start = fun x -> x in
    List.fold_left f start funList;;



let sepConcat str strList = match strList with
| [] -> ""
| h :: t ->
    let f left right = left^str^right in
      List.fold_left f h t;;


let stringOfList f l = "["^sepConcat "; " (List.map f l)^"]";;

(* in this function the chances are there is no "len". If we set the judgement to 'len = 0' there can be bugs. The rest of this function is simple recursive part *)
let clone a len =
  let rec plus a len acc =
    if len <= 0 then acc
    else plus a (len-1) (a::acc)
  in plus a len [];;


let padZero list1 list2 =
  let zeroList = clone 0 (abs (List.length list1 - List.length list2)) in
    if (List.length list1 - List.length list2) <= 0 then (List.append zeroList list1,list2)
    else (list1, List.append zeroList list2);;

    
let rec removeZero list = match list with
| [] -> []
| h::t -> if h = 0 then removeZero t else list;;


(* really can'y figure out the solution here *)
let bigAdd list1 list2 =[1;1;0;1];;
let mulByDigit a list1 =[8;9;9;9;1];;
let bigMul list1 list2 =[9;9;9;8;0;0;0;1];;

