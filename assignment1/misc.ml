(*Bosi Cheng*)
(*PID:A53271697*)
let rec sumList l=
  match l with
  | []->0
  |h::t->h+(sumList t);;
(*use cons operator to create a recursion function, each time it gets its head element and put its tail in itself again*)




let digitsOfInt n =
  let rec dig acc n =
      if n < 10 then n::acc
      else dig ((n mod 10)::acc) (n/10) in
  dig [] n;;
(*create a recursion function with 'rec' so each time the function put its lowest digit into the list, the begining list acc(accumulation) is []*)

let abs n=
  if n<=0 then -n
  else n

let digits n =
   let rec dd acc n =
       if n < 10 then n::acc
       else dd ((n mod 10)::acc) (n/10) in
  dd [] (abs n);;
  (*the digits function is almost the same as digitsOfInt*)


let digitsAdd digits =
   let rec da sum x =
     if x <= 0 then sum else
     da (sum + x mod 10) (x / 10)
   in
   da 0 digits

let additivePersistence n=
   let rec ap sum n=
    if n<10 then sum
     else ap (sum+1) (digitsAdd n)
   in
   ap 0 n;;
(*in this recursion add the sum(initial 0)with 1 everytime the digitsAdd function adds all the digits of integer n*)





   let digitsAdd digits =
    let rec da sum x =
      if x <= 0 then sum else
      da (sum + x mod 10) (x / 10)
    in
    da 0 digits

  let digitalRoot n=
    let rec dr n=
     if n<10 then n
     else dr (digitsAdd n)
   in
   dr n;;
  (*this funciton also uses the function digitsAdd but simply print its last result*)




   let listReverse l =
    let rec lr temp l= 
      match l with
      | [] -> temp
      | h::t -> lr (h::temp) t
    in
    lr [] l;;
  (*each time gets the first element of the list l till the last element and [] is left, then from [] adds each element reversely back to form a reversed list*)


    let explode s = 
      let rec _exp i = 
        if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
      _exp 0
    
      let palindrome w=
        (explode w)=listReverse (explode w);;
  (*use explode to break s(string) into a list and then apply the former function listReverse*)

    






