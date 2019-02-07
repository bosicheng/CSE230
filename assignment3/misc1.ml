(* CSE 130: Programming Assignment 3
 * misc.ml
 *
 * Li, Xiaoqing
 * A97071753
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)
(* val sqsum : int list -> int
 *
 * sqsum takes an int list and sums the squares of the members of
 * that list. It does this by passing the function f a x into
 * fold_left, essentially creating an accumulator a and adding to
 * it the square of each value.
 *)
 let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
    List.fold_left f base xs

(* val pipe : ('a -> 'a) list -> 'a -> 'a
 *
 * pipe takes a list of 'a -> 'a functions and returns a function
 * that takes a value and runs through all of the functions in
 * order. First, the base case returns a function x -> x, which
 * simply returns the input, and each recursive case creates a
 * new function f a x which takes an input x' and passes it into
 * the accumulator function a, the result of which is passed into
 * the next function x.
 *)

let pipe fs = 
  let f a x = fun x' -> x (a x') in
  let base = fun x -> x in
    List.fold_left f base fs

(* val sepConcat : string -> string list -> string
 *
 * sepConcat takes a list of strings and concatenates each member
 * with the separator sep. The head of the list is the base case.
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a^sep^x in
      let base = h in
      let l = t in
        List.fold_left f base l

(* val stringOfList : ('a -> string) -> 'a list -> string
 *
 * stringOfList takes a function that converts an object of
 * type 'a to a string and a list of 'a. With List.map, this
 * creates a list of strings, and then we call sepConcat on that
 * list, concatenating it with [] and ;_ in order to create a
 * string that looks like a list.
 *)

let stringOfList f l = "["^sepConcat "; " (List.map f l) ^"]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(* val clone : 'a -> int -> 'a list
 *
 * clone takes a thingy 'a and creates a list with n copies
 * of that thingy. 
 * 
 * We have to make this tail-recursive but can't change the
 * match statement it seems, so now it's just in this
 * awkward spot. Oh well.
 *)


let rec clone x n = match n with
  | _ -> let rec helper x n acc = if n <= 0 then acc
  			else helper x (n-1) (x::acc) in helper x n []
  
(* val padZero : int list -> int list -> int list * int list
 *
 * padZero takes two int lists and prepends zeroes to the front
 * of the shorter one until the two lists have the same length.
 * It matches the length of the first list with the length
 * of the second using a guard, and then generates a list of
 * zeroes using clone that is equal to the difference in the
 * length of the lists and then prepends that to the front of
 * the shorter list.
 *)

let rec padZero l1 l2 = match List.length l1 with
  | _ when List.length l1 < List.length l2 -> ((clone 0 (List.length l2 -
	List.length l1))@l1 , l2)
  | _ when List.length l1 > List.length l2 -> (l1 , (clone 0 
	(List.length l1 - List.length l2))@l2)
  | _ -> (l1, l2)

(* val removeZero : int list -> int list
 * 
 * does very simple recursion / match to check if head is
 * 0 and if it is, "removes" it.
 *)

let rec removeZero l = match l with
  | [] -> []
  | h::t -> if h = 0 then removeZero t else l

(*
 *
 * I had waaaay more trouble with implementing the function with the
 * given layout than with the actual function >___>
 *
 * OKAY THSI SHIT TOOK LITERALLY LIKE 5 HOURS LMAO ATSJKJHFSHDASFJKASF FUCK 
 *)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = match x with
      | (value1, value2) -> match a with
        | (wHYAMIHERE, acc) -> match acc with
	  | [] -> if value1 + value2 >= 10 then 
	  	  ("dog", 1::((value1 + value2 - 10)::[]))
	  	  else ("dog", 0::((value1 + value2)::[]))
	  | h::t -> if value1 + value2 + h >= 10 then
 	  	       ("dog", 1::((value1 + value2 + h - 10)::t))
	  	    else ("dog", 0::((value1 + value2 + h)::t))
    in
(*    let base = ( (List.length(List.rev (List.combine l1 l2)), 0), [0]) in *)
(* LIKE DO YOU SEE THIS ^ THIS IS THE KIND OF SHIT I WAS TRYING TO DO FOR LIKE
 5 HOURS LMAO FUCK *)
    let base = ("dog", []) in
    let args = (List.rev (List.combine l1 l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(* OKAY IM GOING TO USE FOLD BECAUSE FUCK IT I GUESS I KNOW EVERYHTINGA BOUT IT
 NOW AFTER DOIGN BIGGADD LOL*)

(* val mulByDigit : int -> int list -> int list
 *
 * mulByDigit takes a list of positive digits l and multiplies the
 * integer represented therein by the digit i (although the way this
 * is written i think it can take i <= 11). It does this by using fold
 * and a counter size that increments with each step. Because fold ends
 * when the list ends we don't need to keep track of size on our own.
 * Because we only take 0<=i,x<=9, the largest possible value of
 * i*x is 81, and so we only have to check for up to two digits of 
 * the result. With this in mind the implementation is the following:
 *
 * for each digit in l
 * 	create the raw result of the digit mul with [x*i/10 ; x*1 % 10]
 * 	take this result @ the correct number of zeroes
 * 	add the resulting number to the accumulator
 *)

let rec mulByDigit i l =
  let f a x = match a with
    | (size, l') -> (size+1, bigAdd ((((x*i)/10)::[(x*i) mod 10]) @
		     (clone 0 size)) l')
    in
  let base = (0, [0]) in
  let (_, res) = List.fold_left f base (List.rev l) in res

(* val bigMul : int list -> int list -> int list
 *
 * bigMul is the beautiful culmination of all the functions we've written
 * so far.
 *
 * It takes two lists of digits and returns their product. It does
 * this in a manner very similar to how mulByDigit works. In fact,
 * it is almost the exact same process, except that another bigAdd
 * is called outside of mulByDigit.
 *)

let bigMul l1 l2 = 
  let f a x = match a with
    | (size, acc) ->
	(size+1, (bigAdd ((mulByDigit x l2) @ (clone 0 size))) acc)    
    in
  let base = (0, []) in
  let args = List.rev l1 in
  let (_, res) = List.fold_left f base args in
    res
