let token_list_of_string s =
  let lb = Lexing.from_string s in
  let rec helper l = 
	try 
	  let t = NanoLex.token lb in
	  if t = NanoParse.EOF then List.rev l else helper (t::l)
	with _ -> List.rev l
  in
    helper []

let filename_to_expr f = 
  NanoParse.exp NanoLex.token (Lexing.from_channel (open_in f))

let string_to_expr s =
  NanoParse.exp NanoLex.token (Lexing.from_string s)


let string_to_python s = 
  try
	Nano.exprToPython (string_to_expr s)
  with exn -> "Error: "^(Printexc.to_string exn)

let _ =
  (* Printf.printf "NanoML \n \n";
  Printf.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv)); *)
  try Printf.printf "%s" (string_to_python Sys.argv.(1))
  with _ -> ()
