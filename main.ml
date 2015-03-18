let rec read_file = function (fd) ->
    try
      let line = input_line fd in
      print_endline line;
      read_file fd;
    with End_of_file -> ()

let check_file file argc =
  if (Sys.file_exists file) = false then
    begin
      print_endline "Error: file doesn't exist";
      exit 1
    end
  else
    begin
      let fd = open_in file in
      read_file fd;
      close_in fd;
    end

let read_in =
  try
    while true do
      let line = input_line stdin in
      print_endline line;
      if line = "quit" then exit 1
    done;
  with
    End_of_file -> ()

let main =
  let argv = Array.to_list Sys.argv in
  let argc = List.length argv in
  if argc = 3 then
    check_file (List.nth argv 2) argc
  else
    read_in
  

    (*
    print_int argc; (* Print le nb d'arguments *)
    print_endline "";
    print_endline (List.nth argv (argc - 1)); (* Print le dernier argument *)
     *)
