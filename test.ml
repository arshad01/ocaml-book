let assert_true res testname =
  if res then
    print_string ("Test " ^ testname ^ " passed\n")
  else
    print_string ("Test " ^ testname ^ " failed\n");;

let assert_false res testname = assert_true (not res) testname;;