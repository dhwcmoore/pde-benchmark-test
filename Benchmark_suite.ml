let print_result name time mem size =
  Printf.printf "%s: Time = %.6f s | Mem: %dKB | Size: %d\n" name time mem size

let print_header title =
  print_endline ("\n==== " ^ title ^ " ====")

let measure_time f x =
  let open Unix in
  let start = gettimeofday () in
  let _ = f x in
  let stop = gettimeofday () in
  stop -. start

let measure_mem f x =
  let stat_before = Gc.quick_stat () in
  let _ = f x in
  let stat_after = Gc.quick_stat () in
  let words = stat_after.minor_words +. stat_after.major_words 
              -. stat_before.minor_words -. stat_before.major_words in
  int_of_float (words *. (float_of_int (Sys.word_size / 8)) /. 1024.0)
