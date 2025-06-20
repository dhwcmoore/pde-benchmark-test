open Unix
open Printf
open Test_data_generator
open Validation_benchmarks

let time f x =
  let start = Unix.gettimeofday () in
  let _ = f x in
  Unix.gettimeofday () -. start

let sizes = [10; 100; 1000; 10000; 100000]

let write_csv filename rows =
  let oc = open_out filename in
  fprintf oc "size,coercivity_time,continuity_time\n";
  List.iter (fun (size, t1, t2) ->
    fprintf oc "%d,%.6f,%.6f\n" size t1 t2
  ) rows;
  close_out oc

let run_benchmark_report () =
  print_endline ">>> running report benchmark...";  (* 👈 debug print *)

  let results = List.map (fun size ->
    let op = generate_operator_params size in
    let t1 = time validate_coercivity op in
    let t2 = time validate_continuity op in
    (size, t1, t2)
  ) sizes in

  write_csv "benchmarks/benchmark_report.csv" results;

  print_endline "CSV written to benchmarks/benchmark_report.csv"

