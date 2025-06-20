open Unix
open Test_data_generator
open Validation_benchmarks

let time f x =
  let start = Unix.gettimeofday () in
  let _ = f x in
  Unix.gettimeofday () -. start

let unverified_coercivity op =
  op.alpha > 0.0 && op.beta > 0.0

let unverified_continuity op =
  op.alpha < 10000.0 && op.beta < 10000.0

let run_comparative_benchmarks () =
  let sizes = [10; 100; 1000; 10000] in
  List.iter (fun size ->
    let op = generate_operator_params size in
    let t1 = time validate_coercivity op in
    let t2 = time unverified_coercivity op in
    Printf.printf "[Coercivity] Verified: %.6fs | Unverified: %.6fs | Size: %d\n" t1 t2 size;

    let t3 = time validate_continuity op in
    let t4 = time unverified_continuity op in
    Printf.printf "[Continuity] Verified: %.6fs | Unverified: %.6fs | Size: %d\n" t3 t4 size;

    print_endline "---"
  ) sizes
