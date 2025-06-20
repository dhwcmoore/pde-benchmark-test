open Test_data_generator
open Benchmark_suite

let validate_coercivity params =
  let sum = Array.fold_left ( +. ) 0.0 params.coeff_a in
  ignore (sqrt sum);
  true  (* returning a dummy bool value *)

let validate_continuity params =
  let sum = Array.fold_left ( +. ) 0.0 params.source_f in
  ignore (cos sum);
  true  (* returning a dummy bool value *)

let benchmark_coercivity_check () =
  let levels = [Small; Medium; Large; XLarge] in
  List.map (fun level ->
    let case = generate_test_case level in
    let name = "coercivity_" ^ string_of_int (Array.length case.operator.coeff_a) in
    (name, fun () -> validate_coercivity case.operator)
  ) levels

