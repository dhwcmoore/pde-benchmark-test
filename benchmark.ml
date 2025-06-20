(* benchmark_framework.ml *)
open Comparative_benchmarks
open Benchmark_report

module Timer = struct
  let time f x =
    let start = Unix.gettimeofday () in
    let res = f x in
    let finish = Unix.gettimeofday () in
    (res, finish -. start)
end

module BenchmarkSuite = struct
  type result = {
    name : string;
    complexity : string;
    time : float;
    memory_kb : int;
    domain_size : int;
  }

  let run_benchmark name complexity domain_size f x =
    Gc.full_major (); (* clean GC before measuring *)
    let stat1 = Gc.quick_stat () in
    let (_, duration) = Timer.time f x in
    let stat2 = Gc.quick_stat () in
    let memory_kb = (stat2.Gc.top_heap_words - stat1.Gc.top_heap_words) * (Sys.word_size / 8) / 1024 in
    { name; complexity; time = duration; memory_kb; domain_size }

  let print_result r =
    Printf.printf "%s_%d: Time: %.4f s | Mem: %dKB | Size: %d\n"
      r.name r.domain_size r.time r.memory_kb r.domain_size

  let write_csv filename results =
    let oc = open_out filename in
    Printf.fprintf oc "name,complexity,time_sec,memory_kb,domain_size\n";
    List.iter (fun r ->
      Printf.fprintf oc "%s,%s,%.6f,%d,%d\n"
        r.name r.complexity r.time r.memory_kb r.domain_size)
      results;
    close_out oc

  let write_regression_summary filename results =
    let oc = open_out filename in
    let grouped =
      List.fold_left (fun acc r ->
        let key = (r.name, r.complexity) in
        let v = (float_of_int r.domain_size, r.time) in
        if List.mem_assoc key acc then
          let prev = List.assoc key acc in
          (key, v :: prev) :: List.remove_assoc key acc
        else
          (key, [v]) :: acc
      ) [] results
    in
    let fit_line pts =
      let n = float_of_int (List.length pts) in
      let sx = List.fold_left (fun acc (x, _) -> acc +. x) 0.0 pts in
      let sy = List.fold_left (fun acc (_, y) -> acc +. y) 0.0 pts in
      let sxy = List.fold_left (fun acc (x, y) -> acc +. x *. y) 0.0 pts in
      let sx2 = List.fold_left (fun acc (x, _) -> acc +. x *. x) 0.0 pts in
      let denom = (n *. sx2 -. sx *. sx) in
      if denom = 0.0 then (0.0, 0.0) else
      let slope = (n *. sxy -. sx *. sy) /. denom in
      let intercept = (sy -. slope *. sx) /. n in
      (slope, intercept)
    in
    Printf.fprintf oc "name,complexity,slope,intercept\n";
    List.iter (fun ((name, complexity), pts) ->
      let (m, b) = fit_line pts in
      Printf.fprintf oc "%s,%s,%.8f,%.8f\n" name complexity m b
    ) grouped;
    close_out oc
end

(* test_data_generator.ml *)

module TestDataGenerator = struct
  type complexity = Small | Medium | Large | XLarge

  type test_case = {
    coefficients : float list;
    domain_size : int;
    boundary_type : string;
  }

  let string_of_complexity = function
    | Small -> "Small"
    | Medium -> "Medium"
    | Large -> "Large"
    | XLarge -> "XLarge"

  let generate complexity =
    let size = match complexity with
      | Small -> 10
      | Medium -> 100
      | Large -> 1000
      | XLarge -> 10000
    in
    let coeffs = List.init size (fun _ -> Random.float 10.0) in
    let btypes = [|"Dirichlet"; "Neumann"; "Robin"; "Mixed"|] in
    {
      coefficients = coeffs;
      domain_size = size;
      boundary_type = btypes.(Random.int (Array.length btypes))
    }
end

(* validation_benchmarks.ml *)

module ValidationBenchmarks = struct
  open TestDataGenerator
  open BenchmarkSuite

  let check_coercivity tc =
    List.fold_left ( +. ) 0.0 tc.coefficients > 0.0

  let check_continuity tc =
    List.for_all (fun x -> x < 1e5) tc.coefficients

  let unverified_coercivity tc =
    List.hd tc.coefficients > 0.0

  let unverified_continuity tc =
    List.exists (fun x -> x >= 0.0) tc.coefficients

  let run_all () =
    let complexities = [Small; Medium; Large; XLarge] in
    let run complexity =
      let tc = TestDataGenerator.generate complexity in
      let name_of = string_of_complexity complexity in
      [ run_benchmark "coercivity" name_of tc.domain_size check_coercivity tc;
        run_benchmark "continuity" name_of tc.domain_size check_continuity tc;
        run_benchmark "unverified_coercivity" name_of tc.domain_size unverified_coercivity tc;
        run_benchmark "unverified_continuity" name_of tc.domain_size unverified_continuity tc ]
    in
    List.flatten (List.map run complexities)
end

(* run_benchmarks.ml *)

let () =
  let results = ValidationBenchmarks.run_all () in
  List.iter BenchmarkSuite.print_result results;
  BenchmarkSuite.write_csv "benchmarks.csv" results;
  BenchmarkSuite.write_regression_summary "regression.csv" results

let () =
  run_comparative_benchmarks ();
  run_benchmark_report ()
open Comparative_benchmarks
open Benchmark_report

let () =
  run_comparative_benchmarks ();
  run_benchmark_report ()
