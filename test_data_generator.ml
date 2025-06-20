type complexity_level = Small | Medium | Large | XLarge

type operator_params = {
  coeff_a : float array;
  source_f : float array;
}

type domain_spec = {
  domain_type : string;
  dimensions : int;
}

type boundary_condition = {
  bc_type : string;
  values : float array;
}

let size_of = function
  | Small -> 10
  | Medium -> 100
  | Large -> 1000
  | XLarge -> 10000

let generate_operator_params level =
  let n = size_of level in
  let coeff_a = Array.init n (fun i -> 1.0 +. float i /. 100.0) in
  let source_f = Array.init n (fun i -> sin (float i)) in
  { coeff_a; source_f }
  
let generate_boundary_conditions level =
  match level with
  | Small -> [| 1.0; 1.0 |]
  | Medium -> Array.make 4 1.0
  | Large -> Array.make 8 1.0
  | XLarge -> Array.make 16 1.0

type test_case = {
  level : complexity_level;
  operator : operator_params;
  boundary : float array;
}

let generate_test_case level =
  {
    level;
    operator = generate_operator_params level;
    boundary = generate_boundary_conditions level;
  }
