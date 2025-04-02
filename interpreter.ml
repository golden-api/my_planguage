open Ast

exception DimensionError of string
exception EvalError of string
exception BreakException
exception ContinueException
exception TypeMismatch of string

type value =
  | BoolV of bool
  | IntV of int
  | FloatV of float
  | VectorV of int * value list
  | MatrixV of int * int * value list list

type environment = (string, value) Hashtbl.t

let to_float v =
  match v with
  | IntV i -> float_of_int i
  | FloatV f -> f
  | _ -> failwith "to_float: value is not numeric"

let rec string_of_value v =
  match v with
  | BoolV b -> string_of_bool b
  | IntV i -> string_of_int i
  | FloatV f -> string_of_float f
  | VectorV (_, vs) ->
      "[" ^ (String.concat ", " (List.map string_of_value vs)) ^ "]"
  | MatrixV (_, _, rows) ->
      "[" ^ (String.concat "\n " (
        List.map (fun row ->
          "[" ^ (String.concat ", " (List.map string_of_value row)) ^ "]"
        ) rows
      )) ^ "]"

let dot_product v1 v2 =
  match (v1, v2) with
  | (VectorV (_, vs1), VectorV (_, vs2)) ->
      let products = List.map2 (fun a b ->
        match (a, b) with
        | (IntV i1, IntV i2) -> IntV (i1 * i2)
        | (FloatV f1, FloatV f2) -> FloatV (f1 *. f2)
        | _ -> failwith "dot_product: unreachable"
      ) vs1 vs2 in
      List.fold_left (fun acc x ->
        match (acc, x) with
        | (IntV a, IntV b) -> IntV (a + b)
        | (FloatV a, FloatV b) -> FloatV (a +. b)
        | _ -> failwith "dot_product: unreachable"
      ) (IntV 0) products
  | _ -> failwith "dot_product: expects two vectors"

let magnitude v =
  match v with
  | VectorV (_, vs) ->
      let sum_sq = List.fold_left (fun acc x ->
        let f = to_float x in
        acc +. (f *. f)
      ) 0.0 vs in
      FloatV (sqrt sum_sq)
  | _ -> failwith "magnitude: expects a vector"

let angle_between v1 v2 =
  let dp = dot_product v1 v2 in
  let mag1 = magnitude v1 in
  let mag2 = magnitude v2 in
  let dp_f = to_float dp in
  let m1 = to_float mag1 in
  let m2 = to_float mag2 in
  if m1 = 0.0 || m2 = 0.0 then
    failwith "angle_between: zero magnitude vector"
  else
    FloatV (acos (dp_f /. (m1 *. m2)))

let transpose v =
  match v with
  | MatrixV (n, m, rows) ->
      let rec aux mat =
        if List.exists ((=) []) mat then []
        else
          let heads = List.map List.hd mat in
          let tails = List.map List.tl mat in
          heads :: aux tails
      in
      MatrixV (m, n, aux rows)
  | _ -> failwith "transpose: expects a matrix"

let determinant v =
  let rec det matrix =
    match matrix with
    | [[x]] -> to_float x
    | m ->
        let n = List.length m in
        let rec aux i acc =
          if i >= n then acc
          else
            let x = List.nth (List.hd m) i in
            let sign = if i mod 2 = 0 then 1.0 else -1.0 in
            let submatrix =
              List.map (fun row ->
                let rec remove_ith j = function
                  | [] -> []
                  | y :: ys -> if j = i then ys else y :: remove_ith (j+1) ys
                in
                remove_ith 0 row
              ) (List.tl m)
            in
            aux (i+1) (acc +. sign *. to_float x *. det submatrix)
        in
        aux 0 0.0
  in
  match v with
  | MatrixV (_, _, rows) -> FloatV (det rows)
  | _ -> failwith "determinant: expects a matrix"

let inverse v =
  match v with
  | MatrixV (n, m, rows) ->
      if n = 0 then failwith "inverse: empty matrix"
      else if n <> m then failwith "inverse: non-square matrix"
      else
        let mat = Array.of_list (
          List.map (fun row ->
            Array.of_list (List.map to_float row)
          ) rows
        ) in
        let identity = Array.init n (fun i ->
          Array.init n (fun j -> if i = j then 1.0 else 0.0)
        ) in
        let swap_rows i j =
          let tmp = mat.(i) in
          mat.(i) <- mat.(j);
          mat.(j) <- tmp;
          let tmp = identity.(i) in
          identity.(i) <- identity.(j);
          identity.(j) <- tmp
        in
        for i = 0 to n - 1 do
          if mat.(i).(i) = 0.0 then (
            let rec find_nonzero j =
              if j >= n then failwith "inverse: matrix is singular"
              else if mat.(j).(i) <> 0.0 then j else find_nonzero (j+1)
            in
            let j = find_nonzero (i+1) in
            swap_rows i j
          );
          let pivot = mat.(i).(i) in
          for j = 0 to n - 1 do
            mat.(i).(j) <- mat.(i).(j) /. pivot;
            identity.(i).(j) <- identity.(i).(j) /. pivot;
          done;
          for k = 0 to n - 1 do
            if k <> i then
              let factor = mat.(k).(i) in
              for j = 0 to n - 1 do
                mat.(k).(j) <- mat.(k).(j) -. factor *. mat.(i).(j);
                identity.(k).(j) <- identity.(k).(j) -. factor *. identity.(i).(j);
              done
          done
        done;
        let inv_rows =
          Array.to_list (Array.map (fun row ->
            Array.to_list row |> List.map (fun f -> FloatV f)
          ) identity)
        in
        MatrixV (n, n, inv_rows)
  | _ -> failwith "inverse: expects a matrix"

let parse_vector_data str =
  let clean_str = String.trim str in
  if String.length clean_str < 2 || clean_str.[0] <> '[' || 
     clean_str.[String.length clean_str - 1] <> ']' then
    failwith "Invalid vector format: missing brackets";
  let content = String.sub clean_str 1 (String.length clean_str - 2) in
  let elements = String.split_on_char ',' content in
  List.map (fun s -> FloatV (float_of_string (String.trim s))) elements

let parse_matrix_data str =
  let clean_str = String.trim str in
  if String.length clean_str < 2 ||
     clean_str.[0] <> '[' ||
     clean_str.[String.length clean_str - 1] <> ']' then
    failwith "Invalid matrix format: missing outer brackets";
  let content = String.sub clean_str 1 (String.length clean_str - 2) in
  let rec extract_rows s pos acc =
    if pos >= String.length s then List.rev acc
    else
      match String.index_from_opt s pos '[' with
      | None -> List.rev acc
      | Some open_idx ->
          let rec find_matching_bracket idx level =
            if idx >= String.length s then failwith "Invalid matrix format: unbalanced brackets"
            else match s.[idx] with
              | '[' -> find_matching_bracket (idx + 1) (level + 1)
              | ']' -> if level = 1 then idx else find_matching_bracket (idx + 1) (level - 1)
              | _ -> find_matching_bracket (idx + 1) level
          in
          let close_idx = find_matching_bracket open_idx 0 in
          let row_content = String.sub s (open_idx + 1) (close_idx - open_idx - 1) in
          let row_elements =
            String.split_on_char ',' row_content
            |> List.map (fun s -> FloatV (float_of_string (String.trim s)))
          in
          extract_rows s (close_idx + 1) (row_elements :: acc)
  in
  extract_rows content 0 []

let rec update_index container indices new_value =
  match (container, indices) with
  | MatrixV (rows, cols, data), [i] -> 
      if i < 0 || i >= rows then
        raise (Failure "Matrix row index out of bounds");
      (match new_value with
       | VectorV (v_dim, v_elements) ->
           if v_dim <> cols then
             raise (Failure "Row vector dimension mismatch");
           MatrixV (rows, cols, 
             List.mapi (fun idx row -> 
               if idx = i then v_elements else row
             ) data)
       | _ -> raise (Failure "Matrix row must be a vector"))

  | MatrixV (rows, cols, data), [i; j] ->
      if i < 0 || i >= rows || j < 0 || j >= cols then
        raise (Failure "Matrix index out of bounds");
      let new_data = 
        List.mapi (fun row_idx row ->
          if row_idx = i then
            List.mapi (fun col_idx el -> 
              if col_idx = j then new_value else el
            ) row
          else row
        ) data in
      MatrixV (rows, cols, new_data)
  | VectorV (dim, elements), [i] ->
      if i < 0 || i >= dim then
        raise (Failure "Vector index out of bounds");
      VectorV (dim, 
        List.mapi (fun idx el -> if idx = i then new_value else el) elements)

  | VectorV _, _ -> raise (Failure "Invalid index count for vector")
  | MatrixV _, _ -> raise (Failure "Invalid index count for matrix")
  | _ -> raise (Failure "Cannot update non-vector/matrix")

(* Expression evaluation *)
let rec eval_expr (e : expr) (env : environment) : value =
  match e with
  | ConstBool b -> BoolV b
  | ConstInt i -> IntV i
  | ConstFloat f -> FloatV f
  | ConstV es ->
      VectorV (List.length es, List.map (fun e -> eval_expr e env) es)
  | ConstM rows ->
      let matrix_data = List.map (fun row -> List.map (fun e -> eval_expr e env) row) rows in
      let row_count = List.length matrix_data in
      let col_count = match matrix_data with [] -> 0 | hd :: _ -> List.length hd in
      if List.exists (fun row -> List.length row <> col_count) matrix_data then
        raise (DimensionError "Matrix rows have inconsistent column counts")
      else
        MatrixV (row_count, col_count, matrix_data)
  | Var s ->
      (try Hashtbl.find env s with Not_found -> failwith ("Variable " ^ s ^ " not found"))
  | BinOp (e1, op, e2) ->
      let v1 = eval_expr e1 env in
      let v2 = eval_expr e2 env in
      (match op with
       | Add ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> IntV (i1 + i2)
            | (FloatV f1, FloatV f2) -> FloatV (f1 +. f2)
            | (VectorV (n, vs1), VectorV (m, vs2)) ->
                if n <> m then raise (DimensionError "Vector addition: incompatible dimensions")
                else VectorV (n, List.map2 (fun a b ->
                  match (a, b) with
                  | (IntV i1, IntV i2) -> IntV (i1 + i2)
                  | (FloatV f1, FloatV f2) -> FloatV (f1 +. f2)
                  | _ -> failwith "Addition: mismatched vector element types"
                ) vs1 vs2)
            | (MatrixV (n, m, m1), MatrixV (p, q, m2)) ->
                if n <> p then raise (DimensionError "Matrix addition: incompatible row counts");
                if m <> q then raise (DimensionError "Matrix addition: incompatible column counts");
                MatrixV (n, m, List.map2 (fun row1 row2 ->
                  List.map2 (fun a b ->
                    match (a, b) with
                    | (IntV i1, IntV i2) -> IntV (i1 + i2)
                    | (FloatV f1, FloatV f2) -> FloatV (f1 +. f2)
                    | _ -> failwith "Addition: mismatched matrix element types"
                  ) row1 row2
                ) m1 m2)
            | (BoolV b1, BoolV b2) -> BoolV (b1 || b2)
            | _ -> failwith "Addition: incompatible types")
       | Sub ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> IntV (i1 - i2)
            | (FloatV f1, FloatV f2) -> FloatV (f1 -. f2)
            | (VectorV (n, vs1), VectorV (m, vs2)) ->
                if n <> m then raise (DimensionError "Vector subtraction: incompatible dimensions")
                else VectorV (n, List.map2 (fun a b ->
                  match (a, b) with
                  | (IntV i1, IntV i2) -> IntV (i1 - i2)
                  | (FloatV f1, FloatV f2) -> FloatV (f1 -. f2)
                  | _ -> failwith "Subtraction: mismatched vector element types"
                ) vs1 vs2)
            | (MatrixV (n, m, m1), MatrixV (p, q, m2)) ->
                if n <> p then raise (DimensionError "Matrix subtraction: incompatible row counts");
                if m <> q then raise (DimensionError "Matrix subtraction: incompatible column counts");
                MatrixV (n, m, List.map2 (fun row1 row2 ->
                  List.map2 (fun a b ->
                    match (a, b) with
                    | (IntV i1, IntV i2) -> IntV (i1 - i2)
                    | (FloatV f1, FloatV f2) -> FloatV (f1 -. f2)
                    | _ -> failwith "Subtraction: mismatched matrix element types"
                  ) row1 row2
                ) m1 m2)
            | _ -> failwith "Subtraction: incompatible types")
       | Mul ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> IntV (i1 * i2)
            | (FloatV f1, FloatV f2) -> FloatV (f1 *. f2)
            | (BoolV b1, BoolV b2) -> BoolV (b1 && b2)
            | (MatrixV (n, m, m1), MatrixV (p, q, m2)) ->
                if n <> p then raise (DimensionError "Matrix multiplication: incompatible row counts");
                if m <> q then raise (DimensionError "Matrix multiplication: incompatible column counts");
                MatrixV (n, m, List.map2 (fun row1 row2 ->
                  List.map2 (fun a b ->
                    match (a, b) with
                    | (IntV i1, IntV i2) -> IntV (i1 * i2)
                    | (FloatV f1, FloatV f2) -> FloatV (f1 *. f2)
                    | _ -> failwith "Multiplication: mismatched matrix element types"
                  ) row1 row2
                ) m1 m2)
            | _ -> failwith "Multiplication: not defined for these types")
       | Div ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) ->
                if i2 = 0 then failwith "Division: division by zero" else IntV (i1 / i2)
            | (FloatV f1, FloatV f2) ->
                if f2 = 0.0 then failwith "Division: division by zero" else FloatV (f1 /. f2)
            | _ -> failwith "Division: undefined for these types")
       | Mod ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) ->
                if i2 = 0 then failwith "Modulus: division by zero" else IntV (i1 mod i2)
            | _ -> failwith "Modulus: not defined for non-integers")
       | And ->
           (match (v1, v2) with
            | (BoolV b1, BoolV b2) -> BoolV (b1 && b2)
            | _ -> failwith "Logical and: both operands must be booleans")
       | Or ->
           (match (v1, v2) with
            | (BoolV b1, BoolV b2) -> BoolV (b1 || b2)
            | _ -> failwith "Logical or: both operands must be booleans")
       | Lt ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> BoolV (i1 < i2)
            | (FloatV f1, FloatV f2) -> BoolV (f1 < f2)
            | _ -> failwith "Less-than: incompatible types")
       | Gt ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> BoolV (i1 > i2)
            | (FloatV f1, FloatV f2) -> BoolV (f1 > f2)
            | _ -> failwith "Greater-than: incompatible types")
       | Lte ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> BoolV (i1 <= i2)
            | (FloatV f1, FloatV f2) -> BoolV (f1 <= f2)
            | _ -> failwith "Less-than-or-equal: incompatible types")
       | Gte ->
           (match (v1, v2) with
            | (IntV i1, IntV i2) -> BoolV (i1 >= i2)
            | (FloatV f1, FloatV f2) -> BoolV (f1 >= f2)
            | _ -> failwith "Greater-than-or-equal: incompatible types")
       | Eq -> BoolV (v1 = v2)
       | Dot -> dot_product v1 v2
       | Angle -> angle_between v1 v2)
  | UnOp (op, e1) ->
      let v = eval_expr e1 env in
      (match op with
       | Neg ->
           (match v with
            | IntV i -> IntV (-i)
            | FloatV f -> FloatV (-. f)
            | VectorV (n, vs) ->
                VectorV (n, List.map (fun x ->
                  match x with
                  | IntV i -> IntV (-i)
                  | FloatV f -> FloatV (-. f)
                  | _ -> failwith "Negation: invalid vector element"
                ) vs)
            | _ -> failwith "Negation: not defined for this type")
       | Not ->
           (match v with
            | BoolV b -> BoolV (not b)
            | _ -> failwith "Not: operand must be a boolean")
       | Magnitude -> magnitude v
       | Dimension ->
           (match v with
            | VectorV (n, _) -> IntV n
            | MatrixV (n, _, _) -> IntV n
            | _ -> failwith "Dimension: operand must be a vector or matrix")
       | Transpose -> transpose v
       | Determinant -> determinant v
       | Inverse -> inverse v)
  | Assign (s, e1) ->
      let v = eval_expr e1 env in
      Hashtbl.replace env s v;
      v
  | Input (spec, filename) ->
      let read_input_source source =
        match source with
        | None ->
            print_string "Enter expr: ";
            read_line ()
        | Some source ->
            let ic = open_in source in
            let content = ref [] in
            (try
               while true do
                 content := (input_line ic) :: !content
               done;
               ""
             with End_of_file ->
               close_in ic;
               String.concat "\n" (List.rev !content))
      in
      let input_text = read_input_source filename in
      (try
         match spec with
         | TV ->
             let lines = String.split_on_char '\n' input_text in
             (match lines with
              | [dim_line; data_line] ->
                  let actual_dim = int_of_string (String.trim dim_line) in
                  let vector_data = parse_vector_data data_line in
                  if List.length vector_data <> actual_dim then
                    raise (DimensionError "Vector data length doesn't match declared dimension")
                  else
                    VectorV (actual_dim, vector_data)
              | _ -> raise (Failure "Invalid vector input format"))
         | TM ->
             let lines = String.split_on_char '\n' input_text in
             (match lines with
              | [dim_line; data_line] ->
                  let dims = String.split_on_char ',' dim_line in
                  if List.length dims <> 2 then
                    raise (Failure "Invalid matrix dimension format")
                  else
                    let actual_rows = int_of_string (String.trim (List.nth dims 0)) in
                    let actual_cols = int_of_string (String.trim (List.nth dims 1)) in
                    let matrix_data = parse_matrix_data data_line in
                    if List.length matrix_data <> actual_rows then
                      raise (DimensionError "Row count mismatch")
                    else if List.exists (fun row -> List.length row <> actual_cols) matrix_data then
                      raise (DimensionError "Column count mismatch")
                    else
                      MatrixV (actual_rows, actual_cols, matrix_data)
              | _ -> raise (Failure "Invalid matrix input format"))
         | TInt ->
             let trimmed = String.trim input_text in
             IntV (int_of_string trimmed)
         | TFloat ->
             let trimmed = String.trim input_text in
             FloatV (float_of_string trimmed)
         | TBool ->
             let trimmed = String.trim input_text in
             (match String.lowercase_ascii trimmed with
              | "true" -> BoolV true
              | "false" -> BoolV false
              | _ -> raise (Failure "Invalid boolean value"))
       with
       | Failure msg -> failwith ("Input error: " ^ msg)
       | Invalid_argument msg -> failwith ("Input error: " ^ msg)
       | e -> raise e)
  | Index (target, indices) ->
      let target_val = eval_expr target env in
      let indices_int = List.map (fun idx ->
        match eval_expr idx env with
        | IntV i -> i
        | _ -> raise (Failure "Index must be integer")
      ) indices in
      (match target_val with
       | VectorV (_, elements) ->
           if List.length indices_int <> 1 then
             raise (Failure "Vector indexing requires 1 index")
           else
             let i = List.hd indices_int in
             if i < 0 || i >= List.length elements then
               raise (Failure "Vector index out of bounds")
             else
               List.nth elements i
       | MatrixV (_, _, rows) ->
           let i = List.hd indices_int in
           if i < 0 || i >= List.length rows then
             raise (Failure "Matrix row index out of bounds")
           else
             let row = List.nth rows i in
             if List.length indices_int = 1 then
               VectorV (List.length row, row)
             else
               let j = List.nth indices_int 1 in
               if j < 0 || j >= List.length row then
                 raise (Failure "Matrix column index out of bounds")
               else
                 List.nth row j
       | _ -> raise (Failure "Cannot index non-vector/matrix"))
    | Ast.AssignIdx (Ast.Index (Ast.Var s, indices_expr), e1) ->
      let new_value = eval_expr e1 env in
      let container = Hashtbl.find env s in
      let indices = List.map (fun idx_expr ->
          match eval_expr idx_expr env with
         | IntV i -> i
         | _ -> raise (Failure "Index must be integer")
      )  indices_expr in
      let new_container = update_index container indices new_value in
      Hashtbl.replace env s new_container;
      new_value
      
  | _ -> raise (Failure "Invalid args")

(* Statement evaluation *)
let rec eval_statement (s : statement) (env : environment) : environment =
  match s with
  | Expr e ->
      let _ = eval_expr e env in env
  | Block stmts ->
      let saved_env = Hashtbl.copy env in
      let final_env =
        try List.fold_left (fun acc stmt -> eval_statement stmt acc) env stmts
        with
        | BreakException | ContinueException as ex -> raise ex
      in
      (* Merge final values back into saved_env *)
      Hashtbl.iter (fun k v ->
        if Hashtbl.mem saved_env k then Hashtbl.replace saved_env k v
      ) final_env;
      saved_env
  | If (cond, s1, s2_opt) ->
      (match eval_expr cond env with
       | BoolV true -> eval_statement s1 env
       | BoolV false ->
           (match s2_opt with
            | Some s2 -> eval_statement s2 env
            | None -> env)
       | _ -> raise (TypeMismatch "If condition must be boolean"))
  | While (cond, s_body) ->
      let rec loop env =
        match eval_expr cond env with
        | BoolV true ->
            (try
               let new_env = eval_statement s_body env in
               loop new_env
             with
             | BreakException -> env
             | ContinueException -> loop env)
        | BoolV false -> env
        | _ -> raise (TypeMismatch "While condition must be boolean")
      in
      loop env
  | For (var, e1, e2, s_body) ->
      let start_val =
        match eval_expr e1 env with
        | IntV i -> i
        | _ -> raise (TypeMismatch "For loop start must be numeric")
      in
      let end_val =
        match eval_expr e2 env with
        | IntV i -> i
        | _ -> raise (TypeMismatch "For loop end must be numeric")
      in
      let rec loop i env_loop =
        if i > end_val then env
        else
          let local_env = Hashtbl.copy env_loop in
          Hashtbl.replace local_env var (IntV i);
          (try
             let new_env = eval_statement s_body local_env in
             Hashtbl.iter (fun k v ->
               if k <> var && Hashtbl.mem env k then Hashtbl.replace env k v
             ) new_env;
             loop (i+1) env
           with
           | BreakException -> env
           | ContinueException -> loop (i+1) env)
      in
      loop start_val env
  | Print e ->
      let v = eval_expr e env in
      print_endline (string_of_value v);
      env
  | Break -> raise BreakException
  | Continue -> raise ContinueException
  | Vector (dim, s, e) ->
      let v = eval_expr e env in
      (match v with
       | VectorV (d, lst) ->
           if List.length lst <> dim then
             raise (DimensionError ("Vector declaration dimension mismatch for variable " ^ s))
           else
             Hashtbl.replace env s v; env
       | _ -> raise (TypeMismatch ("Vector declaration requires a vector literal for variable " ^ s)))
  | Matrix (r, c, s, e) ->
      let v = eval_expr e env in
      (match v with
       | MatrixV (_, _, rows) ->
           if List.length rows <> r || List.exists (fun row -> List.length row <> c) rows then
             raise (DimensionError ("Matrix declaration dimension mismatch for variable " ^ s))
           else
             Hashtbl.replace env s v; env
       | _ -> raise (TypeMismatch ("Matrix declaration requires a matrix literal for variable " ^ s)))

(* Program evaluation *)
let eval_program (prog : program) : unit =
  let _ = List.fold_left (fun env stmt -> eval_statement stmt env) (Hashtbl.create 100) prog in
  ()
