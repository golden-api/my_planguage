exception DimensionError of string
exception TypeMismatch of string
exception UndefinedVariable of string
exception TypeError of string

type env = (string, Ast.typ) Hashtbl.t

let empty_env () = Hashtbl.create 20

let rec type_of_expr (env : env) (e : Ast.expr) : Ast.typ =
  match e with
  | Ast.ConstBool _ -> Ast.B
  | Ast.ConstInt _ -> Ast.Int
  | Ast.ConstFloat _ -> Ast.Float
  | Ast.ConstV es ->
      (match es with
      | [] -> raise (TypeMismatch "Empty vector literal")
      | hd :: tl ->
          let t_first = type_of_expr env hd in
          List.iter (fun ex ->
            let t = type_of_expr env ex in
            if t <> t_first then
              raise (TypeMismatch "Vector elements must be of the same type")
          ) tl;
          (match t_first with
          | Ast.Int | Ast.Float -> Ast.V (List.length es)
          | _ -> raise (TypeMismatch "Vector elements must be numeric"))
      )
  | Ast.ConstM rows ->
      (match rows with
      | [] -> raise (TypeMismatch "Empty matrix literal")
      | first_row :: _ ->
          let row_len = List.length first_row in
          if row_len = 0 then
            raise (TypeMismatch "Matrix rows cannot be empty")
          else (
            List.iter (fun row ->
              if List.length row <> row_len then
                raise (TypeMismatch "Inconsistent matrix row lengths")
              else
                List.iter (fun ex ->
                  match type_of_expr env ex with
                  | Ast.Int | Ast.Float -> ()
                  | _ -> raise (TypeMismatch "Matrix elements must be numeric")
                ) row
            ) rows;
            Ast.M (List.length rows, row_len)
          )
      )
  | Ast.Var s ->
      (try Hashtbl.find env s with Not_found -> raise (UndefinedVariable s))
  | Ast.BinOp (e1, op, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match op with
      | Ast.Add ->
          (match t1, t2 with
          | Ast.Int, Ast.Int -> Ast.Int
          | Ast.Float, Ast.Float -> Ast.Float
          | Ast.V _, Ast.V _  -> Ast.V 0
          | Ast.M _, Ast.M _ -> Ast.M (0,0)
          | Ast.B, Ast.B -> Ast.B
          | _ -> raise (TypeMismatch "Addition requires operands of compatible types"))
      | Ast.Sub ->
          (match t1, t2 with
          | Ast.Int, Ast.Int -> Ast.Int
          | Ast.Float, Ast.Float -> Ast.Float
          | Ast.V _, Ast.V _  -> Ast.V 0
          | Ast.M _, Ast.M _  -> Ast.M (0,0)
          | _ -> raise (TypeMismatch "Subtraction requires operands of compatible types"))
      | Ast.Mul ->
          (match t1, t2 with
          | Ast.Int, Ast.Int -> Ast.Int
          | Ast.Float, Ast.Float -> Ast.Float
          | Ast.B, Ast.B -> Ast.B
          | Ast.V n, (Ast.Int | Ast.Float) -> Ast.V n
          | (Ast.Int | Ast.Float), Ast.V n -> Ast.V n
          | Ast.M (m, n), (Ast.Int | Ast.Float) -> Ast.M (m, n)
          | (Ast.Int | Ast.Float), Ast.M (m, n) -> Ast.M (m, n)
          | Ast.M _, Ast.M _ -> Ast.M (0,0)
          | _ -> raise (TypeMismatch "Multiplication requires operands of compatible types"))
      | Ast.Div ->
          (match t1, t2 with
          | Ast.Int, Ast.Int -> Ast.Int
          | Ast.Float, Ast.Float -> Ast.Float
          | Ast.V n, (Ast.Int | Ast.Float) -> Ast.V n
          | Ast.M (m, n), (Ast.Int | Ast.Float) -> Ast.M (m, n)
          | _ -> raise (TypeMismatch "Division requires numeric operands or scalar division"))
      | Ast.Mod ->
          (match t1, t2 with
          | Ast.Int, Ast.Int -> Ast.Int
          | _ -> raise (TypeMismatch "Modulo requires integer operands"))
      | Ast.And ->
          (match t1, t2 with
          | Ast.B, Ast.B -> Ast.B
          | _ -> raise (TypeMismatch "Logical AND requires boolean operands"))
      | Ast.Or ->
          (match t1, t2 with
          | Ast.B, Ast.B -> Ast.B
          | _ -> raise (TypeMismatch "Logical OR requires boolean operands"))
      | Ast.Lt | Ast.Gt | Ast.Lte | Ast.Gte ->
          (match t1, t2 with
          | Ast.Int, Ast.Int -> Ast.B
          | Ast.Float, Ast.Float -> Ast.B
          | _ -> raise (TypeMismatch "Comparison requires numeric operands"))
      | Ast.Eq ->
          if t1 = t2 then Ast.B
          else raise (TypeMismatch "Equality requires matching types")
      | Ast.Dot ->
          (match t1, t2 with
          | Ast.V _, Ast.V _ -> Ast.Float
          | _ -> raise (TypeMismatch "Dot product requires two vectors"))
      | Ast.Angle ->
          (match t1, t2 with
          | Ast.V _, Ast.V _ -> Ast.Float
          | _ -> raise (TypeMismatch "Angle operator requires two vectors"))
      )
  | Ast.UnOp (op, e1) ->
      let t = type_of_expr env e1 in
      (match op with
      | Ast.Neg ->
          (match t with
          | Ast.Int -> Ast.Int
          | Ast.Float -> Ast.Float
          | Ast.V _ -> t
          | _ -> raise (TypeMismatch "Negation requires a numeric or vector type"))
      | Ast.Not ->
          if t = Ast.B then Ast.B
          else raise (TypeMismatch "Not operator requires a boolean type")
      | Ast.Magnitude ->
          (match t with
          | Ast.V _ | Ast.Int | Ast.Float-> Ast.Float
          | _ -> raise (TypeMismatch "Magnitude requires a vector"))
      | Ast.Dimension ->
          (match t with
          | Ast.V _ | Ast.M _ -> Ast.Int
          | _ -> raise (TypeMismatch "Dimension operator requires a vector or matrix"))
      | Ast.Transpose ->
          (match t with
          | Ast.M (m, n) -> Ast.M (n, m)
          | Ast.V (n) -> Ast.M(n,1)
          | _ -> raise (TypeMismatch "Transpose requires a matrix"))
      | Ast.Determinant ->
          (match t with
          | Ast.M (m, n) when m = n -> Ast.Float
          | _ -> raise (TypeMismatch "Determinant requires a square matrix"))
      | Ast.Inverse ->
          (match t with
          | Ast.M (m, n) when m = n -> Ast.M (m, n)
          | _ -> raise (TypeMismatch "Inverse requires a square matrix"))
      )
      | Ast.Assign (s, e1) ->
        let t1 = type_of_expr env e1 in
        (try  let t_var = Hashtbl.find env s in
           match (t1, t_var) with
           | (Ast.V (n1), Ast.V (n2)) ->
               if (n1!=0 && n2!=0 && n1 <> n2) then
                 raise (TypeMismatch ("Assignment type mismatch for variable " ^ s))    else t1
           | (Ast.M (r1, c1), Ast.M (r2, c2)) ->
               if (r1!=0 && r2!=0 && r1 <> r2 || c1 <> c2)  then
                 raise (TypeMismatch ("Assignment type mismatch for variable " ^ s))    else t1
           | _ when t1 = t_var -> t1 
           | _ -> raise (TypeMismatch ("Assignment type mismatch for variable " ^ s))  with Not_found ->Hashtbl.add env s t1; 
         t1)    
  | Ast.Input (spec, _) ->
      (match spec with
      | TInt -> Ast.Int
      | TFloat -> Ast.Float
      | TBool -> Ast.B
      | TV -> Ast.V 0
      | TM -> Ast.M (0, 0)
      )
  | Ast.Index (target, indices) ->
      let target_type = type_of_expr env target in
      List.iter (fun t ->
        if t <> Ast.Int then raise (TypeError "Index must be integer")
      ) (List.map (type_of_expr env) indices);
      (match target_type with
      | Ast.V _ ->
          if List.length indices <> 1 then
            raise (TypeError "Vector requires 1 index")
          else
            Ast.Float
      | Ast.M (rows, cols) ->
          if List.length indices > 2 then
            raise (TypeError "Matrix supports at most 2 indices")
          else if List.length indices = 1 then
            Ast.V cols
          else
            Ast.Float
      | _ ->
          raise (TypeError "Indexing only allowed on vectors/matrices")
      )
  | Ast.AssignIdx (Ast.Index (Ast.Var s, indices), e1) ->
      let t_container = Hashtbl.find env s in let t_value = type_of_expr env e1 in
      List.iter (fun idx ->   match type_of_expr env idx with
          | Ast.Int -> ()
          | _ -> raise (TypeMismatch "Index must be integer")
      ) indices;
     ( match (t_container, List.length indices) with
        | (Ast.V _, 1) when t_value <> Ast.Int && t_value <> Ast.Float ->   raise (TypeMismatch "Vector element type must be int/float")
        | (Ast.V _, 1) -> t_value
        | (Ast.M _, 2) when t_value <> Ast.Int && t_value <> Ast.Float ->   raise (TypeMismatch "Matrix element type must be int/float")
        | (Ast.M _, 2) -> t_value
        | (Ast.M _, 1) ->   (match t_value with
             | Ast.V _ -> t_value   | _ -> raise (TypeMismatch "Matrix element type must be vector"))
        | _ -> raise (TypeMismatch "Invalid container/index combination"))
   
  | _ -> raise (TypeError "Invalid expression")

let rec check_statement (env : env) (s : Ast.statement) : unit =
  match s with
  | Ast.Expr e ->
      let _ = type_of_expr env e in ()
  | Ast.Block statements ->
      let local_env = Hashtbl.copy env in
      List.iter (check_statement local_env) statements
  | Ast.If (e, s1, None) ->
      if type_of_expr env e = Ast.B then check_statement env s1
      else raise (TypeMismatch "If condition must be boolean")
  | Ast.If (e, s1, Some s2) ->
      if type_of_expr env e = Ast.B then (
        check_statement env s1;
        check_statement env s2
      ) else raise (TypeMismatch "If condition must be boolean")
  | Ast.While (e, s1) ->
      if type_of_expr env e = Ast.B then check_statement env s1
      else raise (TypeMismatch "While condition must be boolean")
  | Ast.For (var, e1, e2, s_body) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
      | Ast.Int, Ast.Int
      | Ast.Float, Ast.Float ->
          let local_env = Hashtbl.copy env in
          Hashtbl.add local_env var t1;
          check_statement local_env s_body
      | _ ->
          raise (TypeMismatch "For loop bounds must be numeric")
      )
  | Ast.Print e ->
      let _ = type_of_expr env e in ()
  | Ast.Break -> ()
  | Ast.Continue -> ()
  | Ast.Vector (dim, s, e) ->
      let t = type_of_expr env e in
      (match t with
      | Ast.V n when n = dim ->
          Hashtbl.add env s t
      | _ ->
          raise (TypeMismatch ("Vector declaration type mismatch for variable " ^ s))
      )
  | Ast.Matrix (r, c, s, e) ->
      let t = type_of_expr env e in
      (match t with
      | Ast.M (r2, c2) when r = r2 && c = c2 ->
          Hashtbl.add env s t
      | _ ->
          raise (TypeMismatch ("Matrix declaration type mismatch for variable " ^ s))
      )

let typecheck_program (prog : Ast.program) : unit =
  let env = empty_env () in
  List.iter (check_statement env) prog
