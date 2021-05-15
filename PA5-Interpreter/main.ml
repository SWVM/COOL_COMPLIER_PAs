open Printf
type line_num   = string
and identifier = line_num * string
and param      = expr list
and case_      = identifier * identifier * expr
and binding    = identifier * identifier * (expr option)
and expr =  line_num * cool_type * expr_inner
and expr_inner = Assign       of identifier * expr
               | DDispatch    of expr * identifier * param
               | SDispatch    of expr * identifier * identifier * param
               | SelfDispatch of identifier * param
               | If           of expr * expr * expr
               | While        of expr * expr
               | Block        of expr list
               | New          of identifier
               | Isvoid       of expr
               | Plus         of expr * expr
               | Minus        of expr * expr
               | Times        of expr * expr
               | Divide       of expr * expr
               | Lt           of expr * expr
               | Le           of expr * expr
               | Eq           of expr * expr
               | Not          of expr
               | Negate       of expr
               | Integer      of string
               | String       of string
               | Id           of identifier
               | True_        of string
               | False_       of string
               | Case         of expr * case_ list
               | Let          of (binding list) * expr
               | Internal     of string

and cool_address = int
and cool_type = string
and cool_method = string
and var_name = string
and cool_value   =
  | Cool_Int    of Int32.t
  | Cool_Bool   of bool
  | Cool_String of string * int
  | Cool_Obj     of string * ( (string * cool_address) list)
  | Void

and sys_args = string list


(* Naming Environment  *)
type environment = (string * cool_address) list

(* Store  *)
type store= (cool_address * cool_value) list

(* Class Map *)
type class_map = (cool_type * ((var_name * cool_type * (expr option)) list) ) list


(* Implementation Map*)
type imp_map =
  ((cool_type * cool_method)
   *
   ( (cool_type list) * expr) ) list
(* Parent map -----c--------p-----*)
type parent_map = (string * string) list

(*  address generation *)
let new_location_counter = ref 10000
let newloc () =
  incr new_location_counter;
  !new_location_counter
let activation_rec_count = ref 0

(*  escape \t and \n*)
let escape_t_n s =
  begin
    let result = ref "" in
    let i = ref 0 in
    let maxi = (String.length s) in
    while !i<maxi do begin
      if not (!i = maxi-1)
      then
        begin
          match s.[!i] with
          | '\\' ->
            begin
              match s.[!i+1] with
              | 'n' -> result := !result ^ "\n";incr i;incr i
              | 't' -> result := !result ^ "\t";incr i;incr i
              | _ -> result := !result ^ (String.make 1 s.[!i]);incr i
            end
          | _ ->
            result := !result ^ (String.make 1 s.[!i]);incr i
        end
      else
        begin result := !result ^ (String.make 1 s.[!i]);incr i
        end
    end done;
    !result
  end

(*********************************************************************)
(* type definition and utilities func *)
(*********************************************************************)

(*********************************************************************)
(* Deserializing *)
(*********************************************************************)

let get_ast () =
  let fname = Sys.argv.(1) in
  let fin = open_in fname in

  let read  () =
    input_line fin
  in

  let rec range k =
    if k <= 0 then []
    else k :: (range (k-1))
  in

  let read_list worker =
    let k = int_of_string (read ()) in
    let lst = range k in
    List.map (fun _ -> worker ()) lst
  in

  let rec read_id () =
    let line_ = read () in
    let name_= read () in
    (name_, line_)

  and read_binding () =
    let init_ = read () in
    let name_ = read_id () in
    let type_ = read_id () in
    match init_ with
    | "let_binding_no_init" ->
      (name_, type_, None)
    | "let_binding_init" ->
      (name_,type_, Some(read_expr ()))

  and read_case () =
    let name_ = read_id () in
    let type_ = read_id () in
    let expr_ = read_expr () in
    (name_, type_, expr_)

  and read_expr () =
    let line_ = read () in
    let assoc_type = read () in
    let expr_type = read () in
    let expr_ = match expr_type with
      | "assign" ->
        let name_ = read_id () in
        let expr_ = read_expr () in
        Assign(name_, expr_)
      | "dynamic_dispatch" ->
        let expr_   = read_expr () in
        let method_ = read_id () in
        let args_   = read_list read_expr in
        DDispatch(expr_, method_, args_)
      | "static_dispatch" ->
        let expr_   = read_expr () in
        let type_   = read_id () in
        let method_ = read_id () in
        let args_   = read_list read_expr in
        SDispatch(expr_, type_, method_, args_)
      | "self_dispatch" ->
        let method_ = read_id () in
        let args_   = read_list read_expr in
        SelfDispatch(method_, args_)
      | "if" ->
        let pred_ = read_expr () in
        let then_ = read_expr () in
        let else_ = read_expr () in
        If(pred_, then_, else_)
      | "while" ->
        let pred_ = read_expr () in
        let body_ = read_expr () in
        While(pred_, body_)
      | "block" ->
        let body_ = read_list read_expr in
        Block(body_)
      | "new" ->
        let name_ = read_id () in
        New(name_)
      | "isvoid" ->
        let expr_ = read_expr () in
        Isvoid(expr_)
      | "plus" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Plus(lhs_, rhs_)
      | "minus" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Minus(lhs_, rhs_)
      | "times" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Times(lhs_, rhs_)
      | "divide" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Divide(lhs_, rhs_)
      | "lt" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Lt(lhs_, rhs_)
      | "le" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Le(lhs_, rhs_)
      | "eq" ->
        let lhs_ = read_expr () in
        let rhs_ = read_expr () in
        Eq(lhs_, rhs_)
      | "not" ->
        let expr_ = read_expr () in
        Not(expr_)
      | "negate" ->
        let expr_ = read_expr () in
        Negate(expr_)
      | "integer" ->
        let val_ = read () in
        Integer(val_)
      | "string" ->
        String(read ())
      | "identifier" ->
        Id(read_id ())
      | "true" ->
        True_("true")
      | "false" ->
        False_("false")
      | "let" ->
        let bindings_ = read_list read_binding in
        let expr_     = read_expr () in
        Let(bindings_, expr_)
      | "case" ->
        let expr_  = read_expr () in
        let cases_ = read_list read_case in
        Case(expr_, cases_)
      | "internal" ->
        let detail = read () in
        Internal(detail)
    in
    (line_, assoc_type, expr_)

  and read_attri () =
    let init_ = read () in
    let name_ = read () in
    let type_ = read () in
    let expr_ =
      match init_ with
      | "no_initializer" ->
        (* TODO: how to handle no init *)
        None
      | "initializer" ->
        Some(read_expr ())
    in
    (name_, type_, expr_)

  and read_method cname_ () =
    let mname_ = read () in
    let formals = read_list read in
    (*TODO:skip inheritance detail about method*)
    read ();
    let expr_ = read_expr () in
    ((cname_, mname_), (formals, expr_))

  and read_class_map () =
    let cname_ = read () in
    let attri_ = read_list read_attri in
    (cname_, attri_)

  and read_imp_map () =
    let cname_ = read () in
    let methods = read_list (read_method cname_) in
    methods

  and read_parent_map () =
    let child_name = read () in
    let parent_name= read () in
    (child_name, parent_name)
  in

  (* read class map *)
  read ();
  let class_map_ = read_list read_class_map in
  (* read Implementation map *)
  read ();
  let imp_map_   = List.flatten (read_list read_imp_map) in
  read ();
  let parent_map = read_list read_parent_map in
  (class_map_, imp_map_, parent_map)

(*********************************************************************)
(* maps stored here*)
let c_map,i_map,p_map = get_ast ()
(*********************************************************************)

(*********************************************************************)
(*  get the distance from child class to one of its ancestor   *)
(*  For Case expression   *)
(*********************************************************************)
let rec get_dist child parent =
  (* if current node becomes Ojb, probably this is because the inital c is the ancestor of p*)
  if child=parent
  then 0
  else
    begin
      if child = "Object"
      then -1
      else
        let rec_val = get_dist (List.assoc child p_map) parent in
        if rec_val = -1 then -1 else (rec_val + 1)
    end


(*********************************************************************)
(*********************************************************************)
(*********************************************************************)
(* Debug and printing*)
(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

let do_debug = ref false
let debug fmt =
  let handle result_string =
    if !do_debug then printf "%s" result_string
  in
  kprintf handle fmt


let rec exp_to_str (line_, cool_type, e) =
  match e with
  |Assign((name_, line_), expr_) ->
    sprintf "Assign(%s,%s)" name_ (exp_to_str expr_)
  | DDispatch(ro, (fname, fline), args) ->
    let arg_str =
      List.fold_left
        (fun acc elt -> acc ^ ", " ^ (exp_to_str elt))
        ""
        args
    in
    sprintf "D-Dispatch(%s ,%s ,%s )" (exp_to_str ro) fname arg_str
  | SDispatch(ro, (pname, pline), (fname, fline), args) ->
    let arg_str =
      List.fold_left
        (fun acc elt -> acc ^ ", " ^ (exp_to_str elt))
        ""
        args
    in
    sprintf "D-Dispatch(%s ,%s ,%s, %s )" (exp_to_str ro) pname fname arg_str
  | SelfDispatch((fname, fline),args) ->
    let arg_str =
      List.fold_left
        (fun acc elt -> acc ^ ", " ^ (exp_to_str elt))
        ""
        args
    in
    sprintf "D-Dispatch(%s ,%s )" fname arg_str
  | If(condi, b1, b2) ->
    sprintf "If(%s, %s, %s )" (exp_to_str condi) (exp_to_str b1) (exp_to_str b2)
  | While(condi, body) ->
    sprintf "While(%s, %s )" (exp_to_str condi) (exp_to_str body)
  | Block(exprs) ->
    let expr_str = List.fold_left
        (fun acc elt -> acc ^ ", " ^ (exp_to_str elt))
        ""
        exprs
    in
    sprintf "Block(%s )" expr_str
  | New((name_,line_)) ->
    sprintf "New(%s )" name_
  | Isvoid(expr) ->
    sprintf "Isvoid(%s )" (exp_to_str expr)
  | Plus(e1, e2) ->
    sprintf "Plus(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Minus(e1, e2) ->
    sprintf "Minus(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Times(e1, e2) ->
    sprintf "Times(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Divide(e1, e2) ->
    sprintf "Divide(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Lt(e1, e2) ->
    sprintf "Lt(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Le(e1, e2) ->
    sprintf "Le(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Eq(e1, e2) ->
    sprintf "Eq(%s, %s )" (exp_to_str e1) (exp_to_str e2)
  | Not(expr) ->
    sprintf "Not(%s )" (exp_to_str expr)
  | Negate(expr)->
    sprintf "Negate(%s )" (exp_to_str expr)
  | Integer(s) ->
    sprintf "Integer(%s )" s
  | String(s) ->
    sprintf "String(%s )" s
  | Id(name_,line_) ->
    sprintf "Identifier(%s, %s )" name_ line_
  | True_(s) ->
    sprintf "Bool(%s )" s
  | False_(s) ->
    sprintf "Bool(%s )" s
  | Case(ro, cases) ->
    let case_str = List.fold_left
        (fun acc elt -> acc ^ ", " ^ (case_to_str elt))
        ""
        cases
    in
    sprintf "Case(%s, %s)" (exp_to_str ro) case_str
  | Let(bindings, expr) ->
    let binding_str = List.fold_left
        (fun acc elt -> acc ^ ", " ^ (binding_to_str elt))
        ""
        bindings
    in
    sprintf "Let(%s, %s )" binding_str (exp_to_str expr)
  | Internal(s) ->
    sprintf "Internal(%s )" s

and case_to_str (((name_, line_),(tname, tline),expr)) =
  sprintf "Case(%s, %s, %s )" name_ tname (exp_to_str expr)

and binding_to_str ((name_, line_), (tname, tline), expr) =
  match expr with
  | None ->
    sprintf "BindingNoInit(%s, %s )" name_ tname
  | Some(expr) ->
    sprintf "BindingInit(%s, %s,%s )" name_ tname (exp_to_str expr)

and value_to_string v =
  match v with
  | Cool_Int(i) -> sprintf "Int(%ld)" i
  | Cool_Bool(b) -> sprintf "Bool(%b)" b
  | Cool_String(s,l) -> sprintf "String(%s,%d)" (String.escaped s) l
  | Void -> sprintf "Void"
  | Cool_Obj(cname, attrs) ->
    let attr_str = List.fold_left
        (fun acc (aname, aaddr) ->
           sprintf "%s, %s=%d" acc aname aaddr)
        ""
        attrs
    in
    sprintf "%s([%s])" cname attr_str

and env_to_str env =
  let env_str = List.fold_left
      (fun acc (aname, aaddr) ->
         sprintf "%s, %s=%d" acc aname aaddr)
      ""
      (List.sort compare env)
  in
  sprintf "[%s]" env_str

and store_to_str env =
  let store_str = List.fold_left
      (fun acc (addr, cvalue) ->
         sprintf "%s, %d=%s" acc addr (value_to_string cvalue))
      ""
      (List.sort compare env)
  in
  sprintf "[%s]" store_str

let indent_count = ref 0
let debug_indent () =
  debug "%s" (String.make !indent_count ' ')


(*********************************************************************)
(* Evaluation*)
(*********************************************************************)
let rec eval
    (so : cool_value)
    (s : store)
    (e : environment)
    (exp_ : expr)
  :
    (cool_value * store) =
  (*****************      Function starting    *************************)
  (*********************************************************************)
  let (line_, type_, expr_) = exp_ in

  (***********      Overflow handling                        ***********)
  (* count are incremented here but decremented at the end of each handling branch *)
  begin
    match expr_ with
    | SDispatch(_,_,_,_) -> incr activation_rec_count
    | DDispatch(_,_,_) -> incr activation_rec_count
    | New(_) -> incr activation_rec_count
    | _ -> ()
  end;
  if !activation_rec_count > 1000 then begin
    printf "ERROR: %s: Exception: stack overflow\n" line_;
    exit 1
  end;

  (*********************************************************************)
  (************    SEMENTIC RULES        *******************************)

  let new_val, new_store =
    match expr_ with
    | Integer(i) ->
      (Cool_Int(Int32.of_string i), s)
    | String(i) ->
      let escaped = escape_t_n i in
      (Cool_String(escaped, String.length escaped), s)
    | True_(i) ->
      (Cool_Bool(true), s)
    | False_(i) ->
      (Cool_Bool(false), s)
    | Id(iname, iline) ->
      if (iname = "self")
      then
        (so, s)
      else
        let addr  = List.assoc iname e in
        let value = List.assoc addr s in
        (value, s)
    | Assign((vname,vline), rhs) ->
      let v1, s2 = eval so s e rhs in
      let v_addr = List.assoc vname e in
      let s3 = (v_addr,v1) :: (List.remove_assoc v_addr s2 ) in
      (v1, s3)
    | Plus(e1, e2) ->
      let v1, s2 = eval so s e e1 in
      let v2, s3 = eval so s2 e e2 in
      let result_value = match v1, v2 with
        | Cool_Int(i1), Cool_Int(i2) ->
          Cool_Int(Int32.add i1 i2)
        | _,_ ->
          failwith "bad plus"
      in
      (result_value, s3)
    | Minus(e1, e2) ->
      let v1, s2 = eval so s e e1 in
      let v2, s3 = eval so s2 e e2 in
      let result_value = match v1, v2 with
        | Cool_Int(i1), Cool_Int(i2) ->
          Cool_Int(Int32.sub i1 i2)
        | _,_ ->
          failwith "bad minus"
      in
      (result_value, s3)
    | Times(e1, e2) ->
      let v1, s2 = eval so s e e1 in
      let v2, s3 = eval so s2 e e2 in
      let result_value = match v1, v2 with
        | Cool_Int(i1), Cool_Int(i2) ->
          Cool_Int(Int32.mul i1 i2)
        | _,_ ->
          failwith "bad times"
      in
      (result_value, s3)
    | Divide(e1, e2) ->
      let v1, s2 = eval so s e e1 in
      let v2, s3 = eval so s2 e e2 in
      let result_value = match v1, v2 with
        | Cool_Int(i1), Cool_Int(i2) ->
          let zero = Int32.of_int 0 in
          if i2 = zero then
            begin
              printf "ERROR: %s: Exception: division by zero\n" line_;
              exit 1
            end;
          Cool_Int(Int32.div i1 i2)
        | _,_ ->
          failwith "bad divide"
      in
      (result_value, s3)
    | New((cname, cline)) ->
      let cname =
        begin
          match cname with
          | "SELF_TYPE" ->
            begin
              match so with
              | Cool_Int(_) -> "Int"
              | Cool_String(_,_) -> "String"
              | Cool_Obj(n,a) -> n
              | Void -> "Void"
            end
          | _ -> cname
        end
      in

      let attrs_and_inits = List.assoc cname c_map in
      let new_attr_locs =
        List.map
          (fun (aname, atype, ainit) ->
             newloc())
          attrs_and_inits
      in
      let attr_names = List.map
          (fun (aname, atype, ainit) ->
             aname)
          attrs_and_inits
      in
      let attr_types = List.map
          (fun (aname, atype, ainit) ->
             atype)
          attrs_and_inits
      in
      let attr_inits = List.map
          (fun (aname, atype, ainit) ->
             ainit)
          attrs_and_inits
      in
      (****  NEW BOJECT    *)
      let attrs_and_locs = List.combine attr_names new_attr_locs in
      let v1 = Cool_Obj(cname, attrs_and_locs) in

      let v1 =
        match cname with
        | "Int" -> Cool_Int(Int32.zero)
        | "String" -> Cool_String("",0)
        | "Bool" -> Cool_Bool(false)
        | _ -> Cool_Obj(cname, attrs_and_locs)
      in

      (****  DEFAULT VALUES*)
      let locs_and_types = List.combine new_attr_locs attr_types in
      let store_updates = List.map
          (fun (loc, atype) ->
             let init_val =
               match atype with
               | "Int"    -> Cool_Int(Int32.of_int 0)
               | "String" -> Cool_String("",0)
               | "Bool"   -> Cool_Bool(false)
               | _        -> Void
             in
             (loc, init_val)
          )
          locs_and_types
      in
      let s2 = s @ store_updates in
      (****  ASSIGN INIT VALUES*)
      let final_store =
        List.fold_left
          (fun accumulated_store (aname, atype, ainit) ->
             match ainit with
             | None ->
               accumulated_store
             | Some(ainit) ->
               let _, updated_store = eval v1 accumulated_store attrs_and_locs ("","",Assign((aname,""), ainit)) in
               updated_store
          )
          s2 attrs_and_inits
      in
      decr activation_rec_count;
      v1, final_store
    | DDispatch(expr, (mname, mline), args) ->
      (****  GET ARGS*)
      let current_store = ref s in
      let arg_values =
        List.map
          (fun expr ->
             let arg_val,new_store = eval so !current_store e expr in
             current_store := new_store;
             arg_val
          )
          args
      in
      (****  GET OJB VALUE*)
      let v0, s_n2 = eval so !current_store e expr in
      let cname =
        match v0 with
        | Cool_Obj(cname,_) -> cname
        | Cool_Int(_) ->  "Int"
        | Cool_Bool(_) -> "Bool"
        | Cool_String(_,_) -> "String"
        | Void ->
          printf "ERROR: %s: Exception: dispatch on void\n" line_;
          exit 1
      in
      (****  GET Implementation*)
      let formals, body = List.assoc (cname, mname) i_map in
      (****  bind args to formals*)
      let new_arg_locs =
        List.map
          (fun arg_exp -> newloc ())
          args
      in
      let store_updates = List.combine new_arg_locs arg_values in
      let s_n3 = store_updates @ s_n2 in
      let args_and_locs = List.combine formals new_arg_locs in
      let _, attrs_and_locs =
        match v0 with
        | Cool_Obj(type_, attrs) -> type_, attrs
        | _ -> "", []
      in
      let vn,sn = eval v0 s_n3 (args_and_locs @ attrs_and_locs @ e) body in
      decr activation_rec_count;
      vn,sn
    | SDispatch(expr, (pname, pline), (mname, mline), args) ->
      (****  GET ARGS*)
      let current_store = ref s in
      let arg_values =
        List.map
          (fun expr ->
             let arg_val,new_store = eval so !current_store e expr in
             current_store := new_store;
             arg_val
          )
          args
      in
      (****  GET OJB VALUE*)
      let v0, s_n2 = eval so !current_store e expr in
      begin
        match v0 with
        | Void ->
          printf "ERROR: %s: Exception: static dispatch on void\n" line_;
          exit 1
        | _ ->
          ()
      end;
      (****  GET Implementation*)
      let formals, body = List.assoc (pname, mname) i_map in
      (****  bind args to formals*)
      let new_arg_locs =
        List.map
          (fun arg_exp -> newloc ())
          args
      in
      let store_updates = List.combine new_arg_locs arg_values in
      let s_n3 = store_updates @ s_n2 in
      let args_and_locs = List.combine formals new_arg_locs in
      let _, attrs_and_locs =
        match v0 with
        | Cool_Obj(type_, attrs) -> type_, attrs
        | _ -> "", []
      in

      let vn,sn = eval v0 s_n3 (args_and_locs @ attrs_and_locs @ e) body in
      decr activation_rec_count;
      vn,sn
    | SelfDispatch(method_, args) ->
      eval so s e (line_,type_,DDispatch(("","",Id("self","")),method_,args))
    | If(condi, exp1, exp2) ->
      begin
        let predicate, s_2 = eval so s e condi in
        match predicate with
        | Cool_Bool(true) ->
          eval so s_2 e exp1
        | Cool_Bool(false)->
          eval so s_2 e exp2
        | _ ->
          failwith "unexpected predicate in if"
      end
    | Block(exprs) ->
      List.fold_left
        (fun (exp_val, acc_store) expr->
           eval so acc_store e expr
        )
        (Void, s)
        exprs
    | While(condi, expr) ->
      begin
        let flag = ref true in
        let sn   = ref s in
        while !flag
        do
          begin
            let predicate,new_s = eval so !sn e condi in
            sn := new_s;
            match predicate with
            | Cool_Bool(true) ->
              let _,new_s = eval so !sn e expr in
              sn := new_s
            | Cool_Bool(false) ->
              flag := false
            | _ ->
              failwith "impossible predicate val in while";
          end
        done;
        Void, !sn
      end
    (************************************************************)
    (***********   Abandoned recursive version    ***************)
    (*      begin
            let predicate, s2 = eval so s e condi in
            match predicate with
            | Cool_Bool(true) ->
              let _, s3 = eval so s2 e expr in
              let _, s4 = eval so s3 e exp_ in
              Void, s4
            | Cool_Bool(false) ->
              Void, s2
            | _ ->
              failwith "unexpected predicate in while"
            end
    *)
    (************************************************************)
    | Isvoid(expr) ->
      begin
        let exp_val, s2 = eval so s e expr in
        match exp_val with
        | Void -> Cool_Bool(true), s2
        | _ -> Cool_Bool(false), s2
      end
    | Lt(exp1, exp2) ->
      begin
        let val_1,s2 = eval so s e exp1 in
        let val_2,s3 = eval so s2 e exp2 in
        match val_1,val_2 with
        | Cool_Int(a),Cool_Int(b) ->
          Cool_Bool(a<b), s3
        | Cool_Bool(a),Cool_Bool(b) ->
          Cool_Bool(a<b), s3
        | Cool_String(s1,l1),Cool_String(s2,l2) ->
          Cool_Bool(s1<s2), s3
        | _,_ ->
          failwith "bad compare"
      end
    | Le(exp1, exp2) ->
      begin
        let val_1,s2 = eval so s e exp1 in
        let val_2,s3 = eval so s2 e exp2 in
        match val_1,val_2 with
        | Cool_Int(a),Cool_Int(b) ->
          Cool_Bool(a<=b), s3
        | Cool_Bool(a),Cool_Bool(b) ->
          Cool_Bool(a<=b), s3
        | Cool_String(s1,l1),Cool_String(s2,l2) ->
          Cool_Bool(s1<s2), s3
        | _,_ ->
          failwith "bad compare"
      end
    | Eq(exp1, exp2) ->
      begin
        let v1, s2 = eval so s e exp1 in
        let v2, s3 = eval so s2 e exp2 in
        match v1, v2 with
        | Cool_Int(a),Cool_Int(b) ->
          Cool_Bool(a=b), s3
        | Cool_String(str1,len1),Cool_String(str2,len2) ->
          Cool_Bool(str1=str2 && len1= len2), s3
        | Cool_Bool(a),Cool_Bool(b) ->
          Cool_Bool(a=b), s3
        | Void, Void ->
          Cool_Bool(true), s3
        | Cool_Obj(_,_),Cool_Obj(_,_)->
          Cool_Bool(v1 == v2), s3
        | _,_ ->
          Cool_Bool(false), s3
      end
    | Not(expr) ->
      begin
        let v,s2 = eval so s e expr in
        match v with
        | Cool_Bool(b) ->
          Cool_Bool(not b), s2
        | _ ->
          failwith "non bool exp in not"
      end
    | Negate(expr) ->
      begin
        let v, s2 = eval so s e expr in
        match v with
        | Cool_Int(i) ->
          Cool_Int(Int32.neg i), s2
        | _ ->
          failwith "non int expr in negate"
      end
    | Let(b_lst, body) ->
      begin
        match b_lst with
        | [] ->
          eval so s e body
        | ( (vname,_), (vtype,_), init_ ) :: tl ->
          begin
            let v1, s2 =
              begin
                match init_ with
                | Some(expr) ->
                  eval so s e expr
                | None ->
                  begin
                    match vtype with
                    | "Int"    -> Cool_Int(Int32.of_int 0), s
                    | "String" -> Cool_String("",0), s
                    | "Bool"   -> Cool_Bool(false), s
                    | _        -> Void, s
                  end
              end
            in
            let v_addr = newloc () in
            let s3 = (v_addr, v1) :: s2 in
            let e1 = (vname, v_addr) :: e in
            eval so s3 e1 (  "",  "",  Let(tl,body) )
          end
      end
    | Case(expr, c_lst) ->
      begin
        let v0,s2 = eval so s e expr in
        let e_type = match v0 with
          | Void ->
            printf "ERROR: %s: Exception: case on void\n" line_;
            exit 1
          | Cool_Int(_) ->
            "Int"
          | Cool_String(_,_) ->
            "String"
          | Cool_Bool(_) ->
            "Bool"
          | Cool_Obj(t,_) ->
            t
        in
        let cmp_cases (_,(ctype1,_),_) (_,(ctype2,_),_) =
          let dist_1 = get_dist e_type ctype1 in
          let dist_2 = get_dist e_type ctype2 in
          dist_1 - dist_2
        in
        let c_lst = List.filter
            (fun ((_,_),(t,_),_) ->
               if (get_dist e_type t) = -1
               then false
               else true
            )
            c_lst
        in
        if (List.length c_lst)=0
        then
          begin
            printf "ERROR: %s: Exception: case without matching branch: %s(...)\n" line_ e_type;
            exit 1
          end;
        let sorted_cases = List.sort cmp_cases c_lst in
        let (name_,_), (type_,_), expr_ = List.hd sorted_cases in
        let addr = newloc () in
        let s3 = (addr,v0) :: s2 in
        let e0 = (name_,addr) :: e in
        eval so s3 e0 expr_
      end

    (********************************************************)
    (***********          SYSTEM CALLS       ****************)
    (********************************************************)
    | Internal(stm_call) ->
      begin
        match stm_call with
        | "IO.in_int" ->
          (* BAD FORMATED STRINGS RETURNS 0  *)
          (* trim and chop out valid part of the input string 0  *)
          begin
            let str_in =
              try String.trim (input_line stdin) with
              | _ -> ""
            in
            let chopped = ref "" in
            let flag    = ref true in
            let fisrt   = ref true in
            String.iter
              (fun s ->
                 if !flag then begin
                   match s with
                   | '1' -> chopped := !chopped ^ "1"
                   | '2' -> chopped := !chopped ^ "2"
                   | '3' -> chopped := !chopped ^ "3"
                   | '4' -> chopped := !chopped ^ "4"
                   | '5' -> chopped := !chopped ^ "5"
                   | '6' -> chopped := !chopped ^ "6"
                   | '7' -> chopped := !chopped ^ "7"
                   | '8' -> chopped := !chopped ^ "8"
                   | '9' -> chopped := !chopped ^ "9"
                   | '0' -> chopped := !chopped ^ "0"
                   | '-' -> if !fisrt then chopped := !chopped ^ "-"
                   | _   -> flag := false
                 end;
                 fisrt := false
              )
              str_in;
            let ret_val =
              try Int32.of_string !chopped with
              | _ -> Int32.of_int 0
            in
            Cool_Int(ret_val), s
          end
        | "IO.in_string" ->
          (*   read in line and return "" if contains illegal characters  *)
          let str_in = escape_t_n (input_line stdin) in
          if (String.contains str_in '\x00') || (String.contains str_in '\000')
          then Cool_String("", 0), s
          else Cool_String(str_in, String.length str_in), s
        | "IO.out_int" ->
          (*    find arg from e and s, they print out    *)
          begin
            let arg_addr = List.assoc "x" e in
            let arg0 =  List.assoc arg_addr s in
            begin
              match arg0 with
              | Cool_Int(i) ->
                printf "%d" (Int32.to_int i)
              | _ ->
                failwith "bad arg out_int"
            end;
            so,s
          end
        | "IO.out_string" ->
          (*    find arg from e and s, they print out    *)
          begin
            let arg_addr = List.assoc "x" e in
            let arg0 =  List.assoc arg_addr s in
            begin
              match arg0 with
              | Cool_String(s,l) ->
                printf "%s" s
              | _ ->
                failwith "bad arg out_int"
            end;
            so,s
          end
        | "Object.abort" ->
          printf "abort\n";
          exit 1;
        | "Object.copy" ->
          (*    SHALLOW COPY   *)
          begin
            let v0 =
              match so with
              | Cool_Int(i) ->
                Cool_Int(i)
              | Cool_String(s,l) ->
                Cool_String(s,l)
              | Cool_Bool(b) ->
                Cool_Bool(b)
              | Cool_Obj(t,attri_lst) ->
                Cool_Obj(t,attri_lst)
              | _ -> Void
            in
            v0, ( newloc (),v0) :: s
          end
        | "Object.type_name" ->
          begin
            match so with
            | Cool_Int(_) -> Cool_String("Int", 3), s
            | Cool_String(_,_) -> Cool_String("String", 6), s
            | Cool_Bool(_) -> Cool_String("Bool",4), s
            | Cool_Obj(t,_) -> Cool_String(t, String.length t), s
            | _ -> failwith "bad Obj.type_name"
          end
        | "String.concat" ->
          begin
            let addr = List.assoc "s" e in
            let v0   = List.assoc addr s in
            match so,v0 with
            | Cool_String(s1,l1), Cool_String(s2,l2) ->
              Cool_String(s1 ^ s2, l1 + l2), s
            | _,_ -> failwith "bad String.concat"
          end
        | "String.length" ->
          begin
            match so with
            | Cool_String(_,l) -> Cool_Int(Int32.of_int l), s
            | _ -> failwith "bad String.length"
          end
        | "String.substr" ->
          begin
            let addr1, addr2 = List.assoc "i" e, List.assoc "l" e in
            let v1, v2   = List.assoc addr1 s, List.assoc addr2 s in
            match so,v1,v2 with
            | Cool_String(s_,sl), Cool_Int(i), Cool_Int(l) ->
              begin
                let sub_s =
                  try (String.sub s_ (Int32.to_int i) (Int32.to_int l) ) with
                  | _ ->
                    printf "ERROR: %s: Exception: String.substr out of range\n" line_;
                    exit 1
                in
                Cool_String(sub_s, (Int32.to_int l)), s
              end
            | _,_,_->
              failwith "bad substr args"
          end
        | _ -> failwith "unkown system call"
      end
  in
  (** returns result  **)
  (new_val, new_store)




let main () = begin


  let my_exp = ("","",DDispatch(("","",New("Main","")), ("main", ""), [])) in
  let so = Void in
  let store = [] in
  let environment = [] in
  eval so store environment my_exp;


end;;

main ();;
