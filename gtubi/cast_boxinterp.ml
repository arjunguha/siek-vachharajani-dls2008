open Support.Pervasive
open Support.Error
open Printf

open Cast_ast

exception Stuck of (info*expr)
exception CastFailed of (info*info*ty_ctx*ty*expr*expr)

let rec is_value exp = 
  match exp with
  | ValueE (i,t,e) -> true
  | CastE (_, _, t1, ValueE(_,t2,_)) when (not (type_equal t1 t2))-> true
  | _ -> false

let rec print_value e = 
  match e with
  | ValueE (i,t,v) -> 
      print_expr v
  | CastE (_, ctx, t1, (ValueE(_,_,_) as v)) -> 
      sprintf "<%s>%s" (print_ty_ctx ctx t1) (print_value v)
  | _ -> error UNKNOWN ("Cannot pretty print non-value: " ^ (print_expr e))

let rec box_values env e =
  match e with
    | VarE (i,alpha) ->
	let t = 
	  (try lookup env alpha with
	     | Not_found -> error i (sprintf "Variable not in environment, %s"
				       (print_expr e)))
	in (t,e) 
    | IntE (i,n) -> (IntT i, ValueE (i, IntT i, e))
    | BoolE (i,n) -> (BoolT i, ValueE (i, BoolT i, e))
    | LamE (i, x, t, body) ->
	let (btyp, bbod) = box_values ((x,t)::env) body in
	let etyp = ArrowT (i, t, btyp) in
	  (etyp, ValueE (i, etyp, LamE(i, x, t, bbod)))
    | AppE (i, e1, e2) -> 
	let (t1, be1) = box_values env e1 in
	let (t2, be2) = box_values env e2 in
	  (match t1 with
	     | ArrowT (i, s,t) when (type_equal t2 s) -> (t, AppE(i, be1, be2))
	     | DynT i -> (DynT i, AppE(i, be1, be2))
	     | _ -> error i (sprintf 
			       "Ill-typed App expression in box_values: %s, %s, %s" 
                               (print_expr e)
			       (print_type t1)
                               (print_type t2))
	  )
    | CastE (i, ctx, t, e) ->
	let (t2,be) = box_values env e in
	  if (type_consistent t t2) then (t, CastE (i, ctx, t, be))
	  else error i (sprintf "Guaranteed to fail cast expression %s"
			  (print_expr e))
    | ValueE (i, t, e) ->
	let (t2,be) = box_values env e in
	  if (type_equal t t2) then 
	    (t, be)
	    (* This was causing stuff to get double wrapped. -Jeremy
               (t, ValueE(i,t,be))    *)
	  else error i (sprintf "Illegal value box expression %s"
			  (print_expr e))

let rec subst_var ((n,v) as sub) expr = 
  match expr with
  | IntE (i,n) -> expr
  | BoolE (i,b) -> expr
  | ValueE (i,t,e) -> 
      ValueE (i, t, subst_var sub e)
  | LamE (i,s,t,e) -> 
      if (s=n) then expr else LamE (i, s, t, (subst_var sub e))
  | VarE (i,s) -> if (s=n) then v else expr
  | AppE (i,e1,e2) -> 
      AppE (i, subst_var sub e1, subst_var sub e2)
  | CastE (i, ctx, t, e) -> 
      CastE (i, ctx, t, subst_var sub e)

(*
let apply_succ e2 =
  match e2 with
  | ValueE (i,t,v) -> (
      match v with
      | ConstE (i,c) -> (
	  match c with
	  | IntC x -> ValueE (i, (IntT i), ConstE (i, IntC (x+1)))
	  | _ -> raise (Stuck (i,e2))
	 )
      | _ -> raise (Stuck (i,e2))
     )
  | _ -> error dummyinfo ("Should never attempt to apply succ with a non-value argument" ^ (print_expr e2))

let apply_prev e2 =
  match e2 with
  | ValueE (i,t,v) -> (
      match v with
      | ConstE (i,c) -> (
	  match c with
	  | IntC x -> ValueE (i, (IntT i), ConstE (i, IntC (x-1)))
	  | _ -> raise (Stuck (i,e2))
	 )
      | _ -> raise (Stuck (i,e2))
     )
  | _ -> error dummyinfo ("Should never attempt to apply prev with a non-value argument" ^ (print_expr e2))

let apply_iszero e2 =
  match e2 with
  | ValueE (i,t,v) -> (
      match v with
      | ConstE (i,c) -> (
	  match c with
	  | IntC x -> ValueE (i, (BoolT i), ConstE (i, BoolC (x=0)))
	  | _ -> raise (Stuck (i,e2))
	 )
      | _ -> raise (Stuck (i,e2))
     )
  | _ -> error dummyinfo ("Should never attempt to apply iszero with a non-value argument" ^ (print_expr e2))

let apply_fix i typ v = 
  (*let vt = get_value_type v in*)
  let fixt = ArrowT (i, ArrowT (i, typ, typ), typ) in
  (match typ with
      ArrowT (i, xt, _) ->
	  AppE (i, v, 
	       ValueE (i, typ, LamE (i, "x", xt, 
				    AppE(i, 
					AppE (i,ValueE(i, fixt, ConstE(i, Fix typ)),v),
					VarE (i,"x")))))
    | DynT i ->
	let xt = DynT i in
	  AppE (i, v, 
	       ValueE (i, typ, LamE (i, "x", xt, 
				    AppE(i, 
					AppE (i,ValueE(i, fixt, ConstE(i, Fix typ)),v),
					VarE (i,"x")))))
    | _ -> raise (Stuck (i,ConstE (i, Fix typ))))
*)

let rec apply i e1 e2 = 
  match e1 with 
  | ValueE (i,t,v) -> ( 
      match v with
	| LamE (i,arg,t,body) -> subst_var (arg,e2) body 
(*
	| ConstE (i,c)-> (
	    match c with 
	      | Succ -> apply_succ e2 
	      | Prev -> apply_prev e2 
	      | IsZero -> apply_iszero e2 
              | Fix typ -> apply_fix i typ e2 
	      | _ -> raise (Stuck (i,e1)) 
          ) 
 *)
	| _ ->
	    printf "applying a non-function\n";
	    raise (Stuck (i,v)) 
    ) 
  | CastE (j, ctx, ArrowT (k, s1, s2), (ValueE (l, ArrowT (m, t1, t2), _) as f)) ->
      CastE (j, RightArrow (k, ctx, s1), s2, AppE (i, f, (CastE (j, Hole, t1, e2))))

  | _ ->
      error UNKNOWN ("Should never attempt to apply a non-value" ^ (print_expr e1))

let reduce i expr =
  match expr with
      AppE (i,e1,e2) -> apply i e1 e2
    | CastE (i, ctx, t2, (ValueE (_, t1, _) as v)) when (type_equal t1 t2) -> v
    | CastE (i, ctx, t2, CastE (_, _, _, (ValueE (_, t1, _) as v))) 
        when type_equal t1 t2 -> v
    | CastE (i, ctx2, t2, CastE (j, ctx3, t3, (ValueE (_, t1, ve) as v))) -> 
	if type_consistent t1 t2 then CastE (i, ctx2, t2, v)
	else raise (CastFailed (i,j,ctx2,t2,ve,expr))
    | _ -> raise (Stuck (i, expr))

let rec step expr = 
  match expr with
    | AppE (i,e1,e2) -> (
	match (is_value e1, is_value e2) with
	  | (true,true) -> reduce i expr
	  | (false,_) -> AppE (i, step e1, e2)
	  | (true,false) -> AppE (i, e1, step e2)
      )
    | CastE (i,ctx,t,e) -> 
	if (is_value e) then reduce i expr
	else CastE (i, ctx, t, step e)
    | IntE (i,_)
    | BoolE (i,_)
    | LamE (i,_,_,_)
    | ValueE (i,_,_)
    | VarE (i,_) -> raise (Stuck (i, expr))

let rec run e t =
  let (et,e) = box_values [] e
  in 
    if type_equal t et then
      let t = et in
	if is_value e then
	  e
	else
	  let e = step e in
	    if !Parser_misc.debug then
	      printf "---> %s\n" (Cast_ast.print_expr e);
	    let et = typecheck [] e in
	      if type_equal t et then
		run e t
	      else
		error UNKNOWN "Type changed!\n"
    else
      error UNKNOWN "Type changed after boxing values!\n"

	
