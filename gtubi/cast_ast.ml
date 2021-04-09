open Support.Pervasive
open Support.Error
open Printf

type ty =
    ArrowT of info * ty * ty
  | VarT of info * string
  | DynT of info
  | IntT of info
  | BoolT of info

type ty_ctx =                               (* C::= *)
    Hole                                    (*   [] *)
  | LeftArrow of info * ty_ctx * ty         (* | C[[] -> T] *)
  | RightArrow of info * ty_ctx * ty        (* | C[T -> []] *)

type expr =
  | VarE of info * string
  | IntE of info * int
  | BoolE of info * bool
  | LamE of info * string * ty * expr
  | AppE of info * expr * expr
  | CastE of info * ty_ctx * ty * expr
  | ValueE of info * ty * expr (* Box for boxed interpreter *)

let rec plug ctx t =
  match ctx with
      Hole -> t
    | LeftArrow (i, ctx', t') ->
	plug ctx' (ArrowT (i, t, t'))
    | RightArrow (i, ctx', t') ->
	plug ctx' (ArrowT (i, t', t))
      
let get_info t =
  match t with
      VarT (i,_) 
    | ArrowT (i,_,_) 
    | IntT i
    | BoolT i
    | DynT i -> i

let get_expr_info e =
  match e with
    | VarE (i, _)
    | IntE (i,_)
    | BoolE (i,_)
    | LamE (i, _, _, _)
    | AppE (i, _, _)
    | CastE (i, _, _, _)
    | ValueE (i, _, _) -> i

let rec type_equal t1 t2 =
  match (t1,t2) with
      (ArrowT (_,arg1,ret1), ArrowT (_,arg2,ret2)) -> 
	(type_equal arg1 arg2) && (type_equal ret1 ret2)
    | (VarT (_,nm1), VarT (_,nm2)) -> nm1 = nm2
    | (DynT _, DynT _)
    | (IntT _, IntT _)
    | (BoolT _, BoolT _) -> true
    | (_,_) -> false

let rec type_consistent t1 t2 =
  match (t1,t2) with
    | (DynT _, _)
    | (_, DynT _) -> true
    | (ArrowT (_,arg1,ret1), ArrowT (_,arg2,ret2)) -> 
	(type_consistent arg1 arg2) && (type_consistent ret1 ret2)
    | (VarT (_,nm1), VarT (_,nm2)) -> nm1 = nm2
    | (IntT _, IntT _)
    | (BoolT _, BoolT _) -> true
    | (_,_) -> false

let rec type_consistent_with_blame t1 t2 t3 =
  match (t1,t2,t3) with
    | (DynT i, _, _)
    | (_, DynT i, _) -> (i, true)
    | (ArrowT (_,arg1,ret1), ArrowT (_,arg2,ret2), DynT i) -> 
	let (b,ok) = type_consistent_with_blame arg1 arg2 (DynT i) in
	  if ok then
	    type_consistent_with_blame ret1 ret2 (DynT i) 
	  else
	    (b,ok)
    | (ArrowT (_,arg1,ret1), ArrowT (_,arg2,ret2), 
       ArrowT (_,arg3,ret3)) -> 
	let (b,ok) = type_consistent_with_blame arg1 arg2 arg3 in
	  if ok then
	    type_consistent_with_blame ret1 ret2 ret3 
	  else
	    (b,ok)
    | (VarT (i,nm1), VarT (_,nm2), _) -> 
	(i, nm1 = nm2 )

    | (IntT i, IntT _, _)
    | (BoolT i, BoolT _, _) -> (i, true)
    | (_,_, t) -> 
	(get_info t, false)

let rec print_type t =
  match t with
      VarT (_,alpha) -> alpha
    | ArrowT (_,s,t) -> 
	sprintf "(%s -> %s)" (print_type s) (print_type t)
    | IntT _ -> "int"
    | BoolT _ -> "bool"
    | DynT _ -> "?"

let print_ty_ctx ctx t =
  let rec loop top ctx t = 
    match ctx with
	Hole -> 
	  if top then (print_type t)
	  else sprintf "**%s**" (print_type t)
      | LeftArrow (i, ctx', t') ->
	  sprintf "(%s -> %s)" (loop false ctx' t) (print_type t')
      | RightArrow (i, ctx', t') ->
	  sprintf "(%s -> %s)" (print_type t') (loop false ctx' t)
  in loop true ctx t

let rec print_expr e =
  match e with
    | VarE (i, x) -> x
    | IntE (i,n) -> sprintf "%d" n
    | BoolE (i,n) -> sprintf "%b" n
    | LamE (i, x, t, e) -> 
	sprintf "(fun %s : %s. %s)" x 
	  (print_type t) (print_expr e)
    | AppE (_, AppE (_, VarE (_, "add"), e1), e2) ->
      sprintf "(%s + %s)" (print_expr e1) (print_expr e2)
    | AppE (i, e1, e2) ->
	sprintf "(%s %s)" (print_expr e1) (print_expr e2)
    | CastE (i, ctx, t, e) ->
	sprintf "(<%s> %s)" (print_ty_ctx ctx t) (print_expr e)
    | ValueE (i, t, e) ->
	sprintf "[| <%s> %s |]" (print_type t) (print_expr e)

let get_expr_info e =
  match e with
     VarE (i,_)
    | IntE (i,_)
    | BoolE (i,_)
    | LamE (i,_,_,_)
    | AppE (i,_,_)
    | CastE (i,_,_,_)
    | ValueE (i,_,_) -> i

let rec to_cast_type typ =
  match typ with
      Graph_ast.ArrowT(i,v1,v2) -> ArrowT(i, to_cast_type (Graph_ast.get_stype v1), 
					 to_cast_type (Graph_ast.get_stype v2))
    | Graph_ast.VarT (i,alpha) -> VarT (i,alpha)
    | Graph_ast.DynT i -> DynT i
    | Graph_ast.IntT i -> IntT i
    | Graph_ast.BoolT i -> BoolT i

let lookup env alpha =
  List.assoc alpha env

let print_env env =
  let l = 
    List.map (fun (arg,typ) -> sprintf "%s -> %s" arg (print_type typ)) env 
  in List.fold_left (fun a b -> sprintf "%s, %s" a b) "" l
    
let rec insert_casts env expr =
  match expr with
      (* Fixme, raise error if lookup fails *)
      Graph_ast.VarE (i,alpha) -> (lookup env alpha, VarE (i,alpha))
    | Graph_ast.IntE (i,n) -> (IntT i, IntE (i,n))
    | Graph_ast.BoolE (i,b) -> (BoolT i, BoolE (i,b))
    | Graph_ast.LamE (i,param,typ,body) ->
        let typ = to_cast_type (Graph_ast.get_stype typ) in
	let nenv = (param,typ)::env in 
	let (bodytyp, newbody) = insert_casts nenv body in
	let etyp = ArrowT (i, typ, bodytyp) in
	  (etyp, LamE(i,param, typ, newbody))
    | Graph_ast.LetE (i,var,rhs,body) ->
	let (rhstyp, newrhs) = insert_casts env rhs in	
	let nenv = (var,rhstyp)::env in 
	let (bodytyp, newbody) = insert_casts nenv body in
	  (bodytyp, AppE (i, LamE (i, var, rhstyp, newbody), newrhs))

    | Graph_ast.AppE (i,rator,rand) ->
	let (rator_typ, nrator) = insert_casts env rator in
	let (rand_typ, nrand) = insert_casts env rand in
	  match rator_typ with
	      ArrowT(fi, arg_typ,ret_typ) ->
		if (type_equal arg_typ rand_typ) then
		  (ret_typ, AppE (i,nrator,nrand))
		else
		  (ret_typ, AppE (i, nrator, 
				 CastE (i, Hole, arg_typ, nrand)))
	    | DynT fi -> (
		match rand_typ with
		  | DynT _ -> (DynT fi, AppE (i,nrator,nrand))
		  | _ -> (DynT fi, AppE (i,nrator,CastE (i, Hole, DynT fi, nrand)))
	      )
	    | _ -> error i 
		(sprintf "Ill-typed App expression (%s %s), rand %s, rator %s, env = %s" 
		   (print_expr nrator)
		   (print_expr nrand)
		   (print_type rand_typ)
		   (print_type rator_typ)
		   (print_env env))

let rec typecheck env e =
  match e with
    | VarE (i,alpha) -> 
	(try lookup env alpha with
	   | Not_found -> error i (sprintf "Variable not in environment, %s"
				     (print_expr e)))
    | IntE (i,n) -> IntT i
    | BoolE (i,n) -> BoolT i
    | LamE (i, x, t, e) -> ArrowT (i, t, (typecheck ((x,t)::env) e))
    | AppE (i, e1, e2) -> 
	let t1 = typecheck env e1 in
	let t2 = typecheck env e2 in
	  (match t1 with
	     | ArrowT (_, s,t) -> 
		 if type_equal t2 s then
		   t
		 else
		   error i (sprintf 
			       "Ill-typed application,\n argument type %s\ndoes not match the parameter type %s\nfunction:%s\nargument:%s\n" 
			       (print_type t2) (print_type s) (print_expr e1) (print_expr e2))
	     | DynT i -> DynT i
	     | _ -> error i (sprintf 
			       "Ill-typed application, expected a function, got %s" 
			       (print_type t1))
	  )
    | CastE (i, ctx, t, e) ->
	let t2 = typecheck env e in
	  if (type_consistent t t2) then t
	  else error i (sprintf "Guaranteed to fail cast expression %s"
			  (print_expr e))
    | ValueE (i, t, e) ->
	let t2 = typecheck env e in
	  if (type_equal t t2) then t
	  else error i (sprintf "Illegal value box expression %s"
			  (print_expr e))
