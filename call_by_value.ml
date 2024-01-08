(* 新しい変数の生成 *)
let tempNum = ref 0
let newTemp str = (tempNum:= (!tempNum)+1; str^(string_of_int (!tempNum)))

(* ラムダ式の構文定義 *)
type exp = Lambda of string * exp | App of exp * exp | Var of string

(* 自由変数の検出 *)
let rec fv ast = match ast with
     (Var str) -> str::[]
  |  (App (m,n)) -> (fv m)@(fv n)
  |  (Lambda (x, m)) -> List.filter (fun y -> y <> x )  (fv (m))

exception No_such_selection

(* β変換 *)
let rec beta m x n = match m with
    (Var strZ) -> if (Var strZ) = x then n else (Var strZ)
  | (App (m1,m2)) -> App (beta m1 x n, beta m2 x n)
  | (Lambda (strY,m)) -> 
         if ((Var strY) = x) then (Lambda (strY,m)) 
         else if ((not (List.exists (fun elem -> elem = strY) (fv n))) 
		|| (not (List.exists (fun elem -> (Var elem) = x) (fv m)))) then 
                     (Lambda (strY, beta m x n))
         else let tmp = newTemp strY in
                beta (Lambda (tmp, beta m (Var strY) (Var tmp))) x n
  | _ -> raise No_such_selection

(* 正規形判定 *)
let rec isNormal ast = match ast with
     (App ((Lambda _), _)) -> false
  |  (App (f,a)) -> if (isNormal f) && (isNormal a) then true
                           else false
  |  (Lambda (f,b)) -> isNormal b
  |  _ -> true

(* 値渡し戦略ステップ *)
let rec call_by_value_step ast = match ast with
     (App ((Lambda (f,b)), a)) -> if (isNormal a) then beta b (Var f) a else App ((Lambda (f,b)), call_by_value_step a)
  |  (App (f,a)) -> if (isNormal f) then App (f,call_by_value_step a)
                           else App(call_by_value_step f,a)
  |  (Lambda (f,b)) -> Lambda (f, (call_by_value_step b))
  |  x -> x

(* 式の印字 *)
let rec print_term ast = match ast with
     (App (f,a)) -> (print_string "( "; print_term f; print_string " "; 
                             print_term a; print_string " )")
  |  (Lambda (f,b)) -> (print_string "( lambda "; print_string f; print_string ". "; print_term b;
                                 print_string " )")
  |  (Var x) -> print_string x

(* メイン関数 *)
let call_by_value ast = 
    let rlt = ref ast
        and count = ref 1 in 
      while (not (isNormal (!rlt))) do 
        rlt := call_by_value_step (!rlt);
	print_string "["; print_string (string_of_int (!count)); print_string "]=> ";
	print_term (!rlt);
	count := !count + 1;
        read_line()
      done


(* ラムダ計算の例 *)
let add = Lambda ("x", Lambda ("y", Lambda ("p", Lambda ("q", 
          App (App (Var "x", Var "p"), App (App (Var "y", Var "p"), Var "q"))))))
let mult = Lambda ("x", Lambda ("y", Lambda ("z", App (Var "x", App (Var "y", Var "z")))))
let exp = Lambda ("x", Lambda ("y", App (Var "y", Var "x")))
let c0 = Lambda ("f", Lambda ("x", Var "x"))
let c1 = Lambda ("f", Lambda ("x", App (Var "f", Var "x")))
let c2 = Lambda ("f", Lambda ("x", App (Var "f", App (Var "f", Var "x"))))
let c3 = Lambda ("f", Lambda ("x", App (Var "f" ,App (Var "f", App (Var "f", Var "x")))))
let _true = Lambda ("x", Lambda ("y", Var "x"))
let _false = Lambda ("x", Lambda ("y", Var "y"))
let y = Lambda ("f", App (Lambda ("x", App (Var "f", App (Var "x", Var "x"))), 
                          Lambda ("x", App (Var "f", App (Var "x", Var "x")))))
let iszero = Lambda ("n", App (App (Var "n", Lambda ("x", _false)), _true))
let fst = Lambda ("x", App (Var "x", _true))
let snd = Lambda ("x", App (Var "x", _false))
let pair = Lambda ("m", Lambda ("n", Lambda ("z", App (App (Var "z", Var "m"), Var "n"))))
let zz = App(App (pair, c0), c0)
let ss = Lambda ("p", App (App ( pair, App (snd, Var "p")), App(App (add, c1), App (snd, Var "p"))))
let pred = Lambda ("m", App (fst, App (App (Var "m", ss), zz)))
let fact = Lambda ("f", Lambda ("n", App (App (App (iszero, Var "n"), c1), App (App (mult, Var "n"), 
           App (Var "f", App (pred, Var "n"))))))
let s = Lambda ("x", Lambda ("y", Lambda ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let i = Lambda ("x", Var "x")
let test = App (Var "x", App(App(App (s, App (i, Var "x")), 
App (i, App(i,i))), Var "z"))
let succ = Lambda ("x", App(App (pair, _false), Var "x"))
let pred2 = Lambda ("x", App (Var "x", _false)) 
let iszero2 = Lambda ("x", App (Var "x", _true))
