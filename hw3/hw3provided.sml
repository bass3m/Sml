(* Coursera Programming Languages, Homework 3, Provided Code *)
exception NoAnswer

(**** you can put all your code here ****)

(*val only_capitals = fn : string list -> string list*)
fun only_capitals(strlist) =
  List.filter (fn s => Char.isUpper(String.sub(s,0))) strlist

(*val longest_string1 = fn : string list -> string*)
fun longest_string1(slist) =
  if slist = [] then ""
  else
  List.foldl (fn (s,m) => if (String.size m) >= (String.size s) then m else s) (hd slist) slist

(*val longest_string2 = fn : string list -> string*)
fun longest_string2(slist) =
  if slist = [] then ""
  else
  List.foldl (fn (s,m) => if (String.size m) > (String.size s) then m else s) (hd slist) slist

(*val longest_string_helper = fn : (int * int -> bool) -> string list -> string*)
fun longest_string_helper f slist =
  List.foldl (fn (s,m) => if f ((String.size s),(String.size m)) then s else m) "" slist

(*val longest_string3 = fn : string list -> string*)
fun longest_string3 slist =
    let
      fun str_longer (s,m) = s > m
    in
      longest_string_helper str_longer slist
    end

(*val longest_string4 = fn : string list -> string*)
fun longest_string4 slist =
    let
      fun str_longer (s,m) = s >= m
    in
      longest_string_helper str_longer slist
    end

(*val longest_capitalized = fn : string list -> string*)
val longest_capitalized = longest_string1 o only_capitals

(*val rev_string = fn : string -> string*)
fun rev_string str =
    (String.implode o List.rev o String.explode) str

(*val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b*)
fun first_answer f lst =
  case lst of
       [] => raise NoAnswer
      | x::t => case f x of
                NONE => first_answer f t
              | SOME y => y

(*val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option*)
fun all_answers f lst =
  let fun aux(lst,acc) =
    case lst of
         [] => SOME acc
       | h::t => case f h of
                  NONE => NONE
                | SOME x => aux(t,x @ acc)
  in
    aux(lst,[])
  end

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

(*val g = fn : (unit -> int) -> (string -> int) -> pattern -> int*)
fun g f1 f2 p =
    let
    val r = g f1 f2
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(*val count_wildcards = fn : pattern -> int*)
fun count_wildcards p =
    g (fn x => 1) (fn y => 0) p

(*val count_wild_and_variable_lengths = fn : pattern -> int*)
fun count_wild_and_variable_lengths p =
    g (fn x => 1) (String.size) p

(*val count_some_var = fn : string * pattern -> int*)
fun count_some_var (st,p) =
  g (fn x => 0) (fn s => if st = s then 1 else 0) p

(*val check_pat = fn : pattern -> bool*)
fun check_pat p =
    let fun get_str(p) =
        case p of
            Wildcard          => []
          | Variable x        => [x]
          | TupleP ps         => List.foldl (fn (p,s) => s @ get_str(p)) [] ps
          | ConstructorP(_,p) => get_str(p)
          | _                 => []
    fun check_for_dups(sl) =
        case sl of
             [] => true
           | h::t => if List.exists (fn x => h = x) t then false
                     else check_for_dups(t)
    in
        check_for_dups(get_str(p))
    end

(*val match = fn : valu * pattern -> (string * valu) list option*)
fun match (v,p) =
    case (v,p) of
         (_,Wildcard) => SOME []
       | (Unit,UnitP) => SOME []
       | (v,Variable s) => SOME [(s,v)]
       | (Const x,ConstP y) => if x = y then SOME [] else NONE
       | (Constructor(s1,v2),ConstructorP(s2,p2)) => if (s1 = s2) then match(v2,p2) else NONE
       | (Tuple vl,TupleP pl) => (all_answers (match) (ListPair.zipEq(vl,pl))
                                  handle ListPair.UnequalLengths => NONE)
       | _ => NONE

(*val first_match = fn : valu -> pattern list -> (string * valu) list option*)
fun first_match v plist =
  let fun curry f x y = f(x,y)
  in
    SOME (first_answer (curry match v) plist)
    handle NoAnswer => NONE
  end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string
