(* list functions *)
fun sum_list(listy : int list) =
  if null listy
  then 0
  else hd listy + sum_list(tl listy)

fun product_list(listy : int list) =
  if null listy
  then 1
  else hd listy * product_list(tl listy)

fun countdown(num : int) =
  if num = 0
  then []
  else num::countdown(num -1)

(* all xs followed by ys *)
fun append(xs : int list , xy : int list) =
  if null xs
  then xy
  else hd xs :: append(tl xs,xy)

(* sum_pair_list([(1,2),(3,4)]) -> 10 *)
fun sum_pair_list(xp : (int * int) list) =
  if null xp
  then 0
  else #1 (hd xp) + #2 (hd xp) + sum_pair_list(tl xp)

(* firsts([(1,2),(3,4)]) -> [1,3] *)
fun firsts(xp : (int * int) list) =
  if null xp
  then []
  else #1 (hd xp) :: firsts(tl xp)

(* secs([(1,2),(3,4)]) -> [2,4] *)
fun secs(xp : (int * int) list) =
  if null xp
  then []
  else #2 (hd xp) :: secs(tl xp)

(* sum_pair_list2([(1,2),(3,4)]) -> 10 *)
fun sum_pair_list2(xp : (int * int) list) =
  if null xp
  then 0
  else sum_list(append(firsts(xp),secs(xp)))

(* sum_pair_list3([(1,2),(3,4)]) -> 10 *)
fun sum_pair_list3(xp : (int * int) list) =
  if null xp
  then 0
  else sum_list(firsts(xp)) + sum_list(secs(xp))

fun map(f,xs) =
  case xs of
       [] => []
     | head::xs' =>  (f head)::(map(f,xs'))

fun filter(f,xs) =
  case xs of
       [] => []
     | x::xs' => if f x then x::filter(f,xs') 
                        else filter(f,xs')
