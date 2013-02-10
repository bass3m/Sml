(* Homework 1 *)

(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
(* year : +ve num , month : from 1-12 , day : from 1-31 *)
fun is_older(date1 : int * int * int , date2 : int * int * int) =
  (#1 date1 < #1 date2) orelse 
  (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
  (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(*val number_in_month = fn : (int * int * int) list * int -> int*)
fun number_in_month(lst : (int * int * int) list, mon : int) =
  if null lst
  then 0
  else if #2 (hd lst) = mon
  then number_in_month(tl lst,mon) + 1
  else number_in_month(tl lst,mon)

(* val number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months(lst : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(lst,hd months) + number_in_months(lst,tl months)

(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month(dates : (int * int * int) list, mon : int) =
  if null dates
  then []
  else
    if  #2 (hd dates) = mon 
    then hd dates :: dates_in_month(tl dates,mon)
    else dates_in_month(tl dates,mon)

(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months(dates : (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates,hd months) @ dates_in_months(dates,tl months)

(*val get_nth = fn : string list * int -> string*)
fun get_nth(str_list : string list, nth : int) =
  if null str_list
  then ""
  else
    if nth = 1
    then hd str_list
    else get_nth(tl str_list,nth-1)

(*val date_to_string = fn : int * int * int -> string*)
val months_strings = ["January", "February",
                      "March", "April",
                      "May", "June",
                      "July", "August",
                      "September", "October",
                      "November", "December"];

fun date_to_string(date : int * int * int) =
    get_nth(months_strings,#2 date) ^ " " ^
    Int.toString(#3 date) ^ ", " ^
    Int.toString(#1 date)

(*val number_before_reaching_sum = fn : int * int list -> int*)
fun number_before_reaching_sum(num : int, num_list : int list) =
let
  fun sum(num_list : int list,current_sum : int, current_index : int) =
    if null num_list
    then current_index
    else
      if current_sum + hd num_list >= num
      then current_index
      else sum(tl num_list,hd num_list + current_sum,current_index + 1)
in
  sum(num_list,0,0)
end

val days_in_month = [31,  (* Jan *)
                     28,  (* Feb - non leap *)
                     31,  (* Mar *)
                     30,  (* Apr *)
                     31,  (* May *)
                     30,  (* Jun *)
                     31,  (* Jul *)
                     31,  (* Aug *)
                     30,  (* Sep *)
                     31,  (* Oct *)
                     30,  (* Nov *)
                     31]; (* Dec *)
(*val what_month = fn : int -> int*)
fun what_month(day_of_year : int) =
  (* 1 based *)
  number_before_reaching_sum(day_of_year,days_in_month) + 1

(*val month_range = fn : int * int -> int list*)
(* m1,m1,m1,m2,....,mn,mn len -> day2 - day1 + 1 *)
fun month_range(day_start : int, day_end : int) =
let
  fun month_to_list(day : int, month_list : int list) =
    if day > day_end
    then month_list
    else
      month_to_list(day + 1,month_list @ [what_month(day)])
in
  month_to_list(day_start,[])
end

(*val oldest = fn : (int * int * int) list -> (int * int * int) option*)
fun oldest(date_list : (int * int * int) list) =
  if null date_list
  then NONE
  else if null (tl date_list) 
    then SOME (hd date_list)
  else
    let val the_oldest = oldest(tl date_list)
    in
    if is_older(hd date_list,valOf the_oldest)
    then SOME (hd date_list)
    else the_oldest
    end

fun filter_duplicates(dup_list : int list) =
  if null dup_list
  then []
  else
  let 
    fun num_in_list(l : int list, num : int) =
      if null l
      then false
      else if hd l = num
      then true
      else num_in_list(tl l,num)
    fun unique_list(ulist : int list, newl: int list) = 
      if null ulist
      then newl
      else if num_in_list(newl, hd ulist)
      then unique_list(tl ulist, newl)
      else unique_list(tl ulist, newl @ [hd ulist])
  in
    unique_list(dup_list, [])
  end

fun number_in_months_challenge(lst : (int * int * int) list, months : int list) =
  if null months
  then 0
  else
  let val unique_months = filter_duplicates(months)
  in
  number_in_month(lst,hd unique_months) + number_in_months(lst,tl unique_months)
  end

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else
  let val unique_months = filter_duplicates(months)
  in
  dates_in_month(dates,hd unique_months) @ dates_in_months(dates,tl unique_months)
  end
