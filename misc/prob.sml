(*I'm going through some of this haskell problem set*)
(*http://www.haskell.org/haskellwiki/99_questions/1_to_10*)
fun my_last(lst) =
  case lst of
       [] => NONE
     | last::[] => SOME last
     | item::lst' => my_last(lst')


fun my_bef_last(lst) =
  case lst of
       [] => NONE
     | last::[] => NONE
     | bef::(last::[]) => SOME bef
     | item::lst' => my_bef_last(lst')

(*not part of the problem set so far, but wanted to just do it*)
(*since saw that question on stackoverflow*)
fun find_max(lst) =
    let fun aux(cs,acc) =
        case (cs,acc) of
             ([],acc) => acc
           | (num::cs',acc) => if num > acc
                               then aux(cs',num)
                               else aux(cs',acc)
    in
      aux(tl lst,hd lst)
    end

fun element_at(lst,index) =
  case (lst,index) of
       ([],_) => NONE
     | (element::_,0) => SOME element
     | (element::rest,index) => element_at(rest,index-1)

fun len_list(lst) =
  case lst of
       [] => 0
     | el::rest => 1 + len_list(rest)

fun rev_list(lst) =
  case lst of
       [] => []
     | head::rest => rev_list(rest) @ [head]

(*works, but prefer a better method ?*)
fun is_palindrome(lst) =
  case lst of
       [] => true
     | head::[] => true
     | lst => let val rev_list = rev_list(lst)
              in
                if rev_list = lst then true
                else false
              end

