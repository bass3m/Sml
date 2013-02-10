fun map(f,xs) =
  case xs of
       [] => []
     | head::xs' =>  (f head)::(map(f,xs'))

fun filter(f,xs) =
  case xs of
       [] => []
     | x::xs' => if f x then x::filter(f,xs')
                        else filter(f,xs')
fun n_times(f,n,x) =
  case n of
       0 => x
     | n => f (n_times(f,n-1,x))

fun triple_n_times(n,x) = n_times((fn y => y*3),n,x)

fun fold(f,acc,xs) =
  case xs of
       [] => acc
     | x::xs' => fold(f,f(acc,x),xs')

fun range(x) =
  case x of
       0 => []
     | x' => range(x'-1) @ [x']
(*silly factorial*)
fun fact(x) = fold(fn (x,y) => x*y, 1,range(x))

fun fold_curry f acc xs =
  case xs of
       [] => acc
     | x::xs' => fold_curry f (f(acc,x)) xs'

fun zip xs ys =
  case (xs,ys) of
       ([],[]) => []
     | (x::xs',y::ys') => (x,y)::(zip xs' ys')
     | _ => raise Empty

(*from_to 1 4 => [1,2,3,4]*)
fun from_to i j =
  if i > j then []
  else i::from_to (i+1) j

(*last item in list*)
fun lasty lst =
  case lst of
       [] => raise Empty
     | [x] => x
     | x::l => lasty l

fun is_member x lst =
  case lst of
       [] => false
     | h::t => x = h orelse is_member x t

(*all_answers (fn x => if (isSome x) then SOME ([valOf x]) else NONE) [SOME 1,SOME 2,SOME 3];*)
fun find_min lst =
  let fun aux(ls,acc) =
        case ls of
             [] => acc
           | h::t => if h < acc then aux(t,h) else aux(t,acc)
  in
    case lst of
         [] => []
       | h::t => aux(t, h) :: find_min t
  end

(*insert an item into a sorted list*)
fun insert_in_sorted (item,lst) =
  case lst of
       [] => [item]
     | h::t => if item < h then item::lst else h::insert_in_sorted(item,t)

(*curried version*)
fun insert item lst =
  case lst of
       [] => [item]
     | h::t => if item < h then item::lst else h::insert item t

(*this is insertion sort*)
fun sort_list(lst) =
  case lst of
       [] => []
     | h::t => insert_in_sorted(h,sort_list(t))

(*sorting the folding way*)
fun sorty xs =
  List.foldr insert_in_sorted [] xs

(*merge two ordered lists*)
fun cmp_lists(xs,ys) =
  case (xs,ys) of
       ([],[]) => []
     | (xs',[]) => xs'
     | ([],ys') => ys'
     | (x::xs',y::ys') => if x < y then x :: cmp_lists(xs',y::ys')
                                   else y:: cmp_lists(x::xs',ys')

(*check if element exists in list*)
fun check_elem (el,xs) =
     List.foldl (fn (x,acc) => if x = el then true else acc) false xs

(*add 3 to a list, a more expensive operation since using @*)
fun add_3_bad lst =
  List.foldl (fn (x,acc) => acc@[x+3]) [] lst

(*add 3 to a list, a better solution since using :: operation *)
fun add_3_good lst =
List.foldr (fn (x,acc) => (x+3)::acc) [] lst

(*filter using folds*)
fun filt f xs =
  List.foldr (fn (x,acc) => if f x then x::acc else acc) [] xs

(*reverse list using fold*)
fun revlst lst =
  List.foldl (fn (x,acc) => x::acc) [] lst

(*Haskell quick sort*)
(*quicksort :: (Ord a) => [a] -> [a]  *)
(*quicksort [] = []  *)
(*quicksort (x:xs) =   *)
    (*let smallerSorted = quicksort [a | a <- xs, a <= x]  *)
        (*biggerSorted = quicksort [a | a <- xs, a > x]  *)
    (*in  smallerSorted ++ [x] ++ biggerSorted  *)
(*thanks for F# inspiration*)

(*F# quicksort*)
(*let rec quicksort list =*)
   (*match list with*)
   (*| [] ->                            // If the list is empty*)
        (*[]                            // return an empty list*)
   (*| firstElem::otherElements ->      // If the list is not empty     *)
        (*let smallerElements =         // extract the smaller ones    *)
            (*otherElements             *)
            (*|> List.filter (fun e -> e < firstElem) *)
            (*|> quicksort              // and sort them*)
        (*let largerElements =          // extract the large ones*)
            (*otherElements *)
            (*|> List.filter (fun e -> e >= firstElem)*)
            (*|> quicksort              // and sort them*)
        (*// Combine the 3 parts into a new list and return it*)
        (*List.concat [smallerElements; [firstElem]; largerElements]*)

fun quicksort xs =
  case xs of
       [] => []
     | h::t => let fun small ys = quicksort (List.filter (fn x => x < h) ys)
                   fun big ys = quicksort (List.filter (fn x => x >= h) ys)
               in
                 small t @ (h) :: big t
               end

fun gcd x y =
  if (x mod y) = 0
  then y
  else gcd y (x mod y)

fun sum x =
  List.foldl (op +) 0 x
