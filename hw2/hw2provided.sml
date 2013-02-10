(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*val all_except_option = fn : string * string list -> string list option*)
fun all_except_option (pattern,stlist) =
  let fun aux(stlist,acclist,found) =
    case (stlist,acclist,found) of
         ([],acclist,false) => NONE
       | ([],acclist,true) => SOME acclist
       | (st::stl,acclist,found) => if same_string(st,pattern)
                                    then aux(stl,acclist,true)
                                    else aux(stl,acclist @ [st],found)
  in
           aux(stlist,[],false)
  end

(*val get_substitutions1 = fn : string list list * string -> string list*)
fun get_substitutions1(strlist,str) =
  case (strlist,str) of
       ([],_) => []
     | (st::stlist,_) => let val find_sub = all_except_option(str,st)
                         in
                           case find_sub of
                                NONE => get_substitutions1(stlist,str)
                              | SOME sub => sub @ get_substitutions1(stlist,str)
                         end

(*val get_substitutions2 = fn : string list list * string -> string list*)
fun get_substitutions2(strlist,str) =
  let fun aux(stlist,acc) =
      case (stlist,acc) of
           ([],acc) => acc
         | (strlist'::stl,acc) => let val find_sub = all_except_option(str,strlist')
                                  in
                                    case find_sub of
                                         NONE => aux(stl,acc)
                                       | SOME sub => aux(stl,acc @ sub)
                                  end
  in
    aux(strlist,[])
  end

(*val similar_names = fn : string list list * {first:string, last:string, middle:string}*)
(*-> {first:string, last:string, middle:string} list*)
fun similar_names(strlist,{first,middle,last}) =
  let fun aux(sublist,acc) =
  case (sublist,acc) of
       ([],acc) => acc
     | (head::sublist',acc) => aux(sublist',
              acc @ [{first=head,middle=middle,last=last}])
  in
     aux(get_substitutions2(strlist,first),[{first=first,middle=middle,last=last}])
  end

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(*val card_color = fn : card -> color*)
fun card_color(card) =
  case (card) of
       (Clubs,_) => Black
     | (Spades,_) => Black
     | (_,_) => Red

(*val card_value = fn : card -> int*)
fun card_value(card) =
  case card of
       (_,Ace) => 11
     | (_,Num(i)) => i
     | (_,_) => 10

(*val remove_card = fn : card list * card * exn -> card list*)
fun remove_card(cs,c,e) =
    case cs of
         [] => raise e
       | crd::cs' => if crd = c then cs'
                                else crd::remove_card(cs',c,e)

(*val all_same_color = fn : card list -> bool*)
fun all_same_color(cs) =
  case cs of
       [] => true
     | _::[] => true
     | card::(next_card::cs') => (card_color(card) = card_color(next_card))
     andalso all_same_color(next_card::cs')

(*val sum_cards = fn : card list -> int*)
fun sum_cards(cs) =
  let fun aux(cs',acc) =
    case cs' of
      [] => acc
    | card::cs' => aux(cs',acc + card_value(card))
  in
    aux(cs,0)
  end

(*helper function: used by score and score_challenge*)
fun score_from_sum(sum,cs,goal) =
    let fun calc_color_score(prelim_score) =
            case all_same_color(cs) of
               true => prelim_score div 2
             | false => prelim_score
    in
      case sum > goal of
           true => calc_color_score(3*(sum - goal))
         | false => calc_color_score(goal - sum)
    end

(*val score = fn : card list * int -> int*)
fun score(cs,goal) =
    score_from_sum(sum_cards(cs),cs,goal)

(*val officiate = fn : card list * move list * int -> int*)
fun officiate(cs,moves,goal) =
    let fun process_move(cs',ms,held_cards) =
        case (cs',ms,held_cards) of
             (_,[],held_cards) => score(held_cards,goal)
           | ([],Draw::_,held_cards) => score(held_cards,goal)
           | (card::cs',Draw::ms',held_cards) => if (sum_cards(card::held_cards) > goal)
                                          then score(card::held_cards,goal)
                                          else process_move(cs',ms',card::held_cards)
           | (cs',Discard(card)::ms',held_cards) =>
               process_move(cs',ms',remove_card(held_cards,card,IllegalMove))
    in
      process_move(cs,moves,[])
    end

(*val score_challenge = fn : card list * int -> int*)
fun score_challenge(cs,goal) =
  let fun count_aces(cs,aces) =
    case (cs,aces) of
         ([],aces) => aces
       | ((_,Ace)::cs',aces) => count_aces(cs',aces+1)
       | ((_::cs'),aces) => count_aces(cs',aces)
  fun best_score(sum,aces,min_score) =
    case aces of
         0 => min_score
       | aces => let val new_score = score_from_sum(sum - (aces * 10),cs,goal)
                 in
                   if new_score < min_score
                   then best_score(sum,aces - 1,new_score)
                   else best_score(sum,aces - 1,min_score)
                 end
  val sum = sum_cards(cs)
  in
    best_score(sum,count_aces(cs,0),score_from_sum(sum,cs,goal))
  end

fun officiate_challenge(cs,moves,goal) =
    let fun process_move(cs',ms,held_cards) =
        case (cs',ms,held_cards) of
             (_,[],held_cards) => score_challenge(held_cards,goal)
           | ([],Draw::_,held_cards) => score_challenge(held_cards,goal)
           | (card::cs',Draw::ms',held_cards) => if (sum_cards(card::held_cards) > goal)
                                          then score_challenge(card::held_cards,goal)
                                          else process_move(cs',ms',card::held_cards)
           | (cs',Discard(card)::ms',held_cards) =>
               process_move(cs',ms',remove_card(held_cards,card,IllegalMove))
    in
      process_move(cs,moves,[])
    end
