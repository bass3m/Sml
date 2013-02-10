(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
use "hw2provided.sml";

fun test(test_name,tests) =
    let fun run_test(tests,passed_results,failed_results,msgs) =
        case tests of
             [] => "Results for " ^ test_name ^ ".Passed:" ^ Int.toString(passed_results)
                    ^ " Failed:" ^ Int.toString(failed_results)
           | (({actual,expected,msg})::rest) => if actual = expected
                                            then run_test(rest,
                                                passed_results+1,
                                                failed_results,msgs)
                                            else run_test(rest,
                                                passed_results,
                                                failed_results+1,msgs)
    in
      run_test(tests,0,0,"-")
    end

fun test_all_except_option() =
  let
    val words = ["hello","world","test","this"]
  in
    test("AllExcept",[{actual=all_except_option("test",words),
                       expected=SOME ["hello","world","this"],msg=""},
                      {actual=all_except_option("the",words),
                       expected=NONE,msg=""},
                      {actual=all_except_option("hello",words),
                       expected=SOME ["world","test","this"],msg=""},
                      {actual=all_except_option("this",[]),
                       expected=NONE,msg=""},
                      {actual=all_except_option("",words),
                       expected=NONE,msg=""}])
  end

fun test_substitutions() =
  let 
    val names_1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
    val names_2 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]
  in
    test("Substitutions",[{actual=get_substitutions1(names_1,"Fred"),
                           expected=["Fredrick","Freddie","F"],msg=""},
                          {actual=get_substitutions1(names_2,"Jeff"),
                           expected=["Jeffrey","Geoff","Jeffrey"],msg=""},
                          {actual=get_substitutions2(names_1,"Fred"),
                           expected=["Fredrick","Freddie","F"],msg=""},
                          {actual=get_substitutions2(names_2,"Jeff"),
                           expected=["Jeffrey","Geoff","Jeffrey"],msg=""}])
  end

fun test_similar_names() =
  let
    val names_1 = [["Fred","Fredrick"],
                   ["Elizabeth","Betty"],
                   ["Freddie","Fred","F"]]
    val names_2 = [["liz","lizzie","Elizabeth","Betty"],
                   ["Joe","Joseph"],
                   ["Bob","Rob","Robert","Bobby"]]
    val exp_1 = [{first="Fred", last="Smith", middle="W"},
                 {first="Fredrick", last="Smith", middle="W"},
                 {first="Freddie", last="Smith", middle="W"},
                 {first="F", last="Smith", middle="W"}] 
    val exp_2 = [{first="liz",last="John",middle="B"},
                 {first="lizzie",last="John",middle="B"},
                 {first="Elizabeth",last="John",middle="B"},
                 {first="Betty",last="John",middle="B"}];
    val exp_3 = [{first="Jack",last="John",middle="B"}] 
  in
    test("SimNames",[{actual=similar_names(names_1,{first="Fred", middle="W", last="Smith"}),
                      expected=exp_1,msg=""},
                     {actual=similar_names(names_2,{first="liz",middle="B",last="John"}),
                      expected=exp_2,msg=""},
                     {actual=similar_names(names_2,{first="Jack",middle="B",last="John"}),
                      expected=exp_3,msg=""}])
  end

fun test_card_color() = 
  let val card_a = (Hearts,Num(3))
  in
    test("CardColor",[{actual=card_color(Clubs,Jack), expected=Black,msg=""},
                     {actual=card_color(Diamonds,Num(8)), expected=Red,msg=""},
                     {actual=card_color(card_a), expected=Red,msg=""},
                     {actual=card_color(Spades,Ace), expected=Black,msg=""}])
  end

fun test_card_value() = 
  let val card_a = (Hearts,Num(3))
  in
    test("CardValue",[{actual=card_value(Clubs,Jack), expected=10,msg=""},
                     {actual=card_value(Clubs,Num(8)), expected=8,msg=""},
                     {actual=card_value(card_a), expected=3,msg=""},
                     {actual=card_value(Clubs,Ace), expected=11,msg=""}])
  end

fun test_remove_card() =
  let
    val crd_list = [(Clubs,Num(8)),(Hearts,Num(1)),(Clubs,Ace)];
  in
    test("RemCard",[{actual=remove_card(crd_list,(Hearts, Num 1),IllegalMove),
                     expected=[(Clubs,Num 8),(Clubs,Ace)],msg=""}])
  end

fun test_remove_card_exc() =
    test("RemCard",[{actual=(remove_card([], (Clubs, Ace), IllegalMove); false) handle
                     IllegalMove => true,
                     expected=true,msg=""}])

fun test_same_color() = 
  let val cardset_a = [(Clubs,Num(8)),(Spades,Num(1)),(Clubs,Ace)]
      val cardset_b = [(Clubs,Num(8)),(Spades,Num(1)),(Clubs,Ace),(Spades, Num 2)];
      val cardset_c = [(Clubs,Num 8 ),(Spades,Num 1 ),(Hearts,Ace),(Spades, Num 2)];
      val cardset_d = [(Clubs,Num(8))];
  in
    test("Same Color",[{actual=all_same_color(cardset_a), expected=true,msg=""},
                     {actual=all_same_color(cardset_b), expected=true,msg=""},
                     {actual=all_same_color(cardset_c), expected=false,msg=""},
                     {actual=all_same_color(cardset_d), expected=true,msg=""},
                     {actual=all_same_color([]), expected=true,msg=""}])
  end

fun test_sum_card() = 
  let val cardset_a = [(Clubs,Num(8)),(Spades,Num(1)),(Clubs,Ace)]
      val cardset_b = [(Clubs,Num(8)),(Spades,Num(1)),(Clubs,Ace),(Spades, Num 2)]
      val cardset_c = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades, Ace)]
  in
    test("Sum Card",[{actual=sum_cards(cardset_a), expected=20,msg=""},
                     {actual=sum_cards(cardset_b), expected=22,msg=""},
                     {actual=sum_cards(cardset_c), expected=44,msg=""}])
  end

fun test_score() =
    test("Score",
         [{actual=score([],11),expected=5,msg=""},
         {actual=score([(Spades,King)],12),expected=1,msg=""},
         {actual=score([(Spades,King)],11),expected=0,msg=""},
         {actual=score([(Spades,King)],10),expected=0,msg=""},
         {actual=score([(Spades,King),(Spades,Ace)],11),expected=15,msg=""},
         {actual=score([(Spades,King),(Spades,Ace)],21),expected=0,msg=""},
         {actual=score([(Spades,King),(Spades,Ace)],22),expected=0,msg=""},
         {actual=score([(Spades,King),(Spades,Ace),(Hearts,Ace)],22),expected=30,msg=""},
         {actual=score([(Spades,King),(Spades,Ace),(Hearts,Ace)],32),expected=0,msg=""},
         {actual=score([(Spades,King),(Spades,Ace),(Hearts,Ace)],42),expected=10,msg=""}])

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end

fun test_officiate_exc() =
    test("officiateExc",[{actual=(provided_test1();false) handle IllegalMove => true,
                     expected=true,msg=""}])

fun test_officiate() =
    let val a = (Hearts, Ace)
        val b = (Hearts, Num 2)
        val c = (Spades, Ace)
    in
    test("officiate",[{actual=provided_test2(), expected=3,msg=""},
                      {actual=officiate([],[], 10),expected=5,msg=""},
                      {actual=officiate([],[Draw],10),expected=5,msg=""},
                      {actual=officiate([a], [Draw,Discard a], 12), expected=6,msg=""},
                      {actual=officiate([a,b],[Draw],11),expected=0,msg=""},
                      {actual=officiate([a,b],[Draw],10),expected=1,msg=""},
                      {actual=officiate([a,c],[Draw],10),expected=1,msg=""},
                      {actual=officiate([a,c],[Draw,Draw],10),expected=1,msg=""},
                      {actual=officiate([a,c],[Draw,Draw],12),expected=30,msg=""},
                      {actual=officiate([a,b],[Draw,Draw],10),expected=1,msg=""},
                      {actual=officiate([a,b,c],[Draw, Draw],12),expected=1,msg=""},
                      {actual=officiate([c,a,b],[Draw, Draw],23),expected=1,msg=""}])
    end

