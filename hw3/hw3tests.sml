use "hw3/hw3provided.sml";

fun test(test_name,tests) =
    let fun run_test(tests,passed_results,failed_results) =
        case tests of
             [] => "Results for " ^ test_name ^ ".Passed:" ^ Int.toString(passed_results)
                    ^ " Failed:" ^ Int.toString(failed_results)
           | (({actual,expected})::rest) => if actual = expected
                                            then run_test(rest,
                                                passed_results+1,
                                                failed_results)
                                            else run_test(rest,
                                                passed_results,
                                                failed_results+1)
    in
      run_test(tests,0,0)
    end

fun test_only_capitals() =
  let
    val words = ["This","the","A","Hello","World","not"]
  in
    test("only_caps",[{actual=only_capitals(words),
                       expected=["This","A","Hello","World"]}])
  end

val lwords = ["This","the","A","Hello","World","not","long string","loooong string"]
val swords = ["the","not","cat","dog"] (* check first/last etc *)

fun test_longest_string1() =
    test("ls1",[{actual=longest_string1(lwords),
                 expected="loooong string"},
                {actual=longest_string1(swords),
                 expected="the"}])

fun test_longest_string2() =
    test("ls1",[{actual=longest_string2(lwords),
                 expected="loooong string"},
                {actual=longest_string2(swords),
                 expected="dog"}])

fun test_longest_string3() =
    test("ls1",[{actual=longest_string3 lwords,
                 expected="loooong string"},
                {actual=longest_string3 swords,
                 expected="the"}])

fun test_longest_string4() =
    test("ls1",[{actual=longest_string4 lwords,
                 expected="loooong string"},
                {actual=longest_string4 swords,
                 expected="dog"}])

fun test_longest_cap() =
  let
    val words = ["This","the","A","Hello","World","not","long string","loooong string"]
  in
    test("longestCap",[{actual=longest_capitalized words,
                       expected="Hello"},
                       {actual=longest_capitalized
                       ["then","The","To","testing"],
                       expected="The"},
                       {actual=longest_capitalized
                       ["then","the","to","testing"],
                       expected=""}])
    end
fun test_rev_string() =
    test("revString", [{actual=rev_string "Hello World!",
                       expected="!dlroW olleH"},
                       {actual=rev_string "",
                        expected=""}])

val states = ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","NorthCarolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","southCarolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"];

fun test_first_answer() =
    test("first_answer",
            [{actual=first_answer (fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,4,3],
              expected=4}])

fun test_first_answer_exc() =
    test("first_ans_exc",[{actual=(first_answer 
                (fn x => if (x mod 2) = 0 then SOME x else NONE) [1,1,5,3]; false) 
                handle NoAnswer => true,
                expected=true}])

fun test_all_answers() =
    test("all_answers",
    [{actual=all_answers (fn x => if (x < 0) then NONE else SOME ([1,2,3])) [1,2,3], 
      expected=SOME [1,2,3,1,2,3,1,2,3]},
    {actual=all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [1,2,3],
      expected=SOME [1,1,1]},
    {actual=all_answers (fn x => if (x < 0) then NONE else SOME ([x])) [1,2,3],
      expected=SOME [3,2,1]},
    {actual=all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [],
      expected=SOME []}])

fun test_count_wildcards () =
    test("count_wildcards",
    [{actual=count_wildcards(Wildcard),expected=1},
     {actual=count_wildcards(UnitP),expected=0},
     {actual=count_wildcards(ConstP 1),expected=0},
     {actual=count_wildcards(TupleP []),expected= 0},
     {actual=count_wildcards(TupleP [Wildcard]),expected=1},
     {actual=count_wildcards(TupleP [Wildcard, UnitP, Variable "x"]),expected=1},
     {actual=count_wildcards(TupleP [Wildcard, Wildcard, Variable "xy"]),expected=2},
     {actual=count_wildcards(TupleP [TupleP [Wildcard, Wildcard]]),expected=2},
     {actual=count_wildcards(TupleP [ConstructorP ("a", Wildcard), TupleP [Wildcard]]),
      expected=2}])

fun test_count_some_var() =
  test("count_some_var",
       [{actual=count_some_var("test",
              TupleP [TupleP [Wildcard, UnitP, Variable "test",Variable "test"]]),
         expected=2},
       {actual=count_some_var("test",TupleP [TupleP [Wildcard, UnitP,
                              Variable "te"]]),
         expected=0},
       {actual=count_some_var ("test",
                  TupleP [TupleP [Wildcard, UnitP, Variable "test"]]),
         expected=1}])

fun test_check_pat() =
    test("check_pat",
         [{actual=check_pat (TupleP [Wildcard,Variable "cat",
                            Variable "pp",TupleP[Variable "tt"],
                            Wildcard,ConstP 3,
                            ConstructorP("cony",Variable "pp")]),
           expected=false},
         {actual=check_pat (TupleP [Wildcard,Variable "cat",
                            Variable "pp",TupleP[Variable "tt"],
                            Wildcard,ConstP 3,
                            ConstructorP("cony",Variable "test")]),
           expected=true}])

fun test_match() =
    test("test_match",
         [{actual=match(Unit, UnitP),
           expected=SOME []},
          {actual=match(Unit, Variable "cat"),
           expected=SOME [("cat", Unit)]},
          {actual=match(Unit, ConstP 3),
           expected=NONE},
          {actual=match(Tuple [], TupleP [Wildcard]),
           expected=NONE},
          {actual=match(Tuple [Unit], TupleP []),
           expected=NONE},
          {actual=match(Tuple [Unit], Variable "cat"),
           expected=SOME [("cat", Tuple [Unit])]},
          {actual=match(Tuple [Unit], TupleP [Variable "cat"]),
           expected=SOME [("cat", Unit)]},
          {actual=match(Tuple [Unit, Const 8], TupleP [Variable "cat", ConstP 3]),
           expected=NONE},
          {actual=match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]),
           expected=SOME [("dog", Const 8), ("cat", Unit)]},
          {actual=match(Tuple [Unit, Tuple [Unit, Unit]], 
                        TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]),
           expected=SOME [("rat", Unit), ("dog", Unit), ("cat", Unit)]},
          {actual=match(Constructor ("mat", Unit), ConstructorP ("hat", Variable "cat")),
           expected=NONE},
          {actual=match(Constructor ("dog", Unit), ConstructorP ("dog", Variable "cat")),
           expected=SOME [("cat", Unit)]},
          {actual=match(Tuple[Const 17, Unit, Const 7, 
                        Constructor ("zoe", Const 7), 
                        Constructor ("zoe", (Constructor ("zoe", Const 7)))], 
                        TupleP[Wildcard,Wildcard]),
           expected=NONE},
          {actual=match(Const 7, Wildcard ),
           expected=SOME []},
          {actual=match(Unit, Wildcard ),
           expected=SOME []},
          {actual=match(Tuple[Const 7], Wildcard ),
           expected=SOME []},
          {actual=match(Constructor("cat", Const 7), Wildcard ),
           expected=SOME []},
          {actual=match(Const 7, Variable "Zoe" ),
           expected=SOME [("Zoe", Const 7)]},
          {actual=match(Unit, Variable "chopsticks" ),
           expected=SOME [("chopsticks", Unit)]},
          {actual=match(Unit, UnitP ),
           expected=SOME []},
          {actual=match(Const 7, UnitP ),
           expected=NONE},
          {actual=match(Const 7, ConstP 7 ),
           expected=SOME []},
          {actual=match(Const 7, ConstP 8 ),
           expected=NONE},
          {actual=match(Constructor("Cat", Const 7), ConstructorP("Cat", Wildcard)),
           expected= SOME[]},
          {actual=match(Constructor("Dog", Const 7), ConstructorP("Cat", Wildcard)),
           expected= NONE},
          {actual=match(Constructor("Cat", Const 7), ConstructorP("Cat", UnitP)),
           expected= NONE},
          {actual=match(Constructor("Cat", Const 7), ConstructorP("Cat", Variable "dog")) ,
           expected= SOME [("dog",Const 7)]},
          {actual=match(Tuple[Const 7], TupleP[ConstP 7]),
           expected= SOME []},
          {actual=match(Tuple[Const 7], TupleP[ConstP 7,ConstP 7]),
           expected= NONE},
          {actual=match(Tuple[Const 7, Const 6, Unit, Const 8],
                        TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]),
           expected=SOME [("cat",Const 6)]},
          {actual=match(Tuple[Const 7, Const 6, Unit, Const 7],
                        TupleP[Variable "a", Variable "ab",
                        Variable "abc", Variable "abcd"]),
           expected=SOME [("abcd",Const 7),("abc",Unit),("ab",Const 6),("a",Const 7)]}
         ])

fun test_first_match() =
    test("test_first_match",
         [{actual=first_match Unit [UnitP],
           expected=SOME []},
          {actual=first_match Unit [Variable "cat"],
           expected=SOME [("cat", Unit)]},
          {actual=first_match Unit [ConstP 3],
           expected=NONE},
          {actual=first_match (Tuple []) [TupleP [Wildcard]],
           expected=NONE},
          {actual=first_match (Tuple [Unit]) [TupleP []],
           expected=NONE},
          {actual=first_match (Tuple [Unit]) [TupleP [Variable "cat"]],
           expected=SOME [("cat", Unit)]},
          {actual=first_match (Tuple [Unit, Const 8]) [TupleP [Variable "cat", Variable "dog"]],
           expected=SOME [("dog", Const 8), ("cat", Unit)]},
          {actual=first_match (Constructor ("mat", Unit)) [ConstructorP ("hat",Variable "cat")],
           expected=NONE},
          {actual=first_match (Constructor ("dog", Unit)) [ConstructorP("dog",Variable "cat")],
           expected=SOME [("cat", Unit)]},
          {actual=first_match (Const 7) [Wildcard ],
           expected=SOME []},
          {actual=first_match Unit [Wildcard ],
           expected=SOME []},
          {actual=first_match (Tuple[Const 7]) [Wildcard ],
           expected=SOME []},
          {actual=first_match Unit [Variable "chopsticks" ],
           expected=SOME [("chopsticks", Unit)]},
          {actual=first_match Unit [UnitP ],
           expected=SOME []},
          {actual=first_match (Const 7) [UnitP ],
           expected=NONE},
          {actual=first_match (Const 7) [ConstP 7 ],
           expected=SOME []},
          {actual=first_match (Const 7) [ConstP 8 ],
           expected=NONE},
          {actual=first_match (Constructor("Cat", Const 7)) [ConstructorP("Cat",Variable "dog")] ,
           expected= SOME [("dog",Const 7)]},
          {actual=first_match (Tuple[Const 7]) [TupleP[ConstP 7]],
           expected= SOME []},
          {actual=first_match (Tuple[Const 7]) [TupleP[ConstP 7,ConstP 7]],
           expected= NONE},
          {actual=first_match (Tuple[Const 7, Const 6, Unit, Const 8])
                        [TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]],
           expected=SOME [("cat",Const 6)]},
          {actual=first_match (Tuple[Const 7, Const 6, Unit, Const 7])
                        [TupleP[Variable "a", Variable "ab",
                        Variable "abc", Variable "abcd"]],
           expected=SOME [("abcd",Const 7),("abc",Unit),("ab",Const 6),("a",Const 7)]}
         ])
