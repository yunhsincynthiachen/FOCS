findTransitions (dfaThreeA,"start",'a') = [("start", 'a', "one")];;
findTransitions (dfaThreeA,"start",'b') = [("start", 'b', "start")];;
findTransitions (dfaThreeA,"one",'b') = [("one", 'b', "one")];;
findTransitions (nfaLastThreeB,0,'a') = [(0, 'a', 0)];;
findTransitions (nfaLastThreeB,0,'b') = [(0, 'b', 0); (0, 'b', 1)];;


isAccepting (dfaThreeA,"start");;
isAccepting (dfaThreeA,"one") = false;;
isAccepting (dfaThreeA,"two") = false;;
isAccepting (nfaLastThreeB,3);;
isAccepting (nfaLastThreeB,0) = false;;


step (dfaThreeA, "start",'a') = "one";;
step (dfaThreeA, "start",'b') = "start";;
step (dfaThreeA, "one",'a') = "two";;
step (dfaThreeA, "one",'b') = "one";;
step (dfaThreeA, "two",'a') = "start";;
step (dfaThreeA, "two",'b') = "two";;

steps (dfaThreeA, "start", []) = "start";;
steps (dfaThreeA, "start", ['a']) = "one";;
steps (dfaThreeA, "start", ['a';'b']) = "one";;
steps (dfaThreeA, "start", ['a';'b';'a']) = "two";;
steps (dfaThreeA, "one", []) = "one";;
steps (dfaThreeA, "one", ['a']) = "two";;
steps (dfaThreeA, "one", ['a';'b']) = "two";;
steps (dfaThreeA, "one", ['a';'b';'a']) = "start";;


acceptDFA (dfaThreeA,"") = true;;
acceptDFA (dfaThreeA,"a") = false;;
acceptDFA (dfaThreeA,"b") = true;;
acceptDFA (dfaThreeA,"aa") = false;;
acceptDFA (dfaThreeA,"aaa") = true;;
acceptDFA (dfaThreeA,"ababa") = true;;
acceptDFA (dfaThreeA,"abababa") = false;;

acceptDFA (dfaThreeA,"") = true;;
acceptDFA (dfaThreeA,"a") = false;;
acceptDFA (dfaThreeA,"b") = true;;
acceptDFA (dfaThreeA,"aa") = false;;
acceptDFA (dfaThreeA,"aaa") = true;;
acceptDFA (dfaThreeA,"ababa") = true;;
acceptDFA (dfaThreeA,"abababa") = false;;

keepTarget [] = [];;
keepTarget [(1,'a',2);(1,'b',3)]= [2; 3];;
keepTarget [(1,'a',2);(1,'b',3);(2,'a',2)] = [3; 2];;
keepTarget (dfaThreeA.delta) = ["start"; "one"; "two"];;
keepTarget (nfaLastThreeB.delta) = [0; 1; 2; 3];;

isAcceptingAny (nfaLastThreeB, []) = false;;
isAcceptingAny (nfaLastThreeB, [0]) = false;;
isAcceptingAny (nfaLastThreeB, [0;1]) = false;;
isAcceptingAny (nfaLastThreeB, [0;1;2]) = false;;
isAcceptingAny (nfaLastThreeB, [0;1;2;3]) = true;;
isAcceptingAny (nfaLastThreeB, [3]) = true;;

stepAll (dfaThreeA,[],'a') = [];;
stepAll (dfaThreeA,["start"],'a') = ["one"];;
stepAll (dfaThreeA,["start"],'b') = ["start"];;
stepAll (dfaThreeA,["start";"one"],'a') = ["one"; "two"];;
stepAll (dfaThreeA,["start";"one"],'b') = ["start"; "one"];;
stepAll (nfaLastThreeB,[0;1],'a') = [0];;
stepAll (nfaLastThreeB,[0;1],'b') = [0; 1; 2];;

stepsAll (dfaThreeA,[],[]) = [];;
stepsAll (dfaThreeA,[],['a']) = [];;
stepsAll (dfaThreeA,[],['a';'b']) = [];;
stepsAll (dfaThreeA,["start"],[]) = ["start"];;
stepsAll (dfaThreeA,["start"],['a']) = ["one"];;
stepsAll (dfaThreeA,["start"],['a';'b']) = ["one"];;
stepsAll (dfaThreeA,["start"],['a';'a']) = ["two"];;
stepsAll (dfaThreeA,["start";"one"],['a';'a']) = ["two"; "start"];;
stepsAll (dfaThreeA,["start";"one"],['a';'a';'b']) = ["two"; "start"];;
stepsAll (dfaThreeA,["start";"one"],['a';'a';'b';'a']) = ["start"; "one"];;
stepsAll (nfaLastThreeB,[0;1],['a';'b';'b';'b']) = [0; 1; 2; 3];;

acceptNFA (dfaThreeA,"babab") = false;;
acceptNFA (dfaThreeA,"bababa") = true;;
acceptNFA (dfaThreeA,"bababab") = true;;
acceptNFA (nfaLastThreeB,"abb") = false;;
acceptNFA (nfaLastThreeB,"abbb") = true;;

["a";"b";"aa";"ba";"ab";"bb";"aaa";"baa";"aba";"bba";"aab";"bab";"abb";"aaaa";"baaa";"abaa";"bbaa";"aaba";"baba";"abba";"aaab";"baab";"abab";"bbab";"aabb";"babb";"aaaaa";"baaaa";"abaaa";"bbaaa";"aabaa";"babaa";"abbaa";"aaaba";"baaba";"ababa";"bbaba";"aabba";"babba";"aaaab";"baaab";"abaab";"bbaab";"aabab";"babab";"abbab";"aaabb";"baabb";"ababb";"bbabb";"aaaaaa";"baaaaa";"abaaaa";"bbaaaa";"aabaaa";"babaaa";"abbaaa";"aaabaa";"baabaa";"ababaa";"bbabaa";"aabbaa";"babbaa";"aaaaba";"baaaba";"abaaba";"bbaaba";"aababa";"bababa";"abbaba";"aaabba";"baabba";"ababba";"bbabba";"aaaaab";"baaaab";"abaaab";"bbaaab";"aabaab";"babaab";"abbaab";"aaabab";"baabab";"ababab";"bbabab";"aabbab";"babbab";"aaaabb";"baaabb";"abaabb";"bbaabb";"aababb";"bababb";"abbabb"] =  ["a";"b";"aa";"ba";"ab";"bb";"aaa";"baa";"aba";"bba";"aab";"bab";"abb";"aaaa";"baaa";"abaa";"bbaa";"aaba";"baba";"abba";"aaab";"baab";"abab";"bbab";"aabb";"babb";"aaaaa";"baaaa";"abaaa";"bbaaa";"aabaa";"babaa";"abbaa";"aaaba";"baaba";"ababa";"bbaba";"aabba";"babba";"aaaab";"baaab";"abaab";"bbaab";"aabab";"babab";"abbab";"aaabb";"baabb";"ababb";"bbabb";"aaaaaa";"baaaaa";"abaaaa";"bbaaaa";"aabaaa";"babaaa";"abbaaa";"aaabaa";"baabaa";"ababaa";"bbabaa";"aabbaa";"babbaa";"aaaaba";"baaaba";"abaaba";"bbaaba";"aababa";"bababa";"abbaba";"aaabba";"baabba";"ababba";"bbabba";"aaaaab";"baaaab";"abaaab";"bbaaab";"aabaab";"babaab";"abbaab";"aaabab";"baabab";"ababab";"bbabab";"aabbab";"babbab";"aaaabb";"baaabb";"abaabb";"bbaabb";"aababb";"bababb";"abbabb"];;

["a";"b";"aa";"ba";"ab";"bb";"aaa";"baa";"aba";"bba";"aab";"bab";"abb";"bbb";"baaa";"abaa";"bbaa";"aaba";"baba";"abba";"bbba";"aaab";"baab";"abab";"bbab";"aabb";"babb";"abbb";"abaaa";"bbaaa";"aabaa";"babaa";"abbaa";"bbbaa";"aaaba";"baaba";"ababa";"bbaba";"aabba";"babba";"abbba";"baaab";"abaab";"bbaab";"aabab";"babab";"abbab";"bbbab";"aaabb";"baabb";"ababb";"bbabb";"aabbb";"babbb";"aabaaa";"babaaa";"abbaaa";"bbbaaa";"aaabaa";"baabaa";"ababaa";"bbabaa";"aabbaa";"babbaa";"abbbaa";"baaaba";"abaaba";"bbaaba";"aababa";"bababa";"abbaba";"bbbaba";"aaabba";"baabba";"ababba";"bbabba";"aabbba";"babbba";"abaaab";"bbaaab";"aabaab";"babaab";"abbaab";"bbbaab";"aaabab";"baabab";"ababab";"bbabab";"aabbab";"babbab";"abbbab";"baaabb";"abaabb";"bbaabb";"aababb";"bababb";"abbabb";"bbbabb";"aaabbb";"baabbb";"ababbb";"bbabbb";"aaabaaa";"baabaaa";"ababaaa";"bbabaaa";"aabbaaa";"babbaaa";"abbbaaa";"baaabaa";"abaabaa";"bbaabaa";"aababaa";"bababaa";"abbabaa";"bbbabaa";"aaabbaa";"baabbaa";"ababbaa";"bbabbaa";"aabbbaa";"babbbaa";"abaaaba";"bbaaaba";"aabaaba";"babaaba";"abbaaba";"bbbaaba";"aaababa";"baababa";"abababa";"bbababa";"aabbaba";"babbaba";"abbbaba";"baaabba";"abaabba";"bbaabba";"aababba";"bababba";"abbabba";"bbbabba";"aaabbba";"baabbba";"ababbba";"bbabbba";"aabaaab";"babaaab";"abbaaab";"bbbaaab";"aaabaab";"baabaab";"ababaab";"bbabaab";"aabbaab";"babbaab";"abbbaab";"baaabab";"abaabab";"bbaabab";"aababab";"bababab";"abbabab";"bbbabab";"aaabbab";"baabbab";"ababbab";"bbabbab";"aabbbab";"babbbab";"abaaabb";"bbaaabb";"aabaabb";"babaabb";"abbaabb";"bbbaabb";"aaababb";"baababb";"abababb";"bbababb";"aabbabb";"babbabb";"abbbabb";"baaabbb";"abaabbb";"bbaabbb";"aababbb";"bababbb";"abbabbb";"bbbabbb"] = ["a";"b";"aa";"ba";"ab";"bb";"aaa";"baa";"aba";"bba";"aab";"bab";"abb";"bbb";"baaa";"abaa";"bbaa";"aaba";"baba";"abba";"bbba";"aaab";"baab";"abab";"bbab";"aabb";"babb";"abbb";"abaaa";"bbaaa";"aabaa";"babaa";"abbaa";"bbbaa";"aaaba";"baaba";"ababa";"bbaba";"aabba";"babba";"abbba";"baaab";"abaab";"bbaab";"aabab";"babab";"abbab";"bbbab";"aaabb";"baabb";"ababb";"bbabb";"aabbb";"babbb";"aabaaa";"babaaa";"abbaaa";"bbbaaa";"aaabaa";"baabaa";"ababaa";"bbabaa";"aabbaa";"babbaa";"abbbaa";"baaaba";"abaaba";"bbaaba";"aababa";"bababa";"abbaba";"bbbaba";"aaabba";"baabba";"ababba";"bbabba";"aabbba";"babbba";"abaaab";"bbaaab";"aabaab";"babaab";"abbaab";"bbbaab";"aaabab";"baabab";"ababab";"bbabab";"aabbab";"babbab";"abbbab";"baaabb";"abaabb";"bbaabb";"aababb";"bababb";"abbabb";"bbbabb";"aaabbb";"baabbb";"ababbb";"bbabbb";"aaabaaa";"baabaaa";"ababaaa";"bbabaaa";"aabbaaa";"babbaaa";"abbbaaa";"baaabaa";"abaabaa";"bbaabaa";"aababaa";"bababaa";"abbabaa";"bbbabaa";"aaabbaa";"baabbaa";"ababbaa";"bbabbaa";"aabbbaa";"babbbaa";"abaaaba";"bbaaaba";"aabaaba";"babaaba";"abbaaba";"bbbaaba";"aaababa";"baababa";"abababa";"bbababa";"aabbaba";"babbaba";"abbbaba";"baaabba";"abaabba";"bbaabba";"aababba";"bababba";"abbabba";"bbbabba";"aaabbba";"baabbba";"ababbba";"bbabbba";"aabaaab";"babaaab";"abbaaab";"bbbaaab";"aaabaab";"baabaab";"ababaab";"bbabaab";"aabbaab";"babbaab";"abbbaab";"baaabab";"abaabab";"bbaabab";"aababab";"bababab";"abbabab";"bbbabab";"aaabbab";"baabbab";"ababbab";"bbabbab";"aabbbab";"babbbab";"abaaabb";"bbaaabb";"aabaabb";"babaabb";"abbaabb";"bbbaabb";"aaababb";"baababb";"abababb";"bbababb";"aabbabb";"babbabb";"abbbabb";"baaabbb";"abaabbb";"bbaabbb";"aababbb";"bababbb";"abbabbb";"bbbabbb"];;

["aaa";"bbb";"aaaa";"bbbb";"aaaaa";"bbbbb";"aaaaaa";"bbbaaa";"aaabbb";"bbbbbb";"aaaaaaa";"bbbaaaa";"bbbbaaa";"aaaabbb";"aaabbbb";"bbbbbbb";"aaaaaaaa";"bbbaaaaa";"bbbbaaaa";"bbbbbaaa";"aaaaabbb";"aaaabbbb";"aaabbbbb";"bbbbbbbb";"aaaaaaaaa";"bbbaaaaaa";"bbbbaaaaa";"bbbbbaaaa";"aaabbbaaa";"bbbbbbaaa";"aaaaaabbb";"bbbaaabbb";"aaaaabbbb";"aaaabbbbb";"aaabbbbbb";"bbbbbbbbb";"aaaaaaaaaa";"bbbaaaaaaa";"bbbbaaaaaa";"bbbbbaaaaa";"aaabbbaaaa";"bbbbbbaaaa";"aaaabbbaaa";"aaabbbbaaa";"bbbbbbbaaa";"aaaaaaabbb";"bbbaaaabbb";"bbbbaaabbb";"aaaaaabbbb";"bbbaaabbbb";"aaaaabbbbb";"aaaabbbbbb";"aaabbbbbbb";"bbbbbbbbbb";"aaaaaaaaaaa";"bbbaaaaaaaa";"bbbbaaaaaaa";"bbbbbaaaaaa";"aaabbbaaaaa";"bbbbbbaaaaa";"aaaabbbaaaa";"aaabbbbaaaa";"bbbbbbbaaaa";"aaaaabbbaaa";"aaaabbbbaaa";"aaabbbbbaaa";"bbbbbbbbaaa";"aaaaaaaabbb";"bbbaaaaabbb";"bbbbaaaabbb";"bbbbbaaabbb";"aaaaaaabbbb";"bbbaaaabbbb";"bbbbaaabbbb";"aaaaaabbbbb";"bbbaaabbbbb";"aaaaabbbbbb";"aaaabbbbbbb";"aaabbbbbbbb";"bbbbbbbbbbb";"aaaaaaaaaaaa";"bbbaaaaaaaaa";"bbbbaaaaaaaa";"bbbbbaaaaaaa";"aaabbbaaaaaa";"bbbbbbaaaaaa";"aaaabbbaaaaa";"aaabbbbaaaaa";"bbbbbbbaaaaa";"aaaaabbbaaaa";"aaaabbbbaaaa";"aaabbbbbaaaa";"bbbbbbbbaaaa";"aaaaaabbbaaa";"bbbaaabbbaaa";"aaaaabbbbaaa";"aaaabbbbbaaa";"aaabbbbbbaaa";"bbbbbbbbbaaa";"aaaaaaaaabbb";"bbbaaaaaabbb";"bbbbaaaaabbb";"bbbbbaaaabbb";"aaabbbaaabbb";"bbbbbbaaabbb";"aaaaaaaabbbb";"bbbaaaaabbbb";"bbbbaaaabbbb";"bbbbbaaabbbb";"aaaaaaabbbbb";"bbbaaaabbbbb";"bbbbaaabbbbb";"aaaaaabbbbbb";"bbbaaabbbbbb";"aaaaabbbbbbb";"aaaabbbbbbbb";"aaabbbbbbbbb";"bbbbbbbbbbbb"] = ["aaa";"bbb";"aaaa";"bbbb";"aaaaa";"bbbbb";"aaaaaa";"bbbaaa";"aaabbb";"bbbbbb";"aaaaaaa";"bbbaaaa";"bbbbaaa";"aaaabbb";"aaabbbb";"bbbbbbb";"aaaaaaaa";"bbbaaaaa";"bbbbaaaa";"bbbbbaaa";"aaaaabbb";"aaaabbbb";"aaabbbbb";"bbbbbbbb";"aaaaaaaaa";"bbbaaaaaa";"bbbbaaaaa";"bbbbbaaaa";"aaabbbaaa";"bbbbbbaaa";"aaaaaabbb";"bbbaaabbb";"aaaaabbbb";"aaaabbbbb";"aaabbbbbb";"bbbbbbbbb";"aaaaaaaaaa";"bbbaaaaaaa";"bbbbaaaaaa";"bbbbbaaaaa";"aaabbbaaaa";"bbbbbbaaaa";"aaaabbbaaa";"aaabbbbaaa";"bbbbbbbaaa";"aaaaaaabbb";"bbbaaaabbb";"bbbbaaabbb";"aaaaaabbbb";"bbbaaabbbb";"aaaaabbbbb";"aaaabbbbbb";"aaabbbbbbb";"bbbbbbbbbb";"aaaaaaaaaaa";"bbbaaaaaaaa";"bbbbaaaaaaa";"bbbbbaaaaaa";"aaabbbaaaaa";"bbbbbbaaaaa";"aaaabbbaaaa";"aaabbbbaaaa";"bbbbbbbaaaa";"aaaaabbbaaa";"aaaabbbbaaa";"aaabbbbbaaa";"bbbbbbbbaaa";"aaaaaaaabbb";"bbbaaaaabbb";"bbbbaaaabbb";"bbbbbaaabbb";"aaaaaaabbbb";"bbbaaaabbbb";"bbbbaaabbbb";"aaaaaabbbbb";"bbbaaabbbbb";"aaaaabbbbbb";"aaaabbbbbbb";"aaabbbbbbbb";"bbbbbbbbbbb";"aaaaaaaaaaaa";"bbbaaaaaaaaa";"bbbbaaaaaaaa";"bbbbbaaaaaaa";"aaabbbaaaaaa";"bbbbbbaaaaaa";"aaaabbbaaaaa";"aaabbbbaaaaa";"bbbbbbbaaaaa";"aaaaabbbaaaa";"aaaabbbbaaaa";"aaabbbbbaaaa";"bbbbbbbbaaaa";"aaaaaabbbaaa";"bbbaaabbbaaa";"aaaaabbbbaaa";"aaaabbbbbaaa";"aaabbbbbbaaa";"bbbbbbbbbaaa";"aaaaaaaaabbb";"bbbaaaaaabbb";"bbbbaaaaabbb";"bbbbbaaaabbb";"aaabbbaaabbb";"bbbbbbaaabbb";"aaaaaaaabbbb";"bbbaaaaabbbb";"bbbbaaaabbbb";"bbbbbaaabbbb";"aaaaaaabbbbb";"bbbaaaabbbbb";"bbbbaaabbbbb";"aaaaaabbbbbb";"bbbaaabbbbbb";"aaaaabbbbbbb";"aaaabbbbbbbb";"aaabbbbbbbbb";"bbbbbbbbbbbb"];;

["bbb";"abbb";"bbbb";"cbbb";"aabbb";"babbb";"cabbb";"abbbb";"bbbbb";"cbbbb";"acbbb";"bcbbb";"ccbbb";"aaabbb";"baabbb";"caabbb";"ababbb";"bbabbb";"cbabbb";"acabbb";"bcabbb";"ccabbb";"aabbbb";"babbbb";"cabbbb";"abbbbb";"bbbbbb";"cbbbbb";"acbbbb";"bcbbbb";"ccbbbb";"aacbbb";"bacbbb";"cacbbb";"abcbbb";"bbcbbb";"cbcbbb";"accbbb";"bccbbb";"cccbbb";"aaaabbb";"baaabbb";"caaabbb";"abaabbb";"bbaabbb";"cbaabbb";"acaabbb";"bcaabbb";"ccaabbb";"aababbb";"bababbb";"cababbb";"abbabbb";"bbbabbb";"cbbabbb";"acbabbb";"bcbabbb";"ccbabbb";"aacabbb";"bacabbb";"cacabbb";"abcabbb";"bbcabbb";"cbcabbb";"accabbb";"bccabbb";"cccabbb";"aaabbbb";"baabbbb";"caabbbb";"ababbbb";"bbabbbb";"cbabbbb";"acabbbb";"bcabbbb";"ccabbbb";"aabbbbb";"babbbbb";"cabbbbb";"abbbbbb";"bbbbbbb";"cbbbbbb";"acbbbbb";"bcbbbbb";"ccbbbbb";"aacbbbb";"bacbbbb";"cacbbbb";"abcbbbb";"bbcbbbb";"cbcbbbb";"accbbbb";"bccbbbb";"cccbbbb";"aaacbbb";"baacbbb";"caacbbb";"abacbbb";"bbacbbb";"cbacbbb";"acacbbb";"bcacbbb";"ccacbbb";"aabcbbb";"babcbbb";"cabcbbb";"abbcbbb";"bbbcbbb";"cbbcbbb";"acbcbbb";"bcbcbbb";"ccbcbbb";"aaccbbb";"baccbbb";"caccbbb";"abccbbb";"bbccbbb";"cbccbbb";"acccbbb";"bcccbbb";"ccccbbb"] = ["bbb";"abbb";"bbbb";"cbbb";"aabbb";"babbb";"cabbb";"abbbb";"bbbbb";"cbbbb";"acbbb";"bcbbb";"ccbbb";"aaabbb";"baabbb";"caabbb";"ababbb";"bbabbb";"cbabbb";"acabbb";"bcabbb";"ccabbb";"aabbbb";"babbbb";"cabbbb";"abbbbb";"bbbbbb";"cbbbbb";"acbbbb";"bcbbbb";"ccbbbb";"aacbbb";"bacbbb";"cacbbb";"abcbbb";"bbcbbb";"cbcbbb";"accbbb";"bccbbb";"cccbbb";"aaaabbb";"baaabbb";"caaabbb";"abaabbb";"bbaabbb";"cbaabbb";"acaabbb";"bcaabbb";"ccaabbb";"aababbb";"bababbb";"cababbb";"abbabbb";"bbbabbb";"cbbabbb";"acbabbb";"bcbabbb";"ccbabbb";"aacabbb";"bacabbb";"cacabbb";"abcabbb";"bbcabbb";"cbcabbb";"accabbb";"bccabbb";"cccabbb";"aaabbbb";"baabbbb";"caabbbb";"ababbbb";"bbabbbb";"cbabbbb";"acabbbb";"bcabbbb";"ccabbbb";"aabbbbb";"babbbbb";"cabbbbb";"abbbbbb";"bbbbbbb";"cbbbbbb";"acbbbbb";"bcbbbbb";"ccbbbbb";"aacbbbb";"bacbbbb";"cacbbbb";"abcbbbb";"bbcbbbb";"cbcbbbb";"accbbbb";"bccbbbb";"cccbbbb";"aaacbbb";"baacbbb";"caacbbb";"abacbbb";"bbacbbb";"cbacbbb";"acacbbb";"bcacbbb";"ccacbbb";"aabcbbb";"babcbbb";"cabcbbb";"abbcbbb";"bbbcbbb";"cbbcbbb";"acbcbbb";"bcbcbbb";"ccbcbbb";"aaccbbb";"baccbbb";"caccbbb";"abccbbb";"bbccbbb";"cbccbbb";"acccbbb";"bcccbbb";"ccccbbb"];;

["a";"b";"aa";"ba";"ab";"bb";"aaa";"baa";"aba";"bba";"aab";"bab";"abb";"bbb";"aaaa";"baaa";"abaa";"bbaa";"aaba";"baba";"abba";"bbba";"baab";"abab";"bbab";"aabb";"babb";"abbb";"bbbb";"aaaaa";"baaaa";"abaaa";"bbaaa";"aabaa";"babaa";"abbaa";"bbbaa";"aaaba";"baaba";"ababa";"bbaba";"aabba";"babba";"abbba";"bbbba";"baaab";"abaab";"bbaab";"aabab";"babab";"abbab";"bbbab";"baabb";"ababb";"bbabb";"aabbb";"babbb";"abbbb";"bbbbb";"aaaaaa";"baaaaa";"abaaaa";"bbaaaa";"aabaaa";"babaaa";"abbaaa";"bbbaaa";"aaabaa";"baabaa";"ababaa";"bbabaa";"aabbaa";"babbaa";"abbbaa";"bbbbaa";"aaaaba";"baaaba";"abaaba";"bbaaba";"aababa";"bababa";"abbaba";"bbbaba";"aaabba";"baabba";"ababba";"bbabba";"aabbba";"babbba";"abbbba";"bbbbba";"baaaab";"abaaab";"bbaaab";"aabaab";"babaab";"abbaab";"bbbaab";"baabab";"ababab";"bbabab";"aabbab";"babbab";"abbbab";"bbbbab";"baaabb";"abaabb";"bbaabb";"aababb";"bababb";"abbabb";"bbbabb";"baabbb";"ababbb";"bbabbb";"aabbbb";"babbbb";"abbbbb";"bbbbbb";"aaaaaaa";"baaaaaa";"abaaaaa";"bbaaaaa";"aabaaaa";"babaaaa";"abbaaaa";"bbbaaaa";"aaabaaa";"baabaaa";"ababaaa";"bbabaaa";"aabbaaa";"babbaaa";"abbbaaa";"bbbbaaa";"aaaabaa";"baaabaa";"abaabaa";"bbaabaa";"aababaa";"bababaa";"abbabaa";"bbbabaa";"aaabbaa";"baabbaa";"ababbaa";"bbabbaa";"aabbbaa";"babbbaa";"abbbbaa";"bbbbbaa";"aaaaaba";"baaaaba";"abaaaba";"bbaaaba";"aabaaba";"babaaba";"abbaaba";"bbbaaba";"aaababa";"baababa";"abababa";"bbababa";"aabbaba";"babbaba";"abbbaba";"bbbbaba";"aaaabba";"baaabba";"abaabba";"bbaabba";"aababba";"bababba";"abbabba";"bbbabba";"aaabbba";"baabbba";"ababbba";"bbabbba";"aabbbba";"babbbba";"abbbbba";"bbbbbba";"baaaaab";"abaaaab";"bbaaaab";"aabaaab";"babaaab";"abbaaab";"bbbaaab";"baabaab";"ababaab";"bbabaab";"aabbaab";"babbaab";"abbbaab";"bbbbaab";"baaabab";"abaabab";"bbaabab";"aababab";"bababab";"abbabab";"bbbabab";"baabbab";"ababbab";"bbabbab";"aabbbab";"babbbab";"abbbbab";"bbbbbab";"baaaabb";"abaaabb";"bbaaabb";"aabaabb";"babaabb";"abbaabb";"bbbaabb";"baababb";"abababb";"bbababb";"aabbabb";"babbabb";"abbbabb";"bbbbabb";"baaabbb";"abaabbb";"bbaabbb";"aababbb";"bababbb";"abbabbb";"bbbabbb";"baabbbb";"ababbbb";"bbabbbb";"aabbbbb";"babbbbb";"abbbbbb";"bbbbbbb"] = ["a";"b";"aa";"ba";"ab";"bb";"aaa";"baa";"aba";"bba";"aab";"bab";"abb";"bbb";"aaaa";"baaa";"abaa";"bbaa";"aaba";"baba";"abba";"bbba";"baab";"abab";"bbab";"aabb";"babb";"abbb";"bbbb";"aaaaa";"baaaa";"abaaa";"bbaaa";"aabaa";"babaa";"abbaa";"bbbaa";"aaaba";"baaba";"ababa";"bbaba";"aabba";"babba";"abbba";"bbbba";"baaab";"abaab";"bbaab";"aabab";"babab";"abbab";"bbbab";"baabb";"ababb";"bbabb";"aabbb";"babbb";"abbbb";"bbbbb";"aaaaaa";"baaaaa";"abaaaa";"bbaaaa";"aabaaa";"babaaa";"abbaaa";"bbbaaa";"aaabaa";"baabaa";"ababaa";"bbabaa";"aabbaa";"babbaa";"abbbaa";"bbbbaa";"aaaaba";"baaaba";"abaaba";"bbaaba";"aababa";"bababa";"abbaba";"bbbaba";"aaabba";"baabba";"ababba";"bbabba";"aabbba";"babbba";"abbbba";"bbbbba";"baaaab";"abaaab";"bbaaab";"aabaab";"babaab";"abbaab";"bbbaab";"baabab";"ababab";"bbabab";"aabbab";"babbab";"abbbab";"bbbbab";"baaabb";"abaabb";"bbaabb";"aababb";"bababb";"abbabb";"bbbabb";"baabbb";"ababbb";"bbabbb";"aabbbb";"babbbb";"abbbbb";"bbbbbb";"aaaaaaa";"baaaaaa";"abaaaaa";"bbaaaaa";"aabaaaa";"babaaaa";"abbaaaa";"bbbaaaa";"aaabaaa";"baabaaa";"ababaaa";"bbabaaa";"aabbaaa";"babbaaa";"abbbaaa";"bbbbaaa";"aaaabaa";"baaabaa";"abaabaa";"bbaabaa";"aababaa";"bababaa";"abbabaa";"bbbabaa";"aaabbaa";"baabbaa";"ababbaa";"bbabbaa";"aabbbaa";"babbbaa";"abbbbaa";"bbbbbaa";"aaaaaba";"baaaaba";"abaaaba";"bbaaaba";"aabaaba";"babaaba";"abbaaba";"bbbaaba";"aaababa";"baababa";"abababa";"bbababa";"aabbaba";"babbaba";"abbbaba";"bbbbaba";"aaaabba";"baaabba";"abaabba";"bbaabba";"aababba";"bababba";"abbabba";"bbbabba";"aaabbba";"baabbba";"ababbba";"bbabbba";"aabbbba";"babbbba";"abbbbba";"bbbbbba";"baaaaab";"abaaaab";"bbaaaab";"aabaaab";"babaaab";"abbaaab";"bbbaaab";"baabaab";"ababaab";"bbabaab";"aabbaab";"babbaab";"abbbaab";"bbbbaab";"baaabab";"abaabab";"bbaabab";"aababab";"bababab";"abbabab";"bbbabab";"baabbab";"ababbab";"bbabbab";"aabbbab";"babbbab";"abbbbab";"bbbbbab";"baaaabb";"abaaabb";"bbaaabb";"aabaabb";"babaabb";"abbaabb";"bbbaabb";"baababb";"abababb";"bbababb";"aabbabb";"babbabb";"abbbabb";"bbbbabb";"baaabbb";"abaabbb";"bbaabbb";"aababbb";"bababbb";"abbabbb";"bbbabbb";"baabbbb";"ababbbb";"bbabbbb";"aabbbbb";"babbbbb";"abbbbbb";"bbbbbbb"];;