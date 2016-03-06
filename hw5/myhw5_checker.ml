startConfig asbs "" = {state = "start"; before = []; after = [">"]};;
startConfig asbs "ab" = {state = "start"; before = []; after = [">"; "a"; "b"]};;
startConfig asbs "aaaabbbaa" = {state = "start"; before = []; after = [">"; "a"; "a"; "a"; "a"; "b"; "b"; "b"; "a"; "a"]};;
startConfig anbn "" = {state = "start"; before = []; after = ["|"]};;
startConfig anbn "aabb" = {state = "start"; before = []; after = ["|"; "a"; "a"; "b"; "b"]};;
startConfig anbn "aabbaa" = {state = "start"; before = []; after = ["|"; "a"; "a"; "b"; "b"; "a"; "a"]};;

acceptConfig asbs {state="start"; before=[]; after=["_"]} = false;;
acceptConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]} = false;;
acceptConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]} = true;;
acceptConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]} = false;;
rejectConfig asbs {state="start"; before=[]; after=["_"]} = false;;
rejectConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]} = false;;
rejectConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]} = false;;
rejectConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]} = true;;
haltConfig asbs {state="start"; before=[]; after=["_"]} = false;;
haltConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]} = false;;
haltConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]} = true;;
haltConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]} = true;;

step asbs {state="start"; before=[]; after=["_"]} = {state = "acc"; before = ["_"]; after = []};;
step asbs {state="start"; before=[">";"a"]; after=["b";"b"]} = {state = "q1"; before = [">"; "a"; "b"]; after = ["b"]};;
step asbs {state="q1"; before=[">";"a"]; after=["a";"b"]} = {state = "rej"; before = [">"; "a"; "a"]; after = ["b"]};;
step asbs {state="q1"; before=[">";"a"]; after=["b";"b"]} = {state = "q1"; before = [">"; "a"; "b"]; after = ["b"]};;
step anbn {state="q1"; before=["|";"a";"b"]; after=["/"]} = {state = "q2"; before = ["|"; "a"; "b"; "/"]; after = []};;
step anbn {state="q2"; before=["|";"a";"b"]; after=["/"]} = {state = "q2"; before = ["|"; "a"]; after = ["b"; "/"]};;
step anbn {state="q3"; before=["|"]; after=["a";"b"]} = {state = "q4"; before = ["|"; "X"]; after = ["b"]};;
step anbn {state="q4"; before=["|";"X"]; after=["b"]} = {state = "q2"; before = ["|"; "X"; "X"]; after = []};;