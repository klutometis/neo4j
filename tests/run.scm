(use neo4j test)

(test
 "a b c"
 (replace "a {x} c" "x" "b"))

(test
 "a b {y} d"
 (replace "a {x} {y} d" "x" "b"))

(test
 "a b c d"
 (replace "a {x} {y} d"
          '(("x" . "b")
            ("y" . "c"))))
