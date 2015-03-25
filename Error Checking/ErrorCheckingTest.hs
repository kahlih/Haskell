import Base
import FirstClassFunctions hiding (evaluate, execute)
import FirstClassFunctionsParse
import ErrorChecking

testUBV = execute (parseExp "x")

testDBZ2 = execute (parseExp "3 / 0")

test3 = execute (parseExp "-true")
test4 = execute (parseExp "!4")
test5 = execute (parseExp "4 + true")
test6 = execute (parseExp "4 && false")

test7 = execute (parseExp "if (x) 3 else 4")
test8 = execute (parseExp "if(3 == 3) -true else 1")
test9 = execute (parseExp "if (1 == 2) 2 else y")
test10 = execute (parseExp "var x = 3; var y = t; x")
test11 = execute (parseExp "var y = 3; z")
test12 = execute (parseExp "var x = 2; if (x) 3 else 4")
test13 = execute (parseExp "var x = 2; try { x / 0 } catch { x / 2 }")

p5 = execute(parseExp (
 "var map = function(f) { function(x) { function(y) { f(x) + f(y) }}};"++
 "var g = function(x) { x + 1 };"++
 "map(x)(3)(4)"))

p6 = execute(parseExp (
 "var map = function(f) { function(x) { function(y) { f(x) + f(y) }}};"++
 "var g = function(x) { x + 1 };"++
 "map(g)(-true)(4)"))

p7 = execute(parseExp (
 "var map = function(f) { function(x) { function(y) { f(x) + f(y) }}};"++
 "var g = function(x) { x + 1 };"++
 "map(3)(4)(4)"))
 
p8 = execute(parseExp (
 "var map = function(f) { function(x) { function(y) { f(x) + f(y) }}};"++
 "var g = function(x) { x + 1 };"++
 "map(g)(g)(4)"))

p10 = execute(parseExp (
 "var x = 2; try { try { var f = function(y) { 1/y }; f(0)} catch { f(x) } } catch { 42 }")) 


main = do
  --print "beginning error checking"
  tagged "testUBV" (print testUBV)
  tagged "testDBZ2" (print testDBZ2)
  print test3
  print test4
  print test5
  print test6
  print test7
  print test8
  print test9
  print test10
  print test11
  print test12
  print test13

  print p5
  print p6
  print p7
  print p8
  print p10
  