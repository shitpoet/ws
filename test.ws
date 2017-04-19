// line comment
/* block comment */
fun expr()
  ret 1-1

fun indented-exprs()
  let x = 1 -
    1 // the same expr
  let y = +1
    + 1 // seprate expr
  ret x == 0 && y == 1

fun test-exprs()
  ret expr()===0 && indented-exprs()

fun test-loops()
  while false;
  while true
    //break
    break;
  fun dummy(){}
  ret true

fun test-include()
  +include 'build/included.ws'
  ret included-func() === 123

fun test()
  let tests = [
    test-exprs,
    test-loops,
    test-include,
  ]
  for test of tests
    if !test() ret false
  ret true
  /*ret (
    test-exprs() &&
    test-loops() &&
    test-include()
  )*/

ret test()


