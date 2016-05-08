  def check(lis: List[Char]): Boolean = {
    var bal = 0
    var ans = true
    var ls = lis
    while (ls.nonEmpty){
      if (ls.head == '('){
        bal += 1
      } else {
        bal -= 1
      }
      if (bal < 0) {
        ans = false
      } else {
        ls = ls.tail
      }
    }
    if (bal != 0){
      ans = false
    }
    ans
  }