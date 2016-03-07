  def listSum(ls: List[Int]) : Int = {
    if (ls.isEmpty){
      0
    } else {
      ls.head + listSum(ls.tail)
    }
  }