  def pushBack[Int](ls: List[Int], x: Int) : List[Int] = {
    if (ls.isEmpty){
      List(x)
    } else {
      ls.head :: pushBack(ls.tail, x)
    }
  }
