  def listLen[A](ls: List[A]) : Int = {
    if (ls.isEmpty){
      0
    } else {
      1 + listLen(ls.tail)
    }
  }