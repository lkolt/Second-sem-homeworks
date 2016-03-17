  def reversList[A](ls: List[A]) : List[A] = {
    if (ls.isEmpty){
      List()
    } else {
      reversList(ls.tail) ::: List(ls.head)
    }
  }
