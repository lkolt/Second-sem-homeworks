  def reversList[A](ls: List[A]) : List[A] = {
    if (ls.isEmpty){
      List()
    } else {
      List.concat(reversList(ls.tail), List(ls.head))
    }
  }