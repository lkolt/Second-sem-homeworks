  def pushBack[A](ls: List[A], x: Int) = {
    List.concat(ls, List(x))
  }