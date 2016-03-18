  def reversList[A](ls: List[A]) = {
    def loop[A](ls: List[A], acc: List[A]) : List[Any] = {
      if (ls.isEmpty){
        acc
      } else {
        loop(ls.tail, List(ls.head) ::: acc)
      }
    }
    loop(ls, List())
  }
