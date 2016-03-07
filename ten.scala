  def isDif(x: Int) = if ((x >= 2) && (x <= 4)) true else false

  def listFilterFunc[A](ls: List[A], func: (A) => Boolean): List[A] = {
    if (ls.isEmpty){
      List()
    } else {
      if (func(ls.head)){
        List.concat(List(ls.head), listFilterFunc(ls.tail, func))
      } else {
        listFilterFunc(ls.tail, func)
      }
    }
  }