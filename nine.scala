  def listFilter[A](ls: List[A], x: A) : List[A] = {
    if (ls.isEmpty){
      List()
    } else {
      if (ls.head == x){
        List.concat(List(x), listFilter(ls.tail, x))
      } else {
        listFilter(ls.tail, x)
      }
    }