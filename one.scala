  def Solve(x: Int, y : Int): Int = {
    if ((x == y) || (y == 0)){
      1
    } else {
      Solve(x - 1, y) + Solve(x - 1, y - 1)
    }
  }