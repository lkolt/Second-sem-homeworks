/**
  * Created by lkolt on 07.03.2016.
  */
object work {
  def sqr(x: Double) = x * x
  def cub(x: Double) = x * x * x
  def sign(x: Double) = if (x > 0) 1 else -1
  def abs(x: Double) = Math.abs(x)

  def Solve_pol() = {
    println("enter the degree of the polynom")
    val n = readInt()
    if (n == 2) {
      println("enter per line A, B and C(Ax^2 + Bx + C = 0)")
      val A = readDouble()
      val B = readDouble()
      val C = readDouble()
      val D = B * B - 4 * A * C

      if (D < 0){
        List()
      } else if (D == 0){
        List(B / 2 * A)
      } else if (D > 0){
        List((B - Math.sqrt(D)) / (2 * A), (B + Math.sqrt(D)) / (2 * A))
      }
    } else if (n == 3){ // Using method Viete-Cardano
      println("enter per line A, B, C and D(Ax^3 + Bx^2 + Cx + D = 0)")
      val D = readDouble()
      var A = readDouble()
      var B = readDouble()
      var C = readDouble()
      A = A / D
      B = B / D
      C = C / D
      val Q = (sqr(A) - 3 * B) / 9
      val R = (2 * cub(A) - 9 * A * B + 27 * C) / 54
      val PI = 3.141592653589793

      if (sqr(R) < cub(Q)){
        val t = Math.acos(R / Math.sqrt(cub(Q))) / 3
        val x1 = -2 * Math.sqrt(Q) * Math.cos(t) - A / 3
        val x2 = -2 * Math.sqrt(Q) * Math.cos(t + 2 * PI / 3) - A / 3
        val x3 = -2 * Math.sqrt(Q) * Math.cos(t - 2 * PI / 3) - A / 3;
        List(x1, x2, x3)
      } else {
        val V = -sign(R) * Math.pow(abs(R) + Math.sqrt(sqr(R) - cub(Q)), 1/3.0);
        var U = 0.0
        if (V != 0){
          U = Q / V
        }
        val x1 = (U + V) - A / 3
        if (U == B){
          val x2 = V - A / 3
          List(x1, x2)
        } else {
          List(x1)
        }
      }
    } else {
      println("Input Error")
      List()
    }
  }

  def main(args: Array[String]): Unit = {
    val ls = Solve_pol()
    println(ls)
  }
}
