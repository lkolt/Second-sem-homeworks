import scala.annotation.tailrec

/**
  * Created by lkolt on 15.04.2016.
  */
object Control_work {

  abstract class txtElem {
    val len : Int
  }

  case class txtWord(s: String) extends txtElem {
    val str = s
    val len = s.length()
  }

  case class txtSpace(vids: Int) extends txtElem {
    val len = vids
  }

  def fnd(txt : List[txtWord], bal : Int): (List[txtWord], List[txtWord], Int) = {
    if (txt.isEmpty){
      (List(), List(), bal + 1) // balance + 1 for last
    } else if (txt.head.len <= bal){
      val pans = fnd(txt.tail, bal - txt.head.len - 1) // len + 1 for space
      (txt.head :: pans._1, pans._2, pans._3)
    } else {
      (List(), txt, bal + 1)
    }
  }

  def init(in : List[String]) : List[txtWord] = {
    @annotation.tailrec
    def loop(sub : List[String], acc : List[txtWord]) : List[txtWord] ={
      if (sub.isEmpty){
        acc
      } else {
        loop(sub.tail, acc ++ List(txtWord(sub.head)))
      }
    }
    loop(in, List())
  }

  def addSpaces(in : List[txtWord], cnt : Int = 1, add : Int = 0) : List[txtElem] = {
    @annotation.tailrec
    def loop(sub : List[txtWord], acc : List[txtElem], add : Int) : List[txtElem] = {
      if (sub.tail.isEmpty) {
        acc ++ List(sub.head)
      } else if (add > 0){
        loop(sub.tail, acc ++ List(sub.head) ++ List(txtSpace(cnt + 1)), add - 1)
      } else {
        loop(sub.tail, acc ++ List(sub.head) ++ List(txtSpace(cnt)), add)
      }
    }
    loop(in, List(), add)
  }

  def needSpace(n: Int): List[txtSpace] ={
    if (n == 0){
      List()
    } else {
      List(txtSpace(n))
    }
  }

  def alignLeft(txt : List[txtWord], bal : Int) : List[txtElem] = {
    addSpaces(txt)
  }

  def alignRight(txt : List[txtWord], bal : Int) : List[txtElem] = {
    needSpace(bal) ++ addSpaces(txt)
  }

  def alignCentr(txt : List[txtWord], bal : Int) : List[txtElem] = {
    val balR = bal / 2
    val balL = bal - balR
    needSpace(balL) ++ addSpaces(txt) ++ needSpace(balR)
  }

  def alignFill(txt : List[txtWord], bal : Int) : List[txtElem] = {
    val len = txt.length
    if (len == 1) {
      txt
    } else {
      val spcAdd = (bal / (len - 1))
      val add = bal - spcAdd * (len - 1)
      addSpaces(txt, spcAdd + 1, add)
    }
  }

  def part(cur: List[txtWord], bal: Int, format: String, isLast : Boolean) : List[txtElem] = {
    format match {
      case "L" => alignLeft(cur, bal)
      case "R" => alignRight(cur, bal)
      case "C" => alignCentr(cur, bal)
      case "F" => if (isLast) alignFill(cur, bal) else alignLeft(cur, bal)
    }
  }

  def Solve(in : List[String], width : Int, format: String) : List[List[txtElem]] = {
    var txt = init(in)
    var ans : List[List[txtElem]] = Nil
    var isWork = true
    while (isWork){
      val curFnd = fnd(txt, width)
      isWork = curFnd._2.nonEmpty
      txt = curFnd._2
      val bal = curFnd._3
      val pans = part(curFnd._1, bal, format, isWork)
      ans = ans ++ List(pans)
    }
    ans
  }

  def write(txt: List[List[txtElem]]) : Unit ={
    def determ(el: txtElem) : Unit = {
      el match {
        case el: txtWord => print(el.str)
        case el: txtSpace => {var i = 0; while (i < el.len) {print(" "); i += 1}}
      }
    }
    def in(ins: List[txtElem]) : Unit = {
      if (ins.nonEmpty){
        determ(ins.head)
        in(ins.tail)
      }
    }
    def out(outs: List[List[txtElem]]) : Unit = {
      if (outs.nonEmpty){
        in(outs.head)
        println()
        out(outs.tail)
      }
    }
    out(txt)
  }

  def main(args: Array[String]) : Unit = {
    val inputTxt = List("Test", "this", "solution", "is", "our", "life", "psh", "pshhh", "Huston", "u", "nas", "problemi", "a", "a", "a", "a", "a", "last")
    val inputWidth = 9
    val inputFormat = "L" // "L" - left, "R" - right, "C" - central, "F" - fill
    val ans = Solve(inputTxt, inputWidth, inputFormat)
    println(ans)
    write(ans)
  }
}
