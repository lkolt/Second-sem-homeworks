package GIF

/**
  * Created by lkolt on 26.05.2016.
  */

import scodec._
import bits._
import scodec.codecs._
import scodec.bits.ByteOrdering.{LittleEndian}
import scala.collection.mutable.ArrayBuffer
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.nio.file.{Files, Paths}
import java.io._
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.image.BufferedImage

class DataPanel(data: Array[Array[Color]], BG: String) extends Panel {

  override def paintComponent(g: Graphics2D) {
    val width = data.length
    val height = data.map(_.length).max
    val dx = g.getClipBounds.width.toFloat  / width
    val dy = g.getClipBounds.height.toFloat / height
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      data(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(Color.decode(BG))
      }

      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }

}

object Timer {
  def apply(interval: Int, repeats: Boolean = true)(op: => Unit) {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}

object render extends SimpleSwingApplication {

  val decodedpict = new Decoder("gif.gif");

  val width = decodedpict.HeadOfGIF.sizes.width
  val height = decodedpict.HeadOfGIF.sizes.height
  val scale = 10
  val el = decodedpict.body.ImList.toArray
  var time = decodedpict.body.GraphContrList.toArray
  if (time.isEmpty) time = time:+(GraphicControl(0,0,1000,0))
  def top = new MainFrame {

    var i = 0
    Timer(time(i).delay) {
      if (i == el.length) i = 0
      tick(i)
      i = i + 1
    }

    def tick(i: Int) ={
      val data = Array.ofDim[Color](width, height)

      for {
        x <- 0 until data.length
        y <- 0 until data(x).length} {

        if (x < el(i).imp.left || y < el(i).imp.top) data(x)(y) = new Color(0,0,0,0)
        else data(x + el(i).imp.left)(y + el(i).imp.top) = Color.decode(el(i).decoded(y)(x))
      }
      { contents = new DataPanel(data, decodedpict.HeadOfGIF.BG) {
        preferredSize = new Dimension(width * scale, height * scale)}
      }

    }

  }
}

case class GifHead(sizes: GIFSizes, BG: String, palitrpar: PalitraParametrs, GlobalPalitra: Array[BitVector])
case class GifBody(GraphContrList: List[GraphicControl], TextList: List[PlainText], ImList: List[Images])
case class GIFSizes(width : Int, height : Int)
case class PalitraParametrs(deep: Int, size: Int)
case class ImageParamet(left: Int, top: Int, pH: Int, pW: Int)

case class Images(imp: ImageParamet, lzwstart: Int, v: BitVector, palitraParam: PalitraParametrs, palitra: Array[BitVector]){

  private val maxnum = palitra.length + 2
  private var library = ArrayBuffer[List[Int]]()
  for (i<- 0 until maxnum){
    library += i.toInt :: Nil
  }
  private def reversing (vect: BitVector, total: BitVector): BitVector = {
    if (vect.isEmpty) total
    else reversing(vect.drop(8), total.++(vect.take(8).reverseBitOrder))
  }
  private def lzwdecode (start: BitVector, tot: ArrayBuffer[List[Int]], s: Int, lastnum: Int): ArrayBuffer[List[Int]] = {
    if (start.take(s).reverseBitOrder.toInt(false, LittleEndian) != maxnum - 2 && tot.isEmpty) lzwdecode(start.drop(lzwstart), tot, lzwstart, 0)
    else if (start.take(s).reverseBitOrder.toInt(false, LittleEndian) == maxnum - 2 ) {
      library = library.take(maxnum)
      lzwdecode(start.drop(s + lzwstart), tot += (start.drop(s).take(lzwstart).reverseBitOrder.toInt(false,LittleEndian)::Nil),lzwstart, 0)
    } else if (start.take(s).reverseBitOrder.toInt(false,LittleEndian) == maxnum - 1 || start.isEmpty) {
      tot
    } else {
      val n = start.take(s).reverseBitOrder.toInt(false, LittleEndian)
      if (library.length > n) {
        val x1 = library(n).head
        library += tot(lastnum):::(x1::Nil)
        if (library.length < math.pow(2, s)) lzwdecode(start.drop(s), tot += library(n), s, lastnum+1)
        else lzwdecode(start.drop(s), tot += library(n), s + 1, lastnum + 1)
      } else {
        val x1 = tot(lastnum).head
        library += tot(lastnum):::(x1::Nil)
        if (library.length < math.pow(2, s)) lzwdecode(start.drop(s), tot += library(n), s, lastnum+1)
        else lzwdecode(start.drop(s), tot += library(n), s + 1, lastnum + 1)
      }
    }
  }

  private def tocolorarray (l: List[Int], len: Int): Array[Array[String]] = {
    def newstring (ls: List[Int]): Array[String] = {
      val ar = new Array[String](len)
      for {i<- 0 until len}{
        if (i == palitra.length) ar(i) = "clear"
        else ar(i) = "#" + palitra(ls(i)).toHex
      }
      ar
    }

    def forall (Ls: List[Int], t: ArrayBuffer[Array[String]]): ArrayBuffer[Array[String]] = {
      if (Ls.isEmpty) t
      else forall(Ls.drop(len), t += newstring(Ls))
    }
    forall(l, ArrayBuffer[Array[String]]()).toArray
  }

  private val revbits = reversing(v, v.take(0))
  private val predecodedimage = lzwdecode(revbits, ArrayBuffer(), lzwstart, -1).toList.flatten
  val decoded = tocolorarray(predecodedimage, imp.pW)
}

case class GraphicControl(disp: Int, other: Int, delay: Int, Tr: Int)
case class TextPar(left: Int, top: Int, W:Int, H: Int, cW: Int, cH: Int, TextCol: Int, BG: Int)
case class PlainText(par: TextPar, Text: Array[Char])

class Decoder(path : String) {

  private def head (bitVector: BitVector): GIFSizes = {
    try {
      val headerCodec = (constant(hex"474946383961".bits) :: uint16L :: uint16L).as[GIFSizes] //GIF89a
      val decoded = headerCodec.decode(bitVector)
      decoded.require.value
    }
    catch {
      case e: IllegalArgumentException => {
        val headerCodec = (constant(hex"474946383761".bits) :: uint16L :: uint16L).as[GIFSizes] //GIF87a
        val decoded = headerCodec.decode(bitVector)
        decoded.require.value
      }
    }
  }

  private def pal(pa: BitVector): PalitraParametrs = {
    val d = pa.drop(1).take(3).toInt(false,LittleEndian)
    val s = pa.drop(4).take(4).toInt(false,LittleEndian)
    val par = PalitraParametrs(d + 1, (math.pow(2, s + 1).toInt))
    par
  }

  private def colors (c: BitVector, p: PalitraParametrs): Array[BitVector] ={
    val ColAr = new Array[BitVector](p.size)
    var col = c
    for {i <- 0 until p.size }{
      ColAr(i) = col.take(3 * 8)
      col = col.drop(3 * 8)
    }
    ColAr
  }

  private def headsplit(bitvec: BitVector): GifHead = {
    val Sizes = head(bitvec)
    var bitv = bitvec.drop(48 + 32)
    if (bitv(0) == false) {
      bitv = bitv.drop(8)
      val BG = "#" + bitv.take(8).toHex
      GifHead(Sizes, BG, PalitraParametrs(0,0), null)
    }
    else {
      val Glparam = pal(bitv.take(8))
      println(Glparam)
      bitv = bitv.drop(8)
      val BG = "#" + bitv.take(8).toHex
      bitv = bitv.drop(16)
      GifHead(Sizes,BG,Glparam,(colors(bitv, Glparam)))
    }
  }

  private def split(vec: BitVector): BitVector = {
    def cicle (vect: BitVector, lzwim: BitVector): BitVector = {
      val s = vect.take(8).toInt(false, LittleEndian)
      println(s)
      if (s == 0) lzwim
      else  cicle(vect.drop(8 + s * 8),lzwim.++(vect.drop(8).take(s * 8)))
    }
    cicle(vec.drop(8), vec.take(0))
  }

  private def dr (v: BitVector): Int = {
    def cicle (vec: BitVector, d: Int): Int = {
      val s = vec.take(8).toInt(false, LittleEndian)
      if (s == 0) d+8
      else cicle(vec.drop(8 + s * 8), d + 8 + s * 8)
    }
    cicle(v, 0)
  }

  private def bodydecode (gifH: GifHead, v: BitVector, GraphContrList: List[GraphicControl], TextList: List[PlainText], ImList: List[Images]): GifBody = {
    if (hex"3b".bits == v.take(8)) GifBody(GraphContrList, TextList, ImList)
    else if (hex"2c".bits == v.take(8)) {
      var vec = v.drop(8)
      val par = (uint16L :: uint16L :: uint16L :: uint16L).as[ImageParamet]
      val decoded = par.decode(vec)
      vec = vec.drop(16 * 4)
      if (vec.take(1) == true) {
        val loc = pal(vec.take(8))
        val pict = Images(decoded.require.value, vec.drop(8 + 8 * 3 * loc.size).take(8).toInt(false,LittleEndian) + 1,split(vec.drop(8 + 8 * 3 * loc.size)),loc,colors(vec.drop(8), loc))
        if (pict.v.length % (254 * 8) == 0) bodydecode(gifH, vec.drop(16+8 + 8 * 3 * loc.size + pict.v.length + 8 * (pict.v.length / (8 * 254))), GraphContrList, TextList, pict :: ImList)
        else bodydecode(gifH,vec.drop(16+8 + 8 * 3 * loc.size + pict.v.length + 8 * (pict.v.length / (8 * 254)) + 8), GraphContrList, TextList, pict :: ImList)
      } else {
        val pict = Images(decoded.require.value, vec.drop(8).take(8).toInt(false,LittleEndian) + 1,split(vec.drop(8)),gifH.palitrpar, gifH.GlobalPalitra)
        if (pict.v.length % (254 * 8) == 0) bodydecode(gifH, vec.drop(16+8 + pict.v.length + 8 * (pict.v.length.toInt / (8 * 254))), GraphContrList, TextList,  pict :: ImList)
        else bodydecode(gifH,vec.drop(16+8 + pict.v.length + 8 * (pict.v.length.toInt / (8 * 254)) + 8), GraphContrList, TextList, pict :: ImList)
      }
    } else {
      if (hex"f9".bits == v.drop(8).take(8) ){
        val vec = v.drop(16 + 8 + 3)
        val grap = (uintL(3):: uintL(2):: uint16L :: uint8L).as[GraphicControl]
        val decoded = grap.decode(vec)
        println(decoded.require.value)
        bodydecode(gifH, vec.drop(3 + 2 + 16 + 8 + 8),decoded.require.value::GraphContrList,TextList,ImList)
      } else if (hex"fe".bits == v.drop(8).take(8)) {
        val d = dr(v.drop(8+8))
        bodydecode(gifH, v.drop(d + 16 + 8),GraphContrList,TextList,ImList)
      } else if(hex"01".bits == v.drop(8).take(8)){
        val vec = v.drop(16 + 8)
        val param = (uint16L::uint16L::uint16L::uint16L::uint8L::uint8L::uint8L::uint8L).as[TextPar]
        val decoded = param.decode(vec)
        val text = split(vec.drop(12 * 8))
        val bytetext = text.toByteArray
        val t = new Array[Char](bytetext.length)
        for {i <- 0 until bytetext.length}{
          t(i) = bytetext(i).toChar
        }
        val txt = PlainText(decoded.require.value,t)
        if (text.length % 254 == 0) bodydecode(gifH, vec.drop(12 * 8 + text.length + 8 * (text.length / (8 * 254))),GraphContrList,txt::TextList,ImList)
        else  bodydecode(gifH, vec.drop(12 * 8 + text.length + 8 * (text.length / (8 * 254)) + 8),GraphContrList,txt::TextList,ImList)
      } else {
        bodydecode(gifH, v.drop(19 * 8),GraphContrList,TextList,ImList)
      }
    }
  }

  val byteArray = Files.readAllBytes(Paths.get(path))
  var bitVector = BitVector(byteArray)
  val HeadOfGIF = headsplit(bitVector)
  bitVector = bitVector.drop(13 * 8 + HeadOfGIF.palitrpar.size * 8 * 3)
  val body = bodydecode(HeadOfGIF,bitVector, Nil, Nil, Nil)
}
