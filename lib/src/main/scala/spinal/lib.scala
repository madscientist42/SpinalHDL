package spinal

import spinal.core._
import spinal.lib.tools._
import scala.collection.mutable.ListBuffer
import scala.collection.Iterable

package object lib  {
  //  def Stream[T <: Data](that : T) : Stream[T] = new Stream[T](that)
  //  def mm [T <: Data with IMasterSlave](that : T) = that.asMaster()
  type Event = Stream[NoData]


  def Event = new Stream(NoData)

  def NoData = new NoData


  //implicit def easyStream[T <: Bundle](that: Stream[T]) = that.data
  implicit def traversableOncePimped[T <: Data](that: Seq[T]) = new TraversableOncePimped[T](that)
  implicit def traversableOnceBoolPimped(that: Seq[Bool]) = new TraversableOnceBoolPimped(that)
  implicit def traversableOnceAnyPimped[T <: Any](that: Seq[T]) = new TraversableOnceAnyPimped(that)



//  implicit def seqPimped[T <: Data](that: scala.IndexedSeq[T]) = new TraversableOncePimped[T](that)

  implicit def flowFragmentPimped[T <: Data](that: Flow[Fragment[T]]) = new FlowFragmentPimped[T](that)
  implicit def streamFragmentPimped[T <: Data](that: Stream[Fragment[T]]) = new StreamFragmentPimped[T](that)
  implicit def streamBitsPimped[T <: Data](that: Stream[Bits]) = new StreamBitsPimped(that)
  implicit def flowBitsPimped[T <: Data](that: Flow[Bits]) = new FlowBitsPimped(that)
  implicit def dataCarrierFragmentPimped[T <: Data](that: DataCarrier[Fragment[T]]) = new DataCarrierFragmentPimped[T](that)
  implicit def dataCarrierFragmentBitsPimped(that: DataCarrier[Fragment[Bits]]) = new DataCarrierFragmentBitsPimped(that)
  implicit def streamFragmentBitsPimped(that: Stream[Fragment[Bits]]) = new StreamFragmentBitsPimped(that)
  implicit def stringPimped(that: String) = new StringPimped(that)
  implicit def memPimped[T <: Data](mem: Mem[T]) = new MemPimped(mem)
  implicit def boolPimped(that: Bool) = new BoolPimped(that)

  implicit class UIntPimper(that : UInt){
    def toOneHot : Bits = B"1" << that
  }

  
  def StreamArbiterFactory = new StreamArbiterFactory()
  type ScalaStream[T] = collection.immutable.Stream[T]
  def ScalaStream = collection.immutable.Stream

  // Bring in the polynomial (LFSR) support from SpinalCrypto because it's Generic to everything, not
  // just Crypto functions.  
  /**
    * Used to create polynomial as follow p"x^2 + x + 1"
    */
  implicit class LiteralBuilderPolynomial(private val sc: StringContext) {
    def p(args: Any*): PolynomialGF2 = str2PolynomialGF2(getString(args))

    private def getString(args: Any*): String = {
      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next().toString)

      while (ai.hasNext) {
        if (ai.hasNext && !ai.next.isInstanceOf[List[_]]) bldr append ai.next
        if (pi.hasNext && !pi.next.isInstanceOf[List[_]]) bldr append pi.next
      }

      bldr.result.replace("_", "")
    }
  }


  /**
    * Convert a string into a polynomial
    */
  private[lib] def str2PolynomialGF2(polyStr: String): PolynomialGF2 = {

    assert(polyStr.length > 0, "Empty  polynomial")

    /**
      * Polynomial str into list of coefficient
      */
    def polynomialStrDecoder(p: String): List[Int] = {

      // Get all coefficient
      var duplicate0 = 0
      val pp = """x\^([0-9]+)""".r
      def getCoef(str: List[String]) : List[Int] = str match{
        case "x" :: tail       => 1 :: getCoef(tail)
        case "1" :: tail       => duplicate0 += 1 ; 0 :: getCoef(tail)
        case "0" :: tail       => duplicate0 += 1 ; getCoef(tail)
        case pp(value) :: tail => value.toInt :: getCoef(tail)
        case Nil               => Nil
        case _                 => throw new Exception(s"The polynomial $p is not valid. ")
      }

      val coefficientList = getCoef(p.split('+').toList)

      // Check if there some duplicate coefficient
      val duplicateCoef = coefficientList.diff(coefficientList.distinct)
      assert(duplicateCoef.length == 0 && duplicate0 <= 1, s"Polynomial $p has duplicate coefficient ${duplicateCoef.mkString(",")}")


      return coefficientList
    }


    /**
      * Polynomial bin/hex into list of coefficient
      */
    def polynomialNumberDecoder(radix: Int, p: String): List[Int] = {

      assert(List(2,16).contains(radix), "The following radix for polynomial is forbidden")

      // remove all _
      var strPoly = p.replace("_", "").toLowerCase

      // convert hexadecimal str into binary string
      var bitCount = -1
      if(radix == 16){
        val split = strPoly.split(''')
        bitCount  = split(0).toInt
        strPoly   = split(1).substring(1)
        strPoly   = BigIntToListBoolean(BigInt(strPoly, 16), bitCount bits).map(b => if(b) "1" else "0").reverse.mkString("")
      }else{
        strPoly = strPoly.substring(1)
      }

      // Convert the binary string into list of coefficient
      val listBuffer = new ListBuffer[Int]()
      for((b,i) <- strPoly.reverse.zipWithIndex){
        if(b == '1') listBuffer.append(i)
      }

      // append for hexadecimal polynomial the higher coefficient
      if(bitCount != -1){
        listBuffer.append(bitCount)
      }

      return listBuffer.toList
    }

    // remove all spaces
    val poly = polyStr.replace(" ", "")

    // detect the format of the string
    val rhex = """[0-9]+\'x[0-9a-fA-F_]+""".r
    val rbin = """b[0-1_]+""".r
    val rstr = """[0-9x\^\+]+""".r

    val polynomial = poly match{
      case rhex() => polynomialNumberDecoder(16, poly)
      case rbin() => polynomialNumberDecoder(2, poly)
      case rstr() => polynomialStrDecoder(poly)
      case _      => throw new Exception("Polynomial format issue")
    }

    return new PolynomialGF2(polynomial.sortWith(_ > _))
  }

  
}
