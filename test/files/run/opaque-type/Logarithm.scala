import scala.language.implicitConversions

object opaquetypes {
  // The opaque type definition
  opaque type Logarithm = Double

  // Try to check that an opaque type can be used in a val definition
  val l: Logarithm = 1.toDouble.asInstanceOf[Logarithm]

  // Logarithm$class, ideally all the methods would be static
  object Logarithm {

    // HACK: Should be implicitly added by namer in the future
    private implicit def log2Double(l: Logarithm): Double = l.asInstanceOf[Double]
    private implicit def double2Log(d: Double): Logarithm = d.asInstanceOf[Logarithm]

    val l2: Logarithm = 1.toDouble

    // These are the ways to lift to the logarithm opaque type
    def apply(d: Double): Logarithm =
      math.log(d)
    def safe(d: Double): Option[Logarithm] =
      if (d > 0.0) Some(math.log(d)) else None

    // here you would define all the extension methods (0 or more)
    implicit class LogarithmOps(val `this`: Logarithm) extends AnyVal {
      // This is the way to unlift the logarithm opaque type
      def toDouble: Double = `this`
      def +(that: Logarithm): Logarithm = math.log(math.exp(`this`) + math.exp(exp))
      def *(that: Logarithm): Logarithm = `this` + that
    }
  }

  def run(): Unit = {
    // HACK: This is required because we haven't yet changed the scope of implicit search
    import Logarithm.LogarithmOps

    // This is to force init of logarithm and check you can compare these types without warnings
    assert(l == 1.0)
    val legitLogarithm = Logarithm(1.0) // this works
    val legitDouble: Double = legitLogarithm.toDouble // this works
    legitLogarithm + Logarithm(2.0) // this works
    legitLogarithm *  Logarithm(3.0) // this works
  }
}
