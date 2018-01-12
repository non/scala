import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.ClassTag

package object tagging {

  // Tagged[S, T] means that S is tagged with T
  opaque type Tagged[S, T] = S

  object Tagged {

    // HACK: Should be implicitly added by namer in the future
    private implicit def s2Tag[S, T](s: S): Tagged[S, T] = s.asInstanceOf[Tagged[S, T]]
    private implicit def tag2S[S, T](t: Tagged[S, T]): S = t.asInstanceOf[S]
    private implicit def fs2FTag[F[_], S, T](fs: F[S]): F[Tagged[S, T]] = fs.asInstanceOf[F[Tagged[S, T]]]
    private implicit def ftag2FS[F[_], S, T](ft: F[Tagged[S, T]]): F[S] = ft.asInstanceOf[F[S]]
    
    def tag[S, T](s: S): Tagged[S, T] = s
    def untag[S, T](st: Tagged[S, T]): S = st
  
    def tags[F[_], S, T](fs: F[S]): F[Tagged[S, T]] = fs
    def untags[F[_], S, T](fst: F[Tagged[S, T]]): F[S] = fst
  
    implicit def taggedClassTag[S, T](implicit ct: ClassTag[S]): ClassTag[Tagged[S, T]] =
      ct
  }

  type @@[S, T] = Tagged[S, T]
  
  implicit class UntagOps[S, T](val st: S @@ T) extends AnyVal {
    def untag: S = Tagged.untag(st)
  }
  
  implicit class UntagsOps[F[_], S, T](val fs: F[S @@ T]) extends AnyVal {
    def untags: F[S] = Tagged.untags(fs)
  }
  
  implicit class TagOps[S](val s: S) extends AnyVal {
    def tag[T]: S @@ T = Tagged.tag(s)
  }
  
  implicit class TagsOps[F[_], S](val fs: F[S]) extends AnyVal {
    def tags[T]: F[S @@ T] = Tagged.tags(fs)
  }

  def run(): Unit = {
    trait Meter
    trait Foot
    trait Fathom
    
    val x: Double @@ Meter = (1e7).tag[Meter]
    val y: Double @@ Foot = (123.0).tag[Foot]
    val xs: Array[Double @@ Meter] = Array(1.0, 2.0, 3.0).tags[Meter]

    // val o: Ordering[Double] = implicitly
    // val om: Ordering[Double @@ Meter] = o.tags[Meter]
    // om.compare(x, x) // 0
    // //om.compare(x, y) // does not compile
    // xs.min(om) // 1.0
    // 
    // //xs.min(o) // does not compile
    // 
    // // uses ClassTag[Double] via 'Tagged.taggedClassTag'.
    // val ys = new Array[Double @@ Foot](20)
  }
}
