import scala.language.higherKinds



case class OptionL[A](e: Option[List[A]]) {
  def map[B](f: A => B): OptionL[B] = OptionL(e map(_.map(f)))

  def flatMap[B](f: A => OptionL[B]): OptionL[B] =
    OptionL {
      e map {
        _.flatMap(a => f(a).e.getOrElse(Nil))
      }
    }
}



trait MonadicTransformer[T[_]] {
  def flatMap[A,B](monad: T[A], f: A => T[B]): T[B]
  def map[A,B](monad: T[A], f: A => B): T[B]
  def pure[A](a: A): T[A]
}
object MonadicTransformer {

  implicit object ListMonadicTransformer extends MonadicTransformer[List] {
    override def flatMap[A,B](list: List[A], f: A => List[B]): List[B] = list.flatMap(f)
    override def map[A, B](list: List[A], f: (A) => B): List[B] = list.map(f)
    override def pure[A](a: A): List[A] = List(a)
  }

  implicit object OptionMonadicTransformer extends MonadicTransformer[Option] {
    override def flatMap[A, B](option: Option[A], f: (A) => Option[B]): Option[B] = option.flatMap(f)
    override def map[A, B](option: Option[A], f: (A) => B): Option[B] = option.map(f)
    override def pure[A](a: A): Option[A] = Some(a)
  }

}

case class OptionT[A, M[_]](m: M[Option[A]])(implicit mt: MonadicTransformer[M]) {

  def flatMap[B](f: A => OptionT[B, M]): OptionT[B, M] = {
    val f1: (Option[A]) => M[Option[B]] = _.map(a => f(a).m).getOrElse(mt.pure(None))
    val map1: M[Option[B]] = mt.flatMap(m, f1)
    OptionT.pure(map1)
  }

  def map[B](f: A => B): OptionT[B, M] = {
    def f1(a:Option[A]): Option[B] = a.map(f)
    val res: M[Option[B]] = mt.map(m, f1)
    OptionT.pure(res)
  }
}

object OptionT {
  def pure[A, M[_]](m: M[Option[A]])(implicit mt: MonadicTransformer[M]): OptionT[A, M] = {
    new OptionT(m)
  }
}


//case class ListT[A, M[_]](m: M[List[A]])(implicit mt: MonadicTransformer[M]) {
//  def flatMap[B](f: A => ListT[B, M]): ListT[B, M] = {
//    def f1(t: A): M[List[B]] = f(t).m
//    def f2(t: List[A]): M[List[B]] = t.map(f1)
//    ListT.pure(mt.flatMap(m, f2))
//  }
//
//  def map[B](f: A => B): ListT[B, M] = {
//    def fa: List[A] => List[B] = _.map(f)
//    ListT.pure(mt.map(m, fa))
//  }
//}
//
//object ListT {
//  def pure[A, M[_]](m: M[List[A]])(implicit mt: MonadicTransformer[M]): ListT[A, M] = {
//    new ListT(m)
//  }
//}


