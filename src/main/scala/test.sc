import MonadicTransformer._

val a = Some(List(1)): Option[List[Int]]
val b = Some(List(2)): Option[List[Int]]

OptionL(a).map(_+1)

//ListT(a).map(_+1)
//
////ListT(a).flatMap(b)
//
//for {
//  r1 <- ListT(a)
//  r2 <- ListT(b)
//} yield (r1 + r2)
