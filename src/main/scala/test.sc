import MonadicTransformer._

val a = Some(List(1)): Option[List[Int]]
val b = Some(List(2)): Option[List[Int]]

OptionL(a).map(_+1)

val la = List(Option(1), Option(5))
val lb = List(Option(2), None)

OptionT(la).map(_+1)
OptionT(lb).flatMap(t => OptionT(List(Option(t+2))))

val res = for {
  a <- OptionT(la)
  b <- OptionT(lb)
} yield a + b

res


//ListT(a).map(_+1)
//
////ListT(a).flatMap(b)
//
//for {
//  r1 <- ListT(a)
//  r2 <- ListT(b)
//} yield (r1 + r2)
