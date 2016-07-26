// Functions are objects with apply methods.
// There are traits with up to 22 (!) params
trait Function1[A, B] {
  def apply(x: A): B
}

// Anonymous functions
{ class AnonFun extends Function1[Int, Int] {
    def apply(x: Int): Int = x * x
  }

  new AnonFun
}

// Shorthand
// Things with def are not function values
val f = new Function1[Int, Int] {
  def apply(x: Int): Int = x * x
}

object List2 extends {
  def apply(): List = List.empty

  def apply[T](x: T): List[T] = x :: List.empty

  def apply[T](v1: T, v2: T): List[T] = v1 :: v2 :: List.empty
}

def l1 = List2()
List2(1)
List2(2,3)