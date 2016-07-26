package Week4

// Adding the covariance here allows the assignment of 'x' below to type check
trait List[+T] {
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
  // We can fix variance using a lower bound. This is to stop things like
  // xs.Prepend(Empty) where xs is a List[NonEmpty]
  def prepend [U >: T] (elem : U): List[U] = new Cons(elem, this)
}

class Cons[T](val head : T, val tail : List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean  = true
  def head : Nothing = throw new NoSuchElementException("Nil.head")
  def tail : Nothing = throw new NoSuchElementException("Nil.tail")
}

object Test {
  def f(xs: List[NonEmpty], x: Empty): List[IntSet] = xs prepend x
  val g = new Empty
  // Type casts are possible but generally discouraged and unsafe
  def b = g.isInstanceOf[IntSet]
  def as = g.asInstanceOf[IntSet]
}

// Two differences between lists and arrays:
// - lists are immutable and recursive, arrays are flat.

object scalaLists{
  val fruit = "apples" :: ("oranges" :: "pears" :: Nil)
}
