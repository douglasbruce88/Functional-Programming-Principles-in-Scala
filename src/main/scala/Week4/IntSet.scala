package Week4

abstract class IntSet {
  def inc(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

// object defines a singleton
class Empty extends IntSet {
  override def toString = "."

  def contains(x: Int): Boolean = false

  def inc(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def toString = "{" + left + elem + right + "}"

  def inc(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left inc x, right)
    else if (x > elem) new NonEmpty(elem, left, right inc x)
    else this

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  // Every recursive call is on a smaller set than before, thus it terminates
  def union(other: IntSet): IntSet = ((left union right) union other) inc elem
}

// <: is an UPPER BOUND. This means in practise that:
// if we input Empty we get Empty back;
// if we input NonEmpty we get NonEmpty back.
//def assertAllPos[S <: IntSet](r: S): S

// S <: T generally means S is a subtype of T, vice verse for >:

// If we wrote S>: NonEmpty S could be NonEmpty, IntSet, AnyRef or Any
// We can mix e.g. S >: NonEmpty <: IntSet

// WIth variance
// - is contravariant so it is compatible with passing in a supertype
// + is covariant so we can return a subtype of the declared return type
trait Function1[-A, +B] {
  def apply(x: A): B
}

// + can only appear in method results ('out' e.g. IEnumerable)
// - can only appear in method parameters ('in' e.g. IComparable)
class Array[+T]
{
  //def update(x : T)
}
