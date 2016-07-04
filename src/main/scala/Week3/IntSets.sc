// These are purely functional, persistent data structures
abstract class IntSet {
  def inc(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

// object defines a singleton
object Empty extends IntSet {
  override def toString = "."

  def contains(x: Int): Boolean = false

  def inc(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

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
  def union (other: IntSet): IntSet = ((left union right) union other) inc elem
}

val t1 = new NonEmpty(3, Empty, Empty)

val t2 = t1 inc 4

val t3 = t1 union t2

trait Planar {
  def height : Int
  def width : Int
  def surface = height * width
}

// Subtype of scala.AnyRef, which is a subtype of Any
abstract class Shape

class Square extends Shape with Planar {
  def height: Int = 2

  // Int subtype of scala.AnyVal.
  // Can convert from Byte to Short to Int to Float etc
  def width: Int = 2
}

// Also have Nothing which is an abstract subtype of every type.
// It signals abnormal termination or an empty collection
//def x = new Nothing doesn't work s its abstract
val x = null // grrrr
val y : String =  x
//val z : Int = null // Doesn't work

def p = if (true) 1 else false