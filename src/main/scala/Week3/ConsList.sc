import java.util.NoSuchElementException

trait MList[T] {
  def isEmpty: Boolean

  def head: T

  def tail: MList[T]
}

class Cons[T](val head: T, val tail: MList[T]) extends MList[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends MList[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def nth[T](n: Int,input: MList[T]) = {
  def take(toGo: Int,ls: MList[T]): T = {
    if(ls.isEmpty) throw new IndexOutOfBoundsException
    else if (toGo == 0) ls.head
    else take(toGo - 1,ls.tail)
  }

  take(n,input)
}

val myList = new Cons(2, new Nil)

nth(0,myList)
nth(1,myList)
nth(-1,myList)