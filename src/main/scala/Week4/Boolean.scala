package idealised.Scala

// Scala is pure OO - everything is an object
abstract class Boolean2 {
  def ifThenElse[T](t: => T, e: => T) : T

  def && (x: => Boolean2): Boolean2 = ifThenElse(x, True)
  def || (x: => Boolean2): Boolean2 = ifThenElse(False, x)

  def < (x: => Boolean2): Boolean2 = ifThenElse(False, x)
}

object True extends Boolean2 {
  def ifThenElse[T](t: => T, e: => T): T = t
}
object False extends Boolean2 {
  def ifThenElse[T](t: => T, e: => T): T = e
}

abstract class Int {
  def + (that:Double):Double
}

abstract class Nat {
  def isZero: Boolean
  def predecessor : Nat
  def successor: Nat
  def + (that: Nat):Nat
  def - (that: Nat) : Nat
}

object Zero extends Nat {
  def isZero: Boolean = true

  def successor: Nat = ???

  def +(that: Nat): Nat = ???

  def -(that: Nat): Nat = ???

  def predecessor: Nat = ???
}

class Succ(n : Nat) extends Nat {
  def isZero: Boolean = false

  def successor: Nat = ???

  def +(that: Nat): Nat = ???

  def -(that: Nat): Nat = ???

  def predecessor: Nat = ???
}