// Peano numbers
abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true

  def +(that: Nat) = that

  def -(that: Nat) = if (that.isZero) this else throw new Error("0 minus Succ")

  def predecessor = throw new Error("0.predecessor")
}

class Succ(n: Nat) extends Nat {
  def isZero = false

  def +(that: Nat) = n + that.successor

  def -(that: Nat) = if (that.isZero) this else n - that.predecessor

  def predecessor = n
}

def z = Zero

z.isZero

def one = new Succ(Zero)

def shouldBeTwo = one + one

def two = new Succ(one)

def newZ = two - shouldBeTwo

newZ.isZero