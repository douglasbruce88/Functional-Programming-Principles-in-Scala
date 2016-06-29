def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def fact(n: Int) = product(x => x)(1, n)

def foldN(folder: Int => Int => Int)(f: Int => Int)(initialState: Int)(a: Int, b: Int): Int =
  if (a > b) initialState else folder(f(a))(foldN(folder)(f)(initialState)(a + 1, b))

sum(x => x)(1, 5)
sum(x => x * x)(1, 4)

product(x => x)(1, 4)
product(x => x * x)(1, 4)

fact(5)

def add(x: Int)(y: Int) = x + y
foldN(add)(x => x)(0)(1, 4)

val tolerance = 1e-6
def isCloseEnough(x: Double, y:Double) =
  math.abs((x-y)/x) / x < tolerance

def fixedPoint(f:Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    println(next)
    if(isCloseEnough(guess, next)) next else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f:Double => Double)(x: Double) = (x + f(x))/2

fixedPoint(x => 1 + x/2)(1)

def sqrt (x : Double) = fixedPoint(averageDamp(p => x/p))(1)
def sqrt2 = sqrt(2)
assert(sqrt2 > 0)

class Rational(x : Int, y : Int)
{
  require(y > 0, "denom must be positive")

  def numer = x
  def denom = y

  def this(x: Int) = this(x, 1)

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  override def toString = numer + "/" + denom

  private def gcd(a: Int, b: Int) : Int = if (b == 0) a else gcd (b, a % b)

  def double = this + this

  def unary_- = this

  def - (that:Rational) = this + -that
}

val x = new Rational(2, 3)

x.numer

val y = new Rational(3, 4)

x + y

x - y

x.double

val z = new Rational(5)