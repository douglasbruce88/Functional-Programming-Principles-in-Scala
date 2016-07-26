
// To decompose things (i.e. to reverse the construction process of the object
// and ask which subclass & params were used) we have looked at various options:
//
// - Classification & access methods: quadratic explosion
// - Type casting: unsafe, low-level
// - OO decomposition: doesn't always work; need to touch all classes to add a new method
//
// Instead, pattern matching automates this using case classes. Looking at binary expressions,
// if we want to add more methods then pattern matching is good, if we want to add more classes
// then OO is good. This is knows as the 'Expression Problem'

trait Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(name: String) extends Expr

// variables need to start with a low case letter to distinguish from constants
// x match ... rather than match x with ... in F#.
def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Prod(e1, e2) => show(e1) + " * " + show(e2)
  case Var(name) => name
}