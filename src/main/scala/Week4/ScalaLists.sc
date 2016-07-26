
// Cons associates to the right (any operator ending in a colon does)
// e.g. numbers will call Nil.::(4).::(3).::(2).::(1)
def fruit = "apples" :: "oranges" :: "pears" :: Nil
def numbers = 1 :: 2 :: 3 :: 4 :: Nil
def empty = Nil

// We can defined all operators in terms of head, tail, isEmpty
fruit.head
fruit.tail.head
fruit.isEmpty

// Can pattern match on List(p1,...,pn)
def pat(e : List[Int]) = e match {
  case 1 :: 2 :: xs => "list starting with 1 and 2"
  case x :: Nil => "list of length 1"
  case List(x) => "same as x::Nil"
  case List() => "Nil"
  //case List (2 :: xs) => "list within a list"
}

def insert[T <% Ordered[T]](x: T, xs: List[T]): List[T] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: y :: ys else y :: insert(x, ys)
}

def isort[T <% Ordered[T]](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

isort(2 :: 1 :: Nil)