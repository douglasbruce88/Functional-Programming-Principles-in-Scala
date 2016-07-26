def xs = 1 :: 2 :: 3 :: Nil

xs.length
xs.last
xs.init
xs take 1
xs drop 1
xs(2)
xs apply 2
xs.reverse
xs updated(2, 4)
xs indexOf 2
xs contains 2
xs ++ xs
xs ::: xs

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

// O(n^2), can do better
def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case y :: ys => y match {
    case List(x) => x :: flatten(ys)
    case z :: zs => z :: flatten(zs) ::: flatten(ys)
    case p => p :: flatten(ys)
  }
}

def ys = List(List(1, 1), List(2), 3)

flatten(ys)

import math.Ordering

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val pair = ("answer", 42)
val (label, value) = pair
val label2 = pair._1
val p2 = scala.Tuple2("answer", 42)

val nums = List(-2, 5, 3, 1, -11)

msort(nums)

xs map (x => x * x)
xs filter (x => x > 2)
xs filterNot (x => x > 2)
xs partition (x => x > 2)
xs takeWhile (x => x > 2)
xs dropWhile (x => x > 2)
xs span (x => x > 2)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (fst, snd) = xs span (e => e == x)
    fst :: pack(snd)
}

val data = List("a", "a", "a", "b", "b", "c", "a")

pack(data)

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

encode(data)

// Blows up on empty lists
xs reduceLeft ((x, y) => x + y)
xs reduceLeft (_ + _)

// Works on empty lists
(xs foldLeft 0) (_ + _)