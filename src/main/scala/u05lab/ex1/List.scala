package u05lab.ex1

import u05lab.ex1.List

import scala.annotation.tailrec

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse: List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight: List[(A, Int)] =
    this.foldRight((Nil[(A, Int)](), this.length))((elem, acc) => ((elem, acc._2 - 1) :: acc._1, acc._2 - 1))._1

  def zipRightWithFoldLeft: List[(A, Int)] =
    this.foldLeft((Nil[(A, Int)](), 0))((acc, elem) => ((elem, acc._2) :: acc._1, acc._2 + 1))._1.reverse

  def zipRightWithRecursion: List[(A, Int)] =
    def zip(i: Int, list: List[A]): List[(A, Int)] = list match
      case Nil() => Nil()
      case h :: t => (h, i) :: zip(i + 1, t)
    zip(0, this)

  def partition(pred: A => Boolean): (List[A], List[A]) = (filter(pred), filter(!pred(_)))

  def partitionWithFoldLeft(pred: A => Boolean): (List[A], List[A]) =
    this.foldLeft((Nil(), Nil()))((acc, e) => if pred(e) then (acc._1.append(List(e)), acc._2) else (acc._1, acc._2.append(List(e))))

  def partitionWithRecursion(pred: A => Boolean): (List[A], List[A]) =
    @tailrec
    def part(l: List[A], trueList: List[A], falseList: List[A]): (List[A], List[A]) = l match
      case h :: t if pred(h) => part(t, trueList.append(h :: Nil()), falseList)
      case h :: t => part(t, trueList, falseList.append(h :: Nil()))
      case _ => (trueList, falseList)
    part(this, Nil(), Nil())

  def span(pred: A => Boolean): (List[A], List[A]) =
    this.foldRight((Nil[A](), Nil[A]()))((elem, acc) => if pred(elem) then (elem :: acc._1, acc._2) else (Nil(), elem :: acc._1 append acc._2))

  def spanWithRecursion(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: t if pred(h) => val next = t.spanWithRecursion(pred); (h :: next._1, next._2)
    case _ => (Nil(), this)

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw UnsupportedOperationException()
    case h :: t => t.foldLeft(h)(op)

  def takeRightWithRecursion(n: Int): List[A] =
    @tailrec
    def takRight(l: List[A], n: Int): List[A] =l match
      case _ :: t if n > 0 => takRight(t, n - 1)
      case _ => l
    takRight(this, length - n)

  def takeRight(n: Int): List[A] =
    this.foldRight(Nil())((elem, acc) => if acc.length < n then elem :: acc else acc)

  def collect[B](p: PartialFunction[A, B]): List[B] = filter(p.isDefinedAt).map(p.apply)

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
