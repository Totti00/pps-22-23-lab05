package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

private object PerformanceBenchmarks:
  import PerformanceUtils.*
  val testValues = (1 to 10000000)

  def benchLinearISeq() =
    println("< List (immutable) >")
    val l = testValues.toList
    val lazyList = LazyList.from(testValues)
    measure("creation: ")(testValues.toList)
    measure("size: ")(l.size)
    measure("get tail list: ")(l.tail)
    measure("append: ")(l :+ 0)
    measure("update: ")(l updated(1, 0))

    println("< LazyList (immutable) >")
    measure("creation: ")(LazyList.from(testValues))
    measure("size: ")(lazyList.size)
    measure("get tail list: ")(lazyList.tail)
    measure("append: ")(lazyList :+ 0)
    measure("update: ")(lazyList updated(1, 0))

  def benchLinearMSeq() =
    import scala.collection.mutable.ListBuffer
    println("< ListBuffer (mutable) >")
    val lBuffer = ListBuffer.from(testValues)
    measure("creation: ")(ListBuffer.from(testValues))
    measure("size: ")(lBuffer.size)
    measure("get tail list: ")(lBuffer.tail)
    measure("append: ")(lBuffer += 0)
    measure("remove: ")(lBuffer -= 0)
    measure("update: ")(lBuffer(1) = 0)

  def benchIndexISeq() =
    println("< Vector (immutable) >")
    val lVector = testValues.toVector
    val lArray = testValues.toArray
    measure("creation: ")(testValues.toVector)
    measure("size: ")(lVector.size)
    measure("get tail list: ")(lVector.tail)
    measure("append: ")(lVector :+ 0)
    measure("update: ")(lVector updated (1,0))

    println("< Array (immutable) >")
    measure("creation: ")(testValues.toArray)
    measure("size: ")(lArray.length)
    measure("get tail list: ")(lArray.tail)
    measure("append: ")(lArray :+ 0)
    measure("update: ")(lArray updated(1, 0))

  def benchIndexMSeq() =
    import scala.collection.mutable.ArrayBuffer
    println("< ArrayBuffer (mutable) >")
    val lArrayBuff = ArrayBuffer.from(testValues)
    measure("creation: ")(ArrayBuffer.from(testValues))
    measure("size")(lArrayBuff.size)
    measure("get tail list: ")(lArrayBuff.tail)
    measure("append: ")(lArrayBuff += 0)
    measure("remove: ")(lArrayBuff -= 0)
    measure("update: ")(lArrayBuff(1) = 0)

  def benchISet() =
    println("< Set (immutable) >")
    val s = testValues.toSet
    measure("creation: ")(testValues.toSet)
    measure("size")(s.size)
    measure("get tail list: ")(s.tail)
    measure("append: ")(s + 0)
    measure("remove: ")(s - 0)

  def benchMSet() =
    import scala.collection.mutable.Set
    println("< Set (mutable) >")
    val sMutable = mutable.Set.from(testValues)
    measure("creation: ")(mutable.Set.from(testValues))
    measure("size")(sMutable.size)
    measure("get tail list: ")(sMutable.tail)
    measure("append: ")(sMutable += 0)
    measure("remove: ")(sMutable -= 0)

  def benchIMap() =
    println("< Map (immutable) >")
    val m = testValues.map(x => (x, x)).toMap
    measure("creation: ")(testValues.map(x => (x, x)).toMap)
    measure("size")(m.size)
    measure("get: ")(m(100))
    measure("append: ")(m + (0 -> 0))
    measure("remove: ")(m - 1)

  def benchMMap() =
    import scala.collection.mutable.Map
    println("< Map (mutable) >")
    val mMutable = mutable.Map.from(testValues.map(x => (x, x)))
    measure("creation: ")(mutable.Map.from(testValues.map(x => (x, x))))
    measure("size")(mMutable.size)
    measure("get: ")(mMutable(100))
    measure("append: ")(mMutable += (0 -> 0))
    measure("remove: ")(mMutable -= 1)

@main def checkPerformance(): Unit =

  import PerformanceBenchmarks.*
  /* Linear sequences: List, ListBuffer */
  benchLinearISeq()
  benchLinearMSeq()

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  benchIndexISeq()
  benchIndexMSeq()

  /* Sets */
  benchISet()
  benchMSet()

  /* Maps */
  benchIMap()
  benchMMap()

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))
