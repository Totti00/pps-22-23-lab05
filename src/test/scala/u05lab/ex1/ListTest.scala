package u05lab.ex1

import org.junit.Test
import org.junit.Assert.assertEquals

class ListTest {

  val list: List[Int] = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), list.zipRight)

  def testZipRightWithFoldLeft(): Unit =
    assertEquals(list.zipRight, list.zipRightWithFoldLeft)

  @Test
  def testZipRightWithRecursion(): Unit =
    assertEquals(list.zipRight, list.zipRightWithRecursion)

  @Test def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), list.partition(_ % 2 == 0))

  @Test def testPartitionWithFoldLeft(): Unit =
    assertEquals((List(2, 4), List(1, 3)), list.partitionWithFoldLeft(_ % 2 == 0))

  @Test def testPartitionWithRecursion(): Unit =
    assertEquals(list.partition(_ % 2 == 0), list.partitionWithRecursion(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), list.span(_ % 2 != 0))

  @Test def testSpanWithRecursion(): Unit =
    assertEquals(list.span(_ % 2 != 0), list.spanWithRecursion(_ % 2 != 0))

  @Test def testReduce(): Unit =
    assertEquals(10, list.reduce(_ + _))
    assertEquals(10, List(10).reduce(_ + _))

  @Test def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), list.takeRight(3))
    
  @Test def testTakeRightWithRecursion(): Unit =
    assertEquals(list.takeRight(3), list.takeRightWithRecursion(3))

  @Test def testCollect(): Unit =
    assertEquals(List(2, 3, 4, 5), list.collect({ case x if x > 0 => x + 1 }))
}
