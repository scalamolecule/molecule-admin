package inspiration

import org.scalajs.dom.{DOMList, Node}
import scala.reflect.ClassTag

class Aggregator[@specialized(Int, Long) T: ClassTag](initialSize: Int = 1) {
  protected[this] var data    = new Array[T](initialSize)
  protected[this] var length0 = 0

  def length: Int = length0

  def apply(i: Int): T = data(i)

  def append(i: T): Unit = {
    if (length >= data.length) {
      val newData = new Array[T](data.length * 3 / 2 + 1)
      System.arraycopy(data, 0, newData, 0, length)
      data = newData
    }
    data(length) = i
    length0 += 1
  }
}

class Aggregator2[T: ClassTag](initialSize: Int = 1) {
  protected[this] var data    = new Array[T](initialSize)
  protected[this] var length0 = 0

  def length: Int = length0

  def apply(i: Int): T = data(i)

  def append(i: T): Unit = {
    if (length >= data.length) {
      val newData = new Array[T](data.length * 3 / 2 + 1)
      System.arraycopy(data, 0, newData, 0, length)
      data = newData
    }
    data(length) = i
    length0 += 1
  }
}

class LongAggregator(initialSize: Int = 1) {
  protected[this] var data    = new Array[Long](initialSize)
  private         var length0 = 0

  def length: Int = length0

  def apply(i: Int): Long = data(i)

  def append(i: Long): Unit = {
    val newData = new Array[Long](length0 + 1)
    System.arraycopy(data, 0, newData, 0, length0)
    data = newData
    data(length0) = i
    length0 += 1
  }

  def append(ii: Iterable[Long]): Unit = {
    val count   = ii.size
    val newData = new Array[Long](length0 + count)
    System.arraycopy(data, 0, newData, 0, length0)
    data = newData
    val it = ii.iterator
    var i  = 0
    while (it.hasNext) {
      data(i) = it.next()
      i += 1
    }
    length0 += count
  }
}

//object test {
//
//  val a = new Aggregator[Int](5)
//
//  a.append(2)
//  a(2) == 2
//}
