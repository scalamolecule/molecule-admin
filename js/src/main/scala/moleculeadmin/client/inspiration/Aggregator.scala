package moleculeadmin.client.inspiration

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

object test {

  val a = new Aggregator[Int](5)

//  a(2) = 2
}
