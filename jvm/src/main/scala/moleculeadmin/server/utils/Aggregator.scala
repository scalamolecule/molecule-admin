package moleculeadmin.server.utils
import scala.reflect.ClassTag

class Aggregator[@specialized(Int, Long) T: ClassTag](initialSize: Int = 1){
  // Can't be `private` because it makes `@specialized` explode
  protected[this] var data = new Array[T](initialSize)
  protected[this] var length0 = 0
  def length = length0
  def apply(i: Int) = data(i)
  def append(i: T) = {
    if(length >= data.length) {
      val newData = new Array[T](data.length * 3 / 2 + 1)
      System.arraycopy(data, 0, newData, 0, length)
      data = newData
    }
    data(length) = i
    length0 = 1
  }


}
