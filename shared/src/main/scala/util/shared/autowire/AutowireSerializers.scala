package util.shared.autowire

import java.nio.ByteBuffer
import boopickle.Default.{Pickle, Pickler, Unpickle}


trait AutowireSerializers
  extends autowire.Serializers[ByteBuffer, Pickler, Pickler] {

  override def read[Result: Pickler](p: ByteBuffer): Result =
    Unpickle.apply[Result].fromBytes(p)

  override def write[Result: Pickler](r: Result): ByteBuffer =
    Pickle.intoBytes(r)
}
