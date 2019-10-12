package autowireImpl

import java.nio.ByteBuffer
import boopickle.Default._

object AutoWireByteServer extends autowire.Server[ByteBuffer, Pickler, Pickler] {
  override def read[R: Pickler](p: ByteBuffer) = Unpickle.apply[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}