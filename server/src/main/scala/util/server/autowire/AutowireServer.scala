package util.server.autowire

import java.nio.ByteBuffer
import boopickle.Default._
import util.shared.autowire.AutowireSerializers

object AutowireServer
  extends autowire.Server[ByteBuffer, Pickler, Pickler]
  with AutowireSerializers