package moleculeadmin.client.scalafiddle

import java.util.UUID
import scala.scalajs.js

object ast {

  sealed trait CompilerMessage

  case object CompilerReady extends CompilerMessage

  case object Ping extends CompilerMessage

  case object Pong extends CompilerMessage

  sealed abstract class CompilerRequest {
    def id: String
    def source: String
    def clientAddress: String
    def updated(f: String => String): CompilerRequest
  }

  case class CompilationRequest(id: String, source: String, clientAddress: String, opt: String)
    extends CompilerRequest
      with CompilerMessage {
    def updated(f: String => String) = copy(source = f(source))
  }

  case class CompletionRequest(id: String, source: String, clientAddress: String, offset: Int)
    extends CompilerRequest
      with CompilerMessage {
    def updated(f: String => String) = copy(source = f(source))
  }

  trait CompilerResponse

  case class EditorAnnotation(row: Int, col: Int, text: Seq[String], tpe: String)

  case class CompilationResponse(
                                  jsCode: Option[String],
                                  jsDeps: Seq[String],
                                  cssDeps: Seq[String],
                                  annotations: Seq[EditorAnnotation],
                                  log: String
                                ) extends CompilerResponse
    with CompilerMessage

  case class CompletionResponse(completions: List[(String, String)]) extends CompilerResponse with CompilerMessage

  case class SourceFile(name: String,
                        code: String,
                        prefix: List[String] = Nil,
                        postfix: List[String] = Nil,
                        indent: Int = 0,
                        fiddleId: Option[String] = None,
                        id: String = UUID.randomUUID().toString)

  @js.native
  trait EditorAnnotationJS extends js.Object {
    val row : Int
    val col : Int
    val text: js.Array[String]
    val tpe : String
  }

  @js.native
  trait CompilationResponseJS extends js.Object {
    val jsCode     : js.Array[String]
    val jsDeps     : js.Array[String]
    val cssDeps    : js.Array[String]
    val annotations: js.Array[EditorAnnotationJS]
    val log        : String
  }

  @js.native
  trait _EditLambda extends js.Object {
    val yyy: js.Function2[Long, Int, String]
  }
}