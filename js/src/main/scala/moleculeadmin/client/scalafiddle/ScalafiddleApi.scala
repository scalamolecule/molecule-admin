package moleculeadmin.client.scalafiddle
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import moleculeadmin.client.scalafiddle.ast._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js


/*
  Scalafiddle router and core must be started first
  in separate processes, each from the ScalaFiddle-core project dir:

  $ sbt
  $ router/reStart

  $ sbt
  $ compilerServer/reStart

  test local compiler server:
  http://localhost:8880/embed?layout=v70&source=%0Aimport%20fiddle.Fiddle%2C%20Fiddle.println%0Aimport%20scalajs.js%0A%0A%40js.annotation.JSExportTopLevel%28%22ScalaFiddle%22%29%0Aobject%20ScalaFiddle%20%7B%0A%20%20val%20greeting%20%3D%20%22Hello%20World%21%22%0A%20%20println%28greeting%29%0A%7D%0A%2F%2F%20%24FiddleDependency%20org.scala-js%20%25%25%25%20scalajs-dom%20%25%200.9.5%0A%2F%2F%20%24FiddleDependency%20com.lihaoyi%20%25%25%25%20scalatags%20%25%200.6.7%0A%2F%2F%20%24FiddleDependency%20com.lihaoyi%20%25%25%25%20sourcecode%20%25%200.1.4%0A
 */

case class ScalafiddleApi[Ret](scalaCode: String) {

  def lambda2: Future[(Any, Any) => Ret] = getLambda[(Any, Any) => Ret]
  def lambda3: Future[(Any, Any, Any) => Ret] = getLambda[(Any, Any, Any) => Ret]
  def lambda4: Future[(Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any) => Ret]
  def lambda5: Future[(Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any) => Ret]
  def lambda6: Future[(Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any) => Ret]
  def lambda7: Future[(Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda8: Future[(Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda9: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda10: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda11: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda12: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda13: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda14: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda15: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda16: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda17: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda18: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda19: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda20: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda21: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]
  def lambda22: Future[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret] = getLambda[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Ret]


  private def getLambda[Lambda]: Future[Lambda] = {

    // post actual source code and get compilation result
    val startTime = System.currentTimeMillis()
    Ajax.post(
      url = "http://localhost:8880/compile?opt=fast",
      data = scalaCode
    ).map { res =>
      val compileTime = System.currentTimeMillis() - startTime
      println(s"Compilation took $compileTime ms -------------------------------")
      //      println(s"Compilation took $compileTime ms")
      val compResponse = readCompilationResponse(res.responseText)
      val jsCode       = compResponse.jsCode.getOrElse("No js code...")

      //      println(s"jsCode --------------\n" + jsCode)
      if (compResponse.annotations.nonEmpty) {
        println(s"----- Scala code: -----")
        println(scalaCode)
        println(s"----- Compile error -----")
        println(compResponse.annotations.head.text.mkString("\n"))
      }

      // Evaluate js code to bring ScalaFiddle into js scope
      js.eval(jsCode)

      // Return js edit lambda
      js.eval("_EditLambda.lambda").asInstanceOf[Lambda]

    } recover {
      case e: dom.ext.AjaxException if e.xhr.status == 400 =>
        val msg = s"Scalafiddle ajax error 400: " + e.xhr.responseText
        println(msg)
        println("Please try again (refresh if testing)" +
          " - this seems to give a valid compiler.")
        //        err(msg)
        //        js.eval(s"window.alert('$msg')")
        throw e

      case e: dom.ext.AjaxException if e.xhr.status == 0 =>
        println(s"Error: " + e.xhr.responseText)
        js.eval("window.alert('Please start Scala fiddle router and server.')")
        throw e

      case e: dom.ext.AjaxException =>
        println(s"Error: ${e.xhr.responseText}")
        println(s"Error: ${e.xhr.responseType}")
        println(s"Error: ${e.xhr.status}")
        println(s"Error: ${e.xhr.statusText}")
        js.eval("window.alert('Scalafiddle ajax error: " + e.xhr.responseText + "')")
        throw e
      case e: Throwable             =>
        println(e.toString)
        js.eval("window.alert('Scalafiddle error - see console for error msgs')")
        throw e
    }
  }

  private def readCompilationResponse(jsonStr: String): CompilationResponse = {
    val r = js.JSON.parse(jsonStr).asInstanceOf[CompilationResponseJS]
    CompilationResponse(
      if (r.jsCode.isEmpty) None else Some(r.jsCode(0)),
      r.jsDeps,
      r.cssDeps,
      r.annotations.map { a =>
        EditorAnnotation(a.row, a.col, a.text, a.tpe)
      },
      r.log
    )
  }
}