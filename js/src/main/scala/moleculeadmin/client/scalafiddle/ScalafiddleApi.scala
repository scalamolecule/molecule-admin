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
  $ sbt router/reStart
  $ sbt compilerServer/reStart

  test local compiler server:
  http://localhost:8880/embed?layout=v70&source=%0Aimport%20fiddle.Fiddle%2C%20Fiddle.println%0Aimport%20scalajs.js%0A%0A%40js.annotation.JSExportTopLevel%28%22ScalaFiddle%22%29%0Aobject%20ScalaFiddle%20%7B%0A%20%20val%20greeting%20%3D%20%22Hello%20World%21%22%0A%20%20println%28greeting%29%0A%7D%0A%2F%2F%20%24FiddleDependency%20org.scala-js%20%25%25%25%20scalajs-dom%20%25%200.9.5%0A%2F%2F%20%24FiddleDependency%20com.lihaoyi%20%25%25%25%20scalatags%20%25%200.6.7%0A%2F%2F%20%24FiddleDependency%20com.lihaoyi%20%25%25%25%20sourcecode%20%25%200.1.4%0A
 */

case class ScalafiddleApi[Ret](card: Int,
                               lhsTypes: String,
                               lhsParamsTypes: String,
                               vars: String,
                               varAssignments: String,
                               lhsParams: String,
                               attrType: String,
                               processType: String,
                               transferType: String,
                               rhs: String,
                              ) {

  def readCompilationResponse(jsonStr: String): CompilationResponse = {
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
    val imports = attrType match {
      case "Date" => "import java.util.Date"
      case "UUID" => "import java.util.UUID"
      case "URI"  => "import java.net.URI"
      case _      => ""
    }

    val scalaCode = card match {
      case 1 =>
        val stringify = attrType match {
          case "Date" => "getTime.toString"
          case _      => "toString"
        }

        // Keeping Option handling within scala boundary
        s"""import scalajs.js.annotation.{JSExportTopLevel, JSExport}
           |$imports
           |@JSExportTopLevel("_EditLambda")
           |object _EditLambda {
           |  $vars
           |  val process: ($lhsTypes) => Option[$processType] = {
           |    ($lhsParamsTypes) => {
           |      $varAssignments
           |      // User rhs code:
           |      $rhs
           |    }
           |  }
           |  @JSExport
           |  val lambda: ($lhsTypes) => String = {
           |    ($lhsParamsTypes) =>
           |      // Poor man's Option
           |      process($lhsParams).fold("__None__")(_.$stringify)
           |  }
           |}""".stripMargin.trim

      case 2 =>
        val processed   = Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")
        val postProcess = if (processed.contains(attrType))
          ".map(_.toString)" else ""

        s"""import scalajs.js
           |import js.annotation.{JSExportTopLevel, JSExport}
           |import js.JSConverters._
           |$imports
           |@JSExportTopLevel("_EditLambda")
           |object _EditLambda {
           |  List("Instantiating a List needed to satisfy the compiler. Why?")
           |  var e: BigInt = BigInt(0)
           |  var floats: js.Array[BigDecimal] = new js.Array()
           |  val process: (String, List[String]) => js.Array[BigDecimal] = {
           |    (_e: String, _floats: List[String]) => {
           |      e = BigInt(_e)
           |      floats = _floats.toJSArray.map(v => BigDecimal(v))
           |      // User rhs code:
           |      floats
           |    }.toJSArray
           |  }
           |  @JSExport
           |  val lambda: (String, List[String]) => js.Array[String] = {
           |    (_e: String, _floats: List[String]) =>
           |      process(_e, _floats).map(_.toString)
           |  }
           |}""".stripMargin


        // Empty Option is simply treated as an empty List.
        // For some weird reason we need to instantiate a List todo
        s"""import scalajs.js
           |import js.annotation.{JSExportTopLevel, JSExport}
           |import js.JSConverters._
           |$imports
           |@JSExportTopLevel("_EditLambda")
           |object _EditLambda {
           |  List("Instantiating a List needed to satisfy the compiler. Why?")
           |  $vars
           |  val process: ($lhsTypes) => js.Array[$processType] = {
           |    ($lhsParamsTypes) => {
           |      $varAssignments
           |      // User rhs code:
           |      $rhs
           |    }.toJSArray
           |  }
           |  @JSExport
           |  val lambda: ($lhsTypes) => js.Array[$transferType] = {
           |    ($lhsParamsTypes) =>
           |      process($lhsParams)$postProcess
           |  }
           |}""".stripMargin

      case 3 =>
        val processed   = Seq("Long", "datom", "ref", "Float", "Double", "BigInt", "BigDecimal")
        val (postProcess, postImplicit) =
          if (processed.contains(attrType)) {
            if (card == 3)
              (".map { case (k, v) => k -> v.toString }",
                s"""
                   |  implicit def wrap2dictString(wrap: js.WrappedDictionary[String]): js.Dictionary[String] =  {
                   |    val result = js.Dictionary.empty[String]
                   |    wrap.foreach { case (key, value) => result(key) = value }
                   |    result
                   |  }""".stripMargin
                )
            else
              (".map(_.toString)", "")
          } else {
            ("", "")
          }

        s"""import scalajs.js
           |import js.annotation.{JSExportTopLevel, JSExport}
           |import js.JSConverters._
           |$imports
           |@JSExportTopLevel("_EditLambda")
           |object _EditLambda {
           |  // JSConverters implicits don't pick up Map and WrappedDictionary
           |  implicit def map2dict(map: Map[String, String]): js.Dictionary[String] =  {
           |    val result = js.Dictionary.empty[String]
           |    map.foreach { case (key, value) => result(key) = value }
           |    result
           |  }
           |  implicit def wrap2dict(wrap: js.WrappedDictionary[String]): js.Dictionary[String] =  {
           |    val result = js.Dictionary.empty[String]
           |    wrap.foreach { case (key, value) => result(key) = value }
           |    result
           |  }
           |  implicit def wrap2dictString(wrap: js.WrappedDictionary[String]): js.Dictionary[String] =  {
           |    val result = js.Dictionary.empty[String]
           |    wrap.foreach { case (key, value) => result(key) = value }
           |    result
           |  }
           |  var e: BigInt = BigInt(0)
           |  var strMap: js.Dictionary[String] = js.Dictionary.empty[String]
           |  val process: (String, Map[String, String]) => js.Dictionary[String] = {
           |    (_e: String, _strMap: Map[String, String]) =>
           |      e = BigInt(_e)
           |      strMap = _strMap.toJSDictionary
           |      // User rhs code:
           |      strMap.map{ case (k, v) => strMap(k) = v + "1"}
           |      strMap
           |      //Map("a" -> "b")
           |  }
           |  @JSExport
           |  val lambda: (String, Map[String, String]) => js.Dictionary[String] = {
           |    (_e: String, _strMap: Map[String, String]) =>
           |      process(_e, _strMap)
           |  }
           |}""".stripMargin

        // Empty Option is simply treated as an empty Map.
        s"""import scalajs.js
           |import js.annotation.{JSExportTopLevel, JSExport}
           |import js.JSConverters._
           |$imports
           |@JSExportTopLevel("_EditLambda")
           |object _EditLambda {
           |  // JSConverters implicits don't pick up Map and WrappedDictionary
           |  implicit def map2dict(map: Map[String, $processType]): js.Dictionary[$processType] =  {
           |    val result = js.Dictionary.empty[$processType]
           |    map.foreach { case (key, value) => result(key) = value }
           |    result
           |  }
           |  implicit def wrap2dict(wrap: js.WrappedDictionary[$processType]): js.Dictionary[$processType] =  {
           |    val result = js.Dictionary.empty[$processType]
           |    wrap.foreach { case (key, value) => result(key) = value }
           |    result
           |  }$postImplicit
           |  $vars
           |  val process: ($lhsTypes) => js.Dictionary[$processType] = {
           |    ($lhsParamsTypes) => {
           |      $varAssignments
           |      // User rhs code:
           |      $rhs
           |    }
           |  }
           |  @JSExport
           |  val lambda: ($lhsTypes) => js.Dictionary[$transferType] = {
           |    ($lhsParamsTypes) =>
           |      process($lhsParams)$postProcess
           |  }
           |}""".stripMargin
    }

//    println(scalaCode)

    // post actual source code and get compilation result
    val startTime = System.currentTimeMillis()
    Ajax.post(
      url = s"http://localhost:8880/compile?opt=fast",
      data = scalaCode
    ).map { res =>
      val compileTime = (System.currentTimeMillis() - startTime)
      println(s"Compilation took $compileTime ms")
      val compilationResponse = readCompilationResponse(res.responseText)
      val jsCode              = compilationResponse.jsCode.getOrElse("No js code...")

      //      println(s"jsCode --------------\n" + jsCode)
      if (compilationResponse.annotations.nonEmpty)
        println(s"----- Compile error -----\n" +
          compilationResponse.annotations.head.text.mkString("\n"))

      // Bring ScalaFiddle into scope
      js.eval(jsCode)

      // Return js edit lambda
      js.eval("_EditLambda.lambda").asInstanceOf[Lambda]


    } recover {
      case e: dom.ext.AjaxException if e.xhr.status == 400 =>
        println(s"Debug: " + e.xhr.responseText)
        throw e

      case e: dom.ext.AjaxException =>
        println(s"Error: ${e.xhr.responseText}")
        println(s"Error: ${e.xhr.responseType}")
        println(s"Error: ${e.xhr.status}")
        println(s"Error: ${e.xhr.statusText}")
        throw e
      case e: Throwable             =>
        println(e.toString)
        throw e
    }
  }
}