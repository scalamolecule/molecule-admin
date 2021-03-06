package moleculeadmin.client.app.html.schema
import moleculeadmin.client.app.html.AppElements
import scalatags.JsDom.all._
import org.scalajs.dom.{Element, html}
import org.scalajs.dom.html.{Div, Input, Select}
import scalatags.JsDom
import molecule.ast.model.{Eq, Fn, Value}


trait DefinitionElements extends AppElements {


  def err(idStr: String) = span(color.red, id := idStr).render


}
