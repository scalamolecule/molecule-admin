package moleculeadmin.server.utils


import play.api.http.{ContentTypeOf, ContentTypes, Writeable}
import play.api.mvc.Codec

import scalatags.Text.all.Tag

/*
* Allow Play Actions to receive Scalatags like this:
 *
 * Ok(someScalaTags)
 *
 * instead of
 *
 * Ok(someScalaTags.render).as("text/html")
 *
 * Let your controller extend this class
 * */

trait HtmlTags {

  implicit def contentTypeOfTag(implicit codec: Codec): ContentTypeOf[Tag] = {
    ContentTypeOf[Tag](Some(ContentTypes.HTML))
  }

  implicit def writeableOfTag(implicit codec: Codec): Writeable[Tag] = {
    Writeable(tag => codec.encode("<!DOCTYPE html>\n" + tag.render))
  }
}