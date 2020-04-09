package moleculeadmin.client

import moleculeadmin.shared.api.{DbsApi, QueryApi, SchemaApi}
import util.client.autowire.{AutowireAjax, AutowireWebSocket}


object dbsWire extends AutowireAjax[DbsApi]("dbs")

object schemaWire extends AutowireAjax[SchemaApi]("schema")

object queryWireAjax extends AutowireAjax[QueryApi]("query")
//object queryWireWS extends AutowireWebSocket[QueryApi]("query")

