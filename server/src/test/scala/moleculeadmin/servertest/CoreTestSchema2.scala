/*
* AUTO-GENERATED Molecule DSL schema boilerplate code
*
* To change:
* 1. edit schema definition file in `db.core.schema/`
* 2. `sbt compile` in terminal
* 3. Refresh and re-compile project in IDE
*/
package db.core.schema
import molecule.schema.SchemaTransaction
import datomic.{Util, Peer}

object CoreTestSchema2 extends SchemaTransaction {

  lazy val partitions = Util.list()


  lazy val namespaces = Util.list(

    Util.map(
      ":db/ident"             , ":Ns/int",
      ":db/valueType"         , ":db.type/long",
      ":db/cardinality"       , ":db.cardinality/one",
      ":db/doc"               , "Card one Int attribute",
      ":db/index"             , true.asInstanceOf[Object]),

    Util.map(":db/ident"             , ":Ns/long",
      ":db/valueType"         , ":db.type/long",
      ":db/cardinality"       , ":db.cardinality/one",
      ":db/index"             , true.asInstanceOf[Object]),

  )
}