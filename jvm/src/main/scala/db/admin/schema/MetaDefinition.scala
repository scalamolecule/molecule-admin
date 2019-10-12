package db.admin.schema
import molecule.schema.definition._

@InOut(0, 22)
object MetaDefinition {

  object meta {

    trait Db {
      val name        = oneString.uniqueValue.doc("Database name")
      val isMolecular = oneBoolean.noHistory.doc("Marker for non-molecule db without definition file")

      val codeRoot = oneString.noHistory.doc("Full path to root of source code files where db is used")

      val defFilePath = oneString.noHistory.doc("Full path to definition file for database")
      val pkg         = oneString.noHistory.doc("Package of definition file")
      val inputArity  = oneInt.noHistory
      val outputArity = oneInt.noHistory

      val partitions = many[Partition].isComponent.doc("Database owns Partitions")
    }

    object Partition extends Partition
    trait Partition {
      val pos         = oneInt.doc("Position index starting at 1")
      val name        = oneString.doc("Partition name. If partitions are not used a single meta partition with empty name is used.")
      val descr       = oneString.doc("Human short description of partition (used as comment above part in def file)")
      val namespaces  = many[Namespace].isComponent.doc("Partition owns Namespaces")
      val entityCount = oneInt.doc("Latest count of entities with asserted attributes of this partition")
      val molecules   = manyBi[stats.Molecule.partitions.type].noHistory.doc("Molecules in code using this partition")
    }

    object Namespace extends Namespace
    trait Namespace {
      val pos         = oneInt.doc("Position index starting at 1")
      val name        = oneString.doc("Capitalized namespace name without partition prefix ('Person')")
      val nameFull    = oneString.doc("Non-capitalized namespace name with/without partition prefix ('person' or 'community_Person')")
      val descr       = oneString.doc("Short description of namespace (used as comment above ns in def file)")
      val entityCount = oneInt.doc("Latest count of entities with asserted attributes of this namespace")
      val attrs       = many[Attribute].isComponent.doc("Attribute owns Attributes")
      val molecules   = manyBi[stats.Molecule.namespaces.type].noHistory.doc("Molecules in code using this namespace")
    }

    object Attribute extends Attribute
    trait Attribute {
      val pos       = oneInt.doc("Position index starting at 1")
      val attrGroup = oneString.doc("Optional comment before attribute. Empty string creates an empty line before.")

      val name    = oneString.doc("Attribute (or ref) name")
      val card    = oneInt.doc("Cardinality. 1: one, 2: many, 3: map")
      val tpe     = oneEnum("String", "Int", "Long", "Float", "Double", "Boolean", "BigInt", "BigDecimal", "Date", "UUID", "URI", "Byte", "Enum", "ref", "bi", "biEdge", "target").doc("basic types and references")
      val enums   = manyString.doc("Enum values")
      val refNs   = oneString.doc("Referenced namespace of ref/bi/biEdge/target")
      val options = manyEnum("indexed", "noHistory", "uniqueValue", "uniqueIdentity", "isComponent", "fulltext")
      val doc     = oneString.doc("Doc text")

      // Value stats
      val entityCount        = oneInt.noHistory.doc("Latest count of entities with this attribute asserted")
      val distinctValueCount = oneInt.noHistory.doc("Latest count of distinct values")
      val descrAttr          = oneString.noHistory.doc("For ref attributes only: attr in referenced ns describing that ns")
      val topValues          = many[stats.TopValue].isComponent.noHistory.doc("Latest Top 25 values (as strings)")

      // Code stats
      val molecules = manyBi[stats.Molecule.attributes.type].noHistory.doc("Molecules in code using this attribute")
    }
  }


  object stats {
    trait TopValue {
      val entityCount = oneInt.noHistory.doc("Count of entities with this value asserted")
      val value       = oneString.noHistory.doc("Value as string")
      val label       = oneString.noHistory.doc("Presentation label")
    }

    object Molecule extends Molecule
    trait Molecule {
      val file = oneString.noHistory.doc("Full path to source code file")

      val lineStart     = oneInt.noHistory.doc("Line number of line before molecule")
      val lineBefore    = oneString.noHistory.doc("Line before molecule to give context")
      val moleculeLines = manyString.noHistory.doc("One or more lines that the molecule spans over")
      val lineAfter     = oneString.noHistory.doc("Line after molecule to give context")
      val lineEnd       = oneInt.noHistory.doc("Line number of line after molecule")
      val clean         = oneString.noHistory.doc("Molecule in 1 line")

      val db                 = one[meta.Db].noHistory.doc("Database of this molecule")
      val partitions: AnyRef = manyBi[meta.Partition.molecules.type].noHistory.doc("Partitions of this molecule")
      val namespaces: AnyRef = manyBi[meta.Namespace.molecules.type].noHistory.doc("Namespaces of this molecule")
      val attributes: AnyRef = manyBi[meta.Attribute.molecules.type].noHistory.doc("Attributes of this molecule")
    }
  }


  object user {
    trait User {
      val uuid       = oneUUID
      val username   = oneString
      val password   = oneString
      val snippets   = manyString.noHistory.doc("Open snippets")
      val dbSettings = many[DbSettings].noHistory.isComponent.doc("Multiple settings for multiple databases")
    }
    trait GlobalSettings {
      val maxRows      = oneInt.noHistory.doc("Maximum number of rows retrieved from db. Default: -1 (all)")
      val limit        = oneInt.noHistory.doc("Rows per page. Default: 20")
      val showSnippets = manyString.noHistory.doc("Snippets shown. Default: none")
    }
    trait DbSettings {
      val pos       = oneInt.noHistory.doc("Ordering position")
      val name      = oneString.noHistory.doc("Optional name for this set of favorites")
      val db        = one[meta.Db].noHistory.doc("Database")
      val favorites = many[Favorite].isComponent.noHistory.doc("Saved molecules for this database/set")
    }
    trait Favorite {
      val pos         = oneInt.noHistory.doc("Ordering position")
      val name        = oneString.noHistory.doc("Optional user-friendly name for molecule query")
      val molecule    = oneString.noHistory.doc("Molecule as text string. Model, cache etc will be derived from this.")
      val colSettings = many[ColSetting].noHistory.isComponent.doc("")
    }
    trait ColSetting {
      val index    = oneInt.noHistory.doc("Column index")
      val attrExpr = oneString.noHistory.doc("Attribute expression to be rendered in column header")
      val sortDir  = oneString.noHistory.doc("asc/desc or empty string if not sorted")
      val sortPos  = oneInt.noHistory.doc("If multiple sort columns, an index from 1 to max 5")
      val filters  = many[Filter].noHistory.doc("Multiple filters to this query")
    }
    trait Filter {
      val pos        = oneInt.noHistory.doc("Ordering position")
      val name       = oneString.noHistory.doc("Optional user-friendly name for filter")
      val filterExpr = oneString.noHistory.doc("Filter expression. Can be multiline.")
      val jsCode     = oneString.noHistory.doc("Compiled js code with `fn`. `eval` this to get cached `fn`.")
    }
  }
}
