package db.admin.schema
import molecule.schema.definition._

@InOut(0, 22)
object MoleculeAdminDefinition {

  object meta {
    trait Db {
      val name        = oneString.uniqueValue.doc("Database name")
      val isMolecular = oneBoolean.noHistory.doc("Marker for non-molecule db without definition file")
      val codeRoot    = oneString.noHistory.doc("Full path to root of source code files where db is used")
      val defFilePath = oneString.noHistory.doc("Full path to definition file for database")
      val pkg         = oneString.noHistory.doc("Package of definition file")
      val inputArity  = oneInt.noHistory
      val outputArity = oneInt.noHistory
      val partitions  = many[Partition].isComponent.doc("Partitions in Database")
    }

    object Partition extends Partition
    trait Partition {
      val pos         = oneInt.doc("Position index starting at 1")
      val name        = oneString.doc("Partition name. If partitions are not used a single meta partition with empty name is used.")
      val descr       = oneString.doc("Human short description of partition (used as comment above part in def file)")
      val namespaces  = many[Namespace].isComponent.doc("Partition owns Namespaces")
      val entityCount = oneInt.doc("Latest count of entities with asserted attributes of this partition")
      val molecules   = manyBi[stats.Molecule.partitions.type].doc("Molecules in code using this partition")
    }

    object Namespace extends Namespace
    trait Namespace {
      val pos         = oneInt.doc("Position index starting at 1")
      val name        = oneString.doc("Capitalized namespace name without partition prefix ('Person')")
      val nameFull    = oneString.doc("Non-capitalized namespace name with/without partition prefix ('person' or 'community_Person')")
      val descr       = oneString.doc("Short description of namespace (used as comment above ns in def file)")
      val entityCount = oneInt.doc("Latest count of entities with asserted attributes of this namespace")
      val attrs       = many[Attribute].isComponent.doc("Attribute owns Attributes")
      val molecules   = manyBi[stats.Molecule.namespaces.type].doc("Molecules in code using this namespace")
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
      val topValues          = many[stats.TopValue].noHistory.isComponent.doc("Latest Top 25 values (as strings)")

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
      val settings   = mapString.noHistory.doc("Key/setting pairs")
      val dbSettings = many[DbSettings].noHistory.isComponent.doc("Db specific settings")
    }
    trait DbSettings {
      val db         = one[meta.Db].noHistory.doc("Database")
      val stars      = manyLong.noHistory.doc("Starred entity ids for this db")
      val flags      = manyLong.noHistory.doc("Flagged entity ids for this db")
      val checks     = manyLong.noHistory.doc("Checked entity ids for this db")
      val undoneTs   = manyLong.noHistory.doc("Bit-encoded pairs of new/undone transaction t. For tx rollbacks. Non-intrusive alternative to real tx meta provisioning")
      val queries    = many[Query].noHistory.isComponent.doc("Recent/saved/favorite queries for this db")
    }
    trait Query {
      val molecule    = oneString.noHistory.doc("Molecule of query to build model/view")
      val part        = oneString.noHistory.doc("Initial partition of molecule")
      val ns          = oneString.noHistory.doc("Initial namespace of molecule")
      val isFavorite  = oneBoolean.noHistory.doc("Is a favorite query to appear in alphabetic shortcut list")
      val showGrouped = oneBoolean.noHistory.doc("Show grouped attribute")
      val groupedCols = manyInt.noHistory.doc("Col indexes of open grouped attributes")
      val colSettings = many[ColSetting].noHistory.isComponent.doc("Column settings for query result")
    }
    trait ColSetting {
      val colIndex = oneInt.noHistory.doc("Column index")
      val sortDir  = oneString.noHistory.doc("asc/desc or empty string if not sorted")
      val sortPos  = oneInt.noHistory.doc("If multiple sort columns, an index from 1 to max 5")
    }
  }
}
