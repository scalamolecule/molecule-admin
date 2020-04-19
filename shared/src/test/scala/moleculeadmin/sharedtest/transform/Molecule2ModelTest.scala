package moleculeadmin.sharedtest.transform

import molecule.ast.model._
import molecule.util.Helpers
import moleculeadmin.shared.ops.transform.Molecule2Model
import moleculeadmin.shared.testdata.{ExampleData, TreeSchema}
import utest._
import scala.languageFeature.implicitConversions._


object Molecule2ModelTest extends TestSuite with TreeSchema with Helpers with ExampleData {


  val tests = Tests {

    test("Basic") {

      // mandatory/optional/tacit
      Molecule2Model("Ns.str") ==> Right(Seq(Atom("Ns", "str", "String", 1, VarValue)))
      Molecule2Model("Ns.str$") ==> Right(Seq(Atom("Ns", "str$", "String", 1, VarValue)))
      Molecule2Model("Ns.str_") ==> Right(Seq(Atom("Ns", "str_", "String", 1, VarValue)))

      // type and cardinality
      Molecule2Model("Ns.str.ints") ==> Right(Seq(
        Atom("Ns", "str", "String", 1, VarValue),
        Atom("Ns", "ints", "Int", 2, VarValue)
      ))

      // trim
      Molecule2Model("  Ns . str ") ==> Right(Seq(Atom("Ns", "str", "String", 1, VarValue)))

      // accepting last character to be a punctuation
      Molecule2Model("Ns.str.") ==> Right(Seq(Atom("Ns", "str", "String", 1, VarValue)))

      // not accepting multiple punctuations in end
      Molecule2Model("Ns.str..") ==> Left(
        "Unrecognized pattern `.` in molecule: Ns.str.."
      )

      // Validation
      Molecule2Model("") ==> Left("Can't get model from empty molecule")
      Molecule2Model(".") ==> Left("Can't extract token from empty molecule")
      Molecule2Model("Xx.int") ==> Left("Unrecognized initial namespace name `Xx` in molecule: Xx.int")
      Molecule2Model("Ns.xx") ==> Left("Unrecognized attribute name `xx` in molecule: Ns.xx")
    }


    test("e") {
      Molecule2Model("Ns.e") ==> Right(Seq(
        Generic("Ns", "e", "datom", EntValue)
      ))
      Molecule2Model("Ns.e.str") ==> Right(Seq(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "str", "String", 1, VarValue, None, List(), List())
      ))
      Molecule2Model("Ns.str.e") ==> Right(Seq(
        Atom("Ns", "str", "String", 1, VarValue, None, List(), List()),
        Generic("Ns", "e", "datom", EntValue)
      ))
    }


    test("Enum") {
      Molecule2Model("Ns.enum") ==> Right(Seq(
        Atom("Ns", "enum", "String", 1, EnumVal, Some(":Ns.enum/"), List(), List())
      ))

    }


    test("Ref attr") {
      Molecule2Model("Ns.ref1") ==> Right(Seq(
        Atom("Ns", "ref1", "ref", 1, VarValue, None, List(), List())
      ))

      Molecule2Model("Ns.refs1") ==> Right(Seq(
        Atom("Ns", "refs1", "ref", 2, VarValue, None, List(), List())
      ))
    }


    test("Ref") {

      // refAttr same name as refNs
      Molecule2Model("Ns.int.Ref1.int1") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue),
        Bond("Ns", "ref1", "Ref1", 1),
        Atom("Ref1", "int1", "Int", 1, VarValue),
      ))

      // refAttr name different from refNs
      Molecule2Model("Ns.int.RefSub1.int1") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue),
        Bond("Ns", "refSub1", "Ref1", 1),
        Atom("Ref1", "int1", "Int", 1, VarValue),
      ))

      // Jumping
      Molecule2Model("Ns.Ref1") ==> Right(Seq(
        Bond("Ns", "ref1", "Ref1", 1)
      ))
      Molecule2Model("Ns.Ref1.int1") ==> Right(Seq(
        Bond("Ns", "ref1", "Ref1", 1),
        Atom("Ref1", "int1", "Int", 1, VarValue),
      ))

      Molecule2Model("Ns.Ref1.Ref2.int2") ==> Right(Seq(
        Bond("Ns", "ref1", "Ref1", 1),
        Bond("Ref1", "ref2", "Ref2", 1),
        Atom("Ref2", "int2", "Int", 1, VarValue),
      ))

      // Invalid ref
      Molecule2Model("Ns.int.Xx.int2") ==> Left(
        "Unrecognized ref namespace `Xx` from namespace `Ns` in molecule: Ns.int.Xx.int2"
      )

      // Invalid ref
      Molecule2Model("Ns.int.Ref2.int2") ==> Left(
        "Unrecognized ref namespace `Ref2` from namespace `Ns` in molecule: Ns.int.Ref2.int2"
      )
    }


    test("Backref") {

      Molecule2Model("Ns.int.Ref1.int1._Ns.str") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1", "Int", 1, VarValue, None, Seq(), Seq()),
        ReBond("Ns"),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq())
      ))

      Molecule2Model("Ns.int.RefSub1.int1._Ns.str") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "refSub1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1", "Int", 1, VarValue, None, Seq(), Seq()),
        ReBond("Ns"),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq())
      ))

      Molecule2Model("Ns.int.Ref1.int1.Ref2.int2._Ref1._Ns.str") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ns", "ref1", "Ref1", 1, Seq()),
        Atom("Ref1", "int1", "Int", 1, VarValue, None, Seq(), Seq()),
        Bond("Ref1", "ref2", "Ref2", 1, Seq()),
        Atom("Ref2", "int2", "Int", 1, VarValue, None, Seq(), Seq()),
        ReBond("Ref1"),
        ReBond("Ns"),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq())
      ))

      // Unrecognized backRef
      Molecule2Model("Ns.int.Ref1.int1._Xx.str") ==> Left(
        "Unrecognized back ref namespace `_Xx` from namespace `Ref1` in molecule: Ns.int.Ref1.int1._Xx.str"
      )

      // Invalid backRef
      Molecule2Model("Ns.int.Ref1.int1.Ref2.int2._Ns.str") ==> Left(
        "Namespace `Ref2` can't reference directly back to namespace `Ns` in molecule: Ns.int.Ref1.int1.Ref2.int2._Ns.str"
      )
    }


    test("Generic") {
      Molecule2Model("Ns.int.doc") ==> Left("Can't use reserved generic attribute name `doc` in molecule: Ns.int.doc")
      Molecule2Model("Ns.int.doc_") ==> Left("Can't use reserved generic attribute name `doc_` in molecule: Ns.int.doc_")
    }


    test("apply") {

      // In a query only context, applying empty value is not allowed.
      Molecule2Model("Ns.int.apply()") ==> Left(
        "Applying an empty value to attribute `int` not allowed in molecule: Ns.int.apply()"
      )
      Molecule2Model("Ns.int()") ==> Left(
        "Applying an empty value to attribute `int` not allowed in molecule: Ns.int()"
      )

      Molecule2Model("Ns.int.apply(unknownVariable)") ==> Left(
        "Unrecognized Int expression value `unknownVariable` for attribute `int` in molecule: Ns.int.apply(unknownVariable)"
      )
      Molecule2Model("Ns.int(unknownVariable)") ==> Left(
        "Unrecognized Int expression value `unknownVariable` for attribute `int` in molecule: Ns.int(unknownVariable)"
      )

      // Trim tokens
      Molecule2Model("Ns.int.apply(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))
      Molecule2Model("Ns.int. apply(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))
      Molecule2Model("Ns.int. apply ( 1 )") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))
      Molecule2Model("Ns.int .apply ( 1 )") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))
      Molecule2Model("  Ns  .  int . apply ( 1 )  ") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))

      // Implicit apply
      Molecule2Model("Ns.int(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))
      Molecule2Model("  Ns  .  int  ( 1 )  ") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))

      // varargs
      Molecule2Model("Ns.int(1, 2)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2)))))
      Molecule2Model("Ns.int( 1 ,  2  )") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2)))))

      // Seq/List
      Molecule2Model("Ns.int(Seq(1))") ==>
        Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)))))
      Molecule2Model("Ns.int(Seq(1, 2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2)))))
      Molecule2Model("Ns.int(List(1, 2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2)))))

      // OR expressions
      Molecule2Model("Ns.int(1 or 2)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2)))))

      // Multiple Seq/List
      Molecule2Model("Ns.int(Seq(1), Seq(2, 3))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2, 3)))))
      Molecule2Model("Ns.int(List(1), List(2, 3))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2, 3)))))
      Molecule2Model("Ns.int(Seq(1), List(2, 3))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2, 3)))))
      Molecule2Model("Ns.int(Seq(1, 2), List(3, 4), Seq(5, 6))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1, 2, 3, 4, 5, 6)))))


      // Other types

      // Long with optional postfix "L"
      Molecule2Model("Ns.long(1, 2L)") ==> Right(Seq(Atom("Ns", "long", "Long", 1, Eq(Seq(1L, 2L)))))
      Molecule2Model("Ns.long(1L or 2L)") ==> Right(Seq(Atom("Ns", "long", "Long", 1, Eq(Seq(1L, 2L)))))
      Molecule2Model("Ns.long(Seq(1L, 2))") ==> Right(Seq(Atom("Ns", "long", "Long", 1, Eq(Seq(1L, 2L)))))

      // Refs have their own type "ref"
      Molecule2Model("Ns.ref1(42)") ==> Right(Seq(Atom("Ns", "ref1", "ref", 1, Eq(Seq(42L)), None, List(), List())))
      Molecule2Model("Ns.refs1(42)") ==> Right(Seq(Atom("Ns", "refs1", "ref", 2, Eq(Seq(42L)), None, List(), List())))

      Molecule2Model("Ns.float(1.1, 2f)") ==> Right(Seq(Atom("Ns", "float", "Float", 1, Eq(Seq(1.1f, 2f)))))
      Molecule2Model("Ns.float(1.1 or 2f)") ==> Right(Seq(Atom("Ns", "float", "Float", 1, Eq(Seq(1.1f, 2f)))))
      Molecule2Model("Ns.float(Seq(1.1f, 2.0))") ==> Right(Seq(Atom("Ns", "float", "Float", 1, Eq(Seq(1.1f, 2.0f)))))

      Molecule2Model("Ns.double(1.1, 2)") ==> Right(Seq(Atom("Ns", "double", "Double", 1, Eq(Seq(1.1, 2)))))
      Molecule2Model("Ns.double(1.1 or 2)") ==> Right(Seq(Atom("Ns", "double", "Double", 1, Eq(Seq(1.1, 2)))))
      Molecule2Model("Ns.double(Seq(1.1, 2.0))") ==> Right(Seq(Atom("Ns", "double", "Double", 1, Eq(Seq(1.1, 2.0)))))

      Molecule2Model("""Ns.str("a")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a")))))
      Molecule2Model("""Ns.str("a", "b")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b")))))
      Molecule2Model("""Ns.str("a" or "b")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b")))))
      Molecule2Model("""Ns.str("a" or "b" or "c")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b", "c")))))
      Molecule2Model("""Ns.str(Seq("a", "b"))""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b")))))
      Molecule2Model("""Ns.str(Seq("a", "b"), Seq("c"))""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b", "c")))))
      Molecule2Model("""Ns.str(Seq("a", "b"), Seq("c"), Seq("d"))""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b", "c", "d")))))
      Molecule2Model("""Ns.str(Seq("a", "b"), Seq("c, d"))""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a", "b", "c, d")))))


      // Punctuations inside text strings ok
      Molecule2Model("""Ns.str("...")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("...")))))
      Molecule2Model("""Ns.str("a (b), (c)")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Eq(Seq("a (b), (c)")))))


      Molecule2Model("Ns.bool(true, false)") ==> Right(Seq(Atom("Ns", "bool", "Boolean", 1, Eq(Seq(true, false)))))
      Molecule2Model("Ns.bool(true or false)") ==> Right(Seq(Atom("Ns", "bool", "Boolean", 1, Eq(Seq(true, false)))))
      Molecule2Model("Ns.bool(Seq(true, false))") ==> Right(Seq(Atom("Ns", "bool", "Boolean", 1, Eq(Seq(true, false)))))

      // Annoying Date formatting
      Molecule2Model(s"""Ns.date("${date2str(date1)}")""") ==> Right(Seq(
        Atom("Ns", "date", "Date", 1, Eq(Seq(date1)), None, Seq(), Seq())
      ))
      Molecule2Model(s"""Ns.date("${date2str(date1)}", "${date2str(date2)}")""") ==> Right(Seq(
        Atom("Ns", "date", "Date", 1, Eq(Seq(date1, date2)), None, Seq(), Seq())
      ))
      Molecule2Model(s"""Ns.date("${date2str(date1)}" or "${date2str(date2)}")""") ==> Right(Seq(
        Atom("Ns", "date", "Date", 1, Eq(Seq(date1, date2)), None, Seq(), Seq())
      ))
      Molecule2Model(s"""Ns.date(Seq("${date2str(date1)}", "${date2str(date2)}"))""") ==> Right(Seq(
        Atom("Ns", "date", "Date", 1, Eq(Seq(date1, date2)), None, Seq(), Seq())
      ))

      Molecule2Model(s"""Ns.uuid("$uuid1", "$uuid2")""") ==> Right(Seq(Atom("Ns", "uuid", "UUID", 1, Eq(Seq(uuid1, uuid2)))))
      Molecule2Model(s"""Ns.uuid("$uuid1" or "$uuid2")""") ==> Right(Seq(Atom("Ns", "uuid", "UUID", 1, Eq(Seq(uuid1, uuid2)))))
      Molecule2Model(s"""Ns.uuid(Seq("$uuid1", "$uuid2"))""") ==> Right(Seq(Atom("Ns", "uuid", "UUID", 1, Eq(Seq(uuid1, uuid2)))))

      Molecule2Model(s"""Ns.uri("$uri1", "$uri2")""") ==> Right(Seq(Atom("Ns", "uri", "URI", 1, Eq(Seq(uri1, uri2)))))
      Molecule2Model(s"""Ns.uri("$uri1" or "$uri2")""") ==> Right(Seq(Atom("Ns", "uri", "URI", 1, Eq(Seq(uri1, uri2)))))
      Molecule2Model(s"""Ns.uri(Seq("$uri1", "$uri2"))""") ==> Right(Seq(Atom("Ns", "uri", "URI", 1, Eq(Seq(uri1, uri2)))))

      Molecule2Model(s"Ns.bigInt($bigInt1, $bigInt2)") ==> Right(Seq(Atom("Ns", "bigInt", "BigInt", 1, Eq(Seq(bigInt1, bigInt2)))))
      Molecule2Model(s"Ns.bigInt($bigInt1 or $bigInt2)") ==> Right(Seq(Atom("Ns", "bigInt", "BigInt", 1, Eq(Seq(bigInt1, bigInt2)))))
      Molecule2Model(s"Ns.bigInt(Seq($bigInt1, $bigInt2))") ==> Right(Seq(Atom("Ns", "bigInt", "BigInt", 1, Eq(Seq(bigInt1, bigInt2)))))

      Molecule2Model(s"Ns.bigDec($bigDec1, $bigDec2)") ==> Right(Seq(Atom("Ns", "bigDec", "BigDecimal", 1, Eq(Seq(bigDec1, bigDec2)))))
      Molecule2Model(s"Ns.bigDec($bigDec1 or $bigDec2)") ==> Right(Seq(Atom("Ns", "bigDec", "BigDecimal", 1, Eq(Seq(bigDec1, bigDec2)))))
      Molecule2Model(s"Ns.bigDec(Seq($bigDec1, $bigDec2))") ==> Right(Seq(Atom("Ns", "bigDec", "BigDecimal", 1, Eq(Seq(bigDec1, bigDec2)))))

      // null
      Molecule2Model("Ns.int.str_(Nil)") ==> Right(List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Atom("Ns", "str_", "String", 1, Fn("not", None), None, Seq(), Seq())))
    }


    test("apply, e") {

      // Unrecognized function (seen as a wrong attribute name having a value applied)
      Molecule2Model("Ns.e.xx(1)") ==> Left(
        "Unrecognized attribute name `xx` (having expression) in molecule: Ns.e.xx(1)"
      )
      Molecule2Model("Ns.e_.xx(1)") ==> Left(
        "`e_` can only be tacit if an id is applied to it (`e_(12345L)`) in molecule: Ns.e_.xx(1)"
      )

      // Wrong expression value
      Molecule2Model("""Ns.e.not(1)""") ==> Left(
        "Un-allowed expression function `not` for entity id attribute `e` in molecule: Ns.e.not(1)"
      )

      // Wrong expression value
      Molecule2Model("""Ns.e.apply(true)""") ==> Left(
        "Unrecognized expression value `true` for entity id attribute `e` in molecule: Ns.e.apply(true)"
      )


      Molecule2Model("""Ns.e.apply(123).int""") ==> Right(Seq(
        Generic("Ns", "e", "datom", Eq(Seq(123))),
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List())
      ))
      Molecule2Model("""Ns.e.apply(123L).int""") ==> Right(Seq(
        Generic("Ns", "e", "datom", Eq(Seq(123))),
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List())
      ))
      Molecule2Model("""Ns.e(123).int""") ==> Right(Seq(
        Generic("Ns", "e", "datom", Eq(Seq(123))),
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List())
      ))

      Molecule2Model("""Ns.e_(123).int""") ==> Right(Seq(
        Generic("Ns", "e_", "datom", Eq(Seq(123))),
        Atom("Ns", "int", "Int", 1, VarValue, None, List(), List())
      ))
    }


    test("aggregates") {
      Molecule2Model("Ns.int(min)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("min", None))))
      Molecule2Model("Ns.int(min(1))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("min", Some(1)))))
      Molecule2Model("Ns.int(min(2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("min", Some(2)))))

      Molecule2Model("Ns.int(max)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("max", None))))
      Molecule2Model("Ns.int(max(1))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("max", Some(1)))))
      Molecule2Model("Ns.int(max(2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("max", Some(2)))))

      Molecule2Model("Ns.int(rand)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("rand", None))))
      Molecule2Model("Ns.int(rand(1))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("rand", Some(1)))))
      Molecule2Model("Ns.int(rand(2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("rand", Some(2)))))

      Molecule2Model("Ns.int(sample)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("sample", None))))
      Molecule2Model("Ns.int(sample(1))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("sample", Some(1)))))
      Molecule2Model("Ns.int(sample(2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("sample", Some(2)))))

      Molecule2Model("Ns.int(distinct)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("distinct", None))))

      Molecule2Model("Ns.int(count)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("count", None))))
      Molecule2Model("Ns.int(countDistinct)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("count-distinct", None))))

      Molecule2Model("Ns.int(sum)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("sum", None))))
      Molecule2Model("Ns.int(avg)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("avg", None))))
      Molecule2Model("Ns.int(median)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("median", None))))
      Molecule2Model("Ns.int(variance)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("variance", None))))
      Molecule2Model("Ns.int(stddev)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Fn("stddev", None))))
    }


    test("attr + aggregates") {

      Molecule2Model("Ns.int.int(count)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("count")),
        Atom("Ns", "int", "Int", 1, Fn("count", None))
      ))

      Molecule2Model("Ns.int.int(countDistinct)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("count-distinct")),
        Atom("Ns", "int", "Int", 1, Fn("count-distinct", None))
      ))
      Molecule2Model("Ns.int.int(sum)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("sum")),
        Atom("Ns", "int", "Int", 1, Fn("sum", None))
      ))
      Molecule2Model("Ns.int.int(avg)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("avg")),
        Atom("Ns", "int", "Int", 1, Fn("avg", None))
      ))
      Molecule2Model("Ns.int.int(median)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("median")),
        Atom("Ns", "int", "Int", 1, Fn("median", None))
      ))
      Molecule2Model("Ns.int.int(variance)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("variance")),
        Atom("Ns", "int", "Int", 1, Fn("variance", None))
      ))
      Molecule2Model("Ns.int.int(stddev)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, VarValue, None, Nil, Seq("stddev")),
        Atom("Ns", "int", "Int", 1, Fn("stddev", None))
      ))

      Molecule2Model("Ns.int(rand(3)).int(count).int(avg)") ==> Right(Seq(
        Atom("Ns", "int", "Int", 1, Fn("rand", Some(3)), None, Nil, Seq("count", "avg")),
        Atom("Ns", "int", "Int", 1, Fn("count", None)),
        Atom("Ns", "int", "Int", 1, Fn("avg", None)),
      ))

      Molecule2Model("Ns.e.int(count).int") ==> Left(
        """Expecting aggregate function after clean attribute:
          |Found    : `int(count).int`
          |Expecting: `int.int(count)`""".stripMargin
      )
    }


    test("comparison") {

      Molecule2Model("Ns.int.>(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Gt(1))))

      // postfix notation supported for last expression
      Molecule2Model("Ns.int > 1") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Gt(1))))
      // only works for the last expression
      Molecule2Model("Ns.int > 1.str") ==> Left(
        "Unrecognized Int expression value `1.str` for attribute `int` in molecule: Ns.int > 1.str"
      )


      Molecule2Model("Ns.int.>=(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Ge(1))))
      Molecule2Model("Ns.int.<(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Lt(1))))
      Molecule2Model("Ns.int.<=(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Le(1))))

      Molecule2Model("Ns.int.!=(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1)))))
      Molecule2Model("Ns.int.!=(1, 2)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1, 2)))))
      Molecule2Model("Ns.int.!=(Seq(1, 2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1, 2)))))

      Molecule2Model("Ns.int.not(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1)))))
      Molecule2Model("Ns.int.not(1, 2)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1, 2)))))
      Molecule2Model("Ns.int.not(Seq(1, 2))") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1, 2)))))

      Molecule2Model("Ns.int.>()") ==> Left(
        "Comparing an empty value to attribute `int` not allowed in molecule: Ns.int.>()"
      )
      Molecule2Model("Ns.int.!<(7)") ==> Left(
        "Unrecognized pattern `!<(7)` in molecule: Ns.int.!<(7)"
      )

      Molecule2Model("Ns.double.not(3.7)") ==> Right(Seq(Atom("Ns", "double", "Double", 1, Neq(Seq(3.7)))))
    }


    test("fulltext") {

      Molecule2Model("""Ns.str.contains("b")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Fulltext(Seq("b")), None, List(), List())))
      Molecule2Model("""Ns.str.contains("b", "c")""") ==> Right(Seq(Atom("Ns", "str", "String", 1, Fulltext(Seq("b", "c")), None, List(), List())))

      Molecule2Model("""Ns.str.contains("a")""") ==> Left(
        """Can't use non-indexed standard word `a` for fulltext search for attribute `str` in molecule: Ns.str.contains("a")"""
      )
    }


    test("multiple") {

      Molecule2Model("Ns.int.long") ==> Right(Seq(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, VarValue, None, Seq(), Seq())))
      Molecule2Model("Ns.int.long(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, Eq(Seq(1)), None, Seq(), Seq())))
      Molecule2Model("Ns.int(1).long") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)), None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, VarValue, None, Seq(), Seq())))
      Molecule2Model("Ns.int(1).long(2)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Eq(Seq(1)), None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, Eq(Seq(2)), None, Seq(), Seq())))

      Molecule2Model("Ns.int.long.not(1)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, Neq(Seq(1)), None, Seq(), Seq())))
      Molecule2Model("Ns.int.not(1).long") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1)), None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, VarValue, None, Seq(), Seq())))
      Molecule2Model("Ns.int.not(1).long.not(2)") ==> Right(Seq(Atom("Ns", "int", "Int", 1, Neq(Seq(1)), None, Seq(), Seq()), Atom("Ns", "long", "Long", 1, Neq(Seq(2)), None, Seq(), Seq())))


      Molecule2Model("Ns.double(2.3).int.>(7).str") ==> Right(Seq(
        Atom("Ns", "double", "Double", 1, Eq(Seq(2.3)), None, Seq(), Seq()),
        Atom("Ns", "int", "Int", 1, Gt(7), None, Seq(), Seq()),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq())
      ))
    }

    test("multiple") {
      Molecule2Model("Ns.int.str.tx") ==> Right(List(
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq()),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq("tx")),
        Generic("Ns", "tx", "datom", NoValue),
      ))

      Molecule2Model("Ns.e.str.tx.int") ==> Right(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq("tx")),
        Generic("Ns", "tx", "datom", NoValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq())))

      Molecule2Model("Ns.e.str.str(count).tx.int") ==> Right(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq("count", "tx")),
        Atom("Ns", "str", "String", 1, Fn("count", None), None, Seq(), Seq()),
        Generic("Ns", "tx", "datom", NoValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq())))

      Molecule2Model("Ns.e.int.int(count).int(sum).tx.str") ==> Right(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("count", "sum", "tx")),
        Atom("Ns", "int", "Int", 1, Fn("count", None), None, Seq(), Seq()),
        Atom("Ns", "int", "Int", 1, Fn("sum", None), None, Seq(), Seq()),
        Generic("Ns", "tx", "datom", NoValue),
        Atom("Ns", "str", "String", 1, VarValue, None, Seq(), Seq())))
    }

    test("edit") {
      Molecule2Model("Ns.e.int.int") ==> Right(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("orig")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit"))))

      Molecule2Model("Ns.e.int(2).int.int(count).t.tx") ==> Right(List(
        Generic("Ns", "e", "datom", EntValue),
        Atom("Ns", "int", "Int", 1, Eq(Seq(2)), None, Seq(), Seq("orig", "count", "t", "tx")),
        Atom("Ns", "int", "Int", 1, VarValue, None, Seq(), Seq("edit")),
        Atom("Ns", "int", "Int", 1, Fn("count", None), None, Seq(), Seq()),
        Generic("Ns", "t", "datom", NoValue),
        Generic("Ns", "tx", "datom", NoValue)
      ))
    }


    test("edit2") {

      import moleculeadmin.shared.testdata.mBrainzSchema
      implicit val nsMap = mBrainzSchema.nsMap

      Molecule2Model(
        "Release.name.Country.e.name._Release.Labels.name.Country.e.name"
      ) ==> Right(List(
        Atom("Release", "name", "String", 1, VarValue, None, Seq(), Seq()),
        Bond("Release", "country", "Country", 1, Seq()),
        Generic("Country", "e", "datom", EntValue),
        Atom("Country", "name", "String", 1, VarValue, None, Seq(), Seq()),
        ReBond("Release"),
        Bond("Release", "labels", "Label", 2, Seq()),
        Atom("Label", "name", "String", 1, VarValue, None, Seq(), Seq()),
        Bond("Label", "country", "Country", 1, Seq()),
        Generic("Country", "e", "datom", EntValue),
        Atom("Country", "name", "String", 1, VarValue, None, Seq(), Seq())
      ))
    }
  }
}


























































