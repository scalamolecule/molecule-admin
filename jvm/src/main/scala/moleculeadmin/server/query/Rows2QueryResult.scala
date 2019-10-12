package moleculeadmin.server.query


import moleculeadmin.shared.api.QueryApi
import moleculeadmin.shared.ast.query.{Col, QueryResult}
import moleculeadmin.shared.lib.moleculeExtras.HelpersAdmin

case class Rows2QueryResult(rowCollection: java.util.Collection[java.util.List[AnyRef]],
                            rowCountAll: Int,
                            rowCount: Int,
                            cols: Seq[Col],
                            queryTime: Long) extends CastHelpers(rowCount) with QueryApi with HelpersAdmin {

  def getCastingLambda(colIndex: Int): (java.util.List[AnyRef], Int) => Unit = {
    val col = cols(colIndex)
    if (col.aggrType.nonEmpty) {
      if (col.card == 1) {
        col.aggrType match {
          case "aggrInt"          => castAggrInt(col, colIndex)
          case "aggrDouble"       => castAggrDouble(col, colIndex)
          case "aggrSingle"       => col.colType match {
            case "string" => castMandatoryOneString(col, colIndex)
            case "double" => castMandatoryOneDouble(col, colIndex)
          }
          case "aggrSingleSample" => castAggrSingleSample(col, colIndex)
          case "aggrList"         => col.colType match {
            case "string" => castAggrListString(col, colIndex)
            case "double" => castAggrListDouble(col, colIndex)
          }
          case "aggrListRand"     => col.colType match {
            case "string" => castAggrListRandString(col, colIndex)
            case "double" => castAggrListRandDouble(col, colIndex)
          }
          case "aggrListDistinct" => castAggrListDistinct(col, colIndex)
        }
      } else {
        col.aggrType match {
          case "aggrInt"          => castAggrInt(col, colIndex)
          case "aggrDouble"       => castAggrDouble(col, colIndex)
          case "aggrSingle"       => col.colType match {
            case "listString" => castAggrManySingleString(col, colIndex)
            case "listDouble" => castAggrManySingleDouble(col, colIndex)
          }
          case "aggrSingleSample" => castAggrSingleSample(col, colIndex)
          case "aggrList"         => col.colType match {
            case "listString" => castAggrListString(col, colIndex)
            case "listDouble" => castAggrListDouble(col, colIndex)
          }
          case "aggrListRand"     => col.colType match {
            case "listString" => castAggrListRandString(col, colIndex)
            case "listDouble" => castAggrListRandDouble(col, colIndex)
          }
          case "aggrListDistinct" => castAggrListDistinct(col, colIndex)
        }
      }
    } else if (col.opt) {
      col.colType match {
        case "string"     => castOptionalOneString(col, colIndex)
        case "double"     => castOptionalOneDouble(col, colIndex)
        case "listString" => castOptionalListString(col, colIndex)
        case "listDouble" => castOptionalListDouble(col, colIndex)
        case "mapString"  => castOptionalMapString(col, colIndex)
        case "mapDouble"  => castOptionalMapDouble(col, colIndex)
      }
    } else {
      col.colType match {
        case "string"     => castMandatoryOneString(col, colIndex)
        case "double"     => castMandatoryOneDouble(col, colIndex)
        case "listString" => castMandatoryListString(col, colIndex)
        case "listDouble" => castMandatoryListDouble(col, colIndex)
        case "mapString"  => castMandatoryMapString(col, colIndex)
        case "mapDouble"  => castMandatoryMapDouble(col, colIndex)
      }
    }
  }

  val rows                        = rowCollection.iterator
  var row: java.util.List[AnyRef] = _
  var i                           = 0
  def get: QueryResult = {

    // Populate mutable arrays
    cols.size match {
      case 1 =>
        // Casting lambda for first column
        val cast1 = getCastingLambda(0)
        while (i < rowCount) {
          row = rows.next()
          // Populate casted data for column 1
          cast1(row, i)
          i += 1
        }

      case 2 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          i += 1
        }

      case 3 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          i += 1
        }

      case 4 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          i += 1
        }

      case 5 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          i += 1
        }

      case 6 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          i += 1
        }

      case 7 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          i += 1
        }

      case 8 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          i += 1
        }

      case 9 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          i += 1
        }

      case 10 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          i += 1
        }

      case 11 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          i += 1
        }

      case 12 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          i += 1
        }

      case 13 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          i += 1
        }

      case 14 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          i += 1
        }

      case 15 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          i += 1
        }

      case 16 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          i += 1
        }

      case 17 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        val cast17 = getCastingLambda(16)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          cast17(row, i)
          i += 1
        }

      case 18 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        val cast17 = getCastingLambda(16)
        val cast18 = getCastingLambda(17)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          cast17(row, i)
          cast18(row, i)
          i += 1
        }

      case 19 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        val cast17 = getCastingLambda(16)
        val cast18 = getCastingLambda(17)
        val cast19 = getCastingLambda(18)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          cast17(row, i)
          cast18(row, i)
          cast19(row, i)
          i += 1
        }

      case 20 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        val cast17 = getCastingLambda(16)
        val cast18 = getCastingLambda(17)
        val cast19 = getCastingLambda(18)
        val cast20 = getCastingLambda(19)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          cast17(row, i)
          cast18(row, i)
          cast19(row, i)
          cast20(row, i)
          i += 1
        }

      case 21 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        val cast17 = getCastingLambda(16)
        val cast18 = getCastingLambda(17)
        val cast19 = getCastingLambda(18)
        val cast20 = getCastingLambda(19)
        val cast21 = getCastingLambda(20)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          cast17(row, i)
          cast18(row, i)
          cast19(row, i)
          cast20(row, i)
          cast21(row, i)
          i += 1
        }

      case 22 =>
        val cast1 = getCastingLambda(0)
        val cast2 = getCastingLambda(1)
        val cast3 = getCastingLambda(2)
        val cast4 = getCastingLambda(3)
        val cast5 = getCastingLambda(4)
        val cast6 = getCastingLambda(5)
        val cast7 = getCastingLambda(6)
        val cast8 = getCastingLambda(7)
        val cast9 = getCastingLambda(8)
        val cast10 = getCastingLambda(9)
        val cast11 = getCastingLambda(10)
        val cast12 = getCastingLambda(11)
        val cast13 = getCastingLambda(12)
        val cast14 = getCastingLambda(13)
        val cast15 = getCastingLambda(14)
        val cast16 = getCastingLambda(15)
        val cast17 = getCastingLambda(16)
        val cast18 = getCastingLambda(17)
        val cast19 = getCastingLambda(18)
        val cast20 = getCastingLambda(19)
        val cast21 = getCastingLambda(20)
        val cast22 = getCastingLambda(21)
        while (i < rowCount) {
          row = rows.next()
          cast1(row, i)
          cast2(row, i)
          cast3(row, i)
          cast4(row, i)
          cast5(row, i)
          cast6(row, i)
          cast7(row, i)
          cast8(row, i)
          cast9(row, i)
          cast10(row, i)
          cast11(row, i)
          cast12(row, i)
          cast13(row, i)
          cast14(row, i)
          cast15(row, i)
          cast16(row, i)
          cast17(row, i)
          cast18(row, i)
          cast19(row, i)
          cast20(row, i)
          cast21(row, i)
          cast22(row, i)
          i += 1
        }

      case other => throw new RuntimeException("Unexpected number of cols: " + other)
    }

    // Return populated and empty arrays
    QueryResult(
      strArrays,
      numArrays,
      listStrArrays,
      listNumArrays,
      mapStrArrays,
      mapNumArrays,
      arrayIndexes,
      rowCountAll,
      rowCount,
      queryTime
    )
  }
}
