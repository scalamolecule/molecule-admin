package moleculeadmin.client.app.logic.query.keyEvents

import moleculeadmin.client.app.logic.query.QueryState._
import org.scalajs.dom.raw.KeyboardEvent
import rx.Ctx
import scala.scalajs.js
import scala.scalajs.js.Date


trait Paging extends BaseKeyEvents {

  // Page throttling params
  private var beginning   = 0.0
  private var i           = 0
  private var j           = 0
  private var t1          = 0.0
  private var t2          = 0.0
  private var delta       = 0.0
  private var tProcess    = 0.0
  private var keyRepeatMs = 0L
  private var avg         = 0.0
  private var ratio       = 0.0
  private var cycles      = 3

  def throttle(
    e: KeyboardEvent,
    backward: () => Unit,
    forward: () => Unit,
  ) = {
    // Throttle paging for smooth rendering
    if (beginning < 10000000) {
      beginning = js.Date.now
    }
    i += 1
    if (keyRepeatMs == 0 && i == 3) {
      // Measure duration of 2 key repeats and apply a rough
      // factor of 40% for processing leaving 60% time to rendering
      keyRepeatMs = ((js.Date.now - beginning) / 2 * 0.4).round
      // println("keyRepeatInterval " + keyRepeatMs)
    }

    if (i % cycles == 0) {
      t1 = js.Date.now
      e.key match {
        case "ArrowLeft"  => backward(); j += 1
        case "ArrowRight" => forward(); j += 1
        case _            => ()
      }
      t2 = js.Date.now
      delta = t2 - t1
      tProcess = tProcess + delta
      avg = tProcess / j
      ratio = avg / keyRepeatMs
      cycles = ratio.ceil.toInt
      // println(s"  $j  $delta    $avg    $ratio    $cycles    " + (t2 - beginning))
    }
  }

  def paging(
    e: KeyboardEvent,
    ctrl: Boolean,
    alt: Boolean,
    cmd: Boolean
  )(implicit ctx: Ctx.Owner): Unit = {
    if (ctrl && alt && cmd) {
      e.key match {
        case "ArrowLeft"  => firstPage
        case "ArrowRight" => lastPage
        case _            => ()
      }

    } else if (ctrl && alt) {
      if (e.repeat) {
        throttle(e, () => prevChunk, () => nextChunk)
      } else {
        e.key match {
          case "ArrowLeft"  => prevChunk
          case "ArrowRight" => nextChunk
          case _            => ()
        }
      }

    } else if (ctrl) {
      if (e.repeat) {
        throttle(e, () => prevPage, () => nextPage)
      } else {
        e.key match {
          case "ArrowLeft"  => prevPage
          case "ArrowRight" => nextPage
          case _            => ()
        }
      }
    }
  }

  def actualRowCount: Int = if (filters.now.isEmpty)
    rowCount else cachedSortFilterIndex.length

  def isFirstPage: Boolean = offset.now == 0

  def isLastPage: Boolean = offset.now + limit.now >= actualRowCount

  def remainingRows: Int = actualRowCount - offset.now
  def curLastRow: Int = {
    val remainingRows1 = remainingRows
    offset.now + (if (remainingRows1 < limit.now) remainingRows1 else limit.now)
  }

  def chunkSize: Int = {
    val curRowCount1 = actualRowCount
    val factor       =
      if (curRowCount1 > 25000)
        100
      else if (curRowCount1 > 10000)
        50
      else if (curRowCount1 > 5000)
        25
      else
        10
    curRowCount1 / factor / limit.now
  }

  def hasChunkBefore: Boolean =
    chunkSize > 0 && offset.now > limit.now * chunkSize

  def hasChunkAfter: Boolean =
    chunkSize > 0 && remainingRows > limit.now * chunkSize


  def firstPage(implicit ctx: Ctx.Owner): Unit =
    if (!isFirstPage) offset() = 0

  def prevChunk(implicit ctx: Ctx.Owner): Unit =
    if (hasChunkBefore)
      offset() = offset.now - limit.now * chunkSize else firstPage

  def prevPage(implicit ctx: Ctx.Owner): Unit =
    if (!isFirstPage) offset() = offset.now - limit.now

  def nextPage(implicit ctx: Ctx.Owner): Unit =
    if (!isLastPage) offset() = offset.now + limit.now

  def nextChunk(implicit ctx: Ctx.Owner): Unit =
    if (hasChunkAfter)
      offset() = offset.now + limit.now * chunkSize else lastPage

  def lastPage(implicit ctx: Ctx.Owner): Unit =
    if (!isLastPage) {
      offset() = {
        val curRowCount1 = actualRowCount
        val rest         = curRowCount1 % limit.now
        if (rest == 0)
          curRowCount1 - limit.now
        else
          curRowCount1 - rest
      }
    }
}
