package moleculeadmin.client.inspiration

import scala.concurrent.{Future, Promise}


abstract class FetcherLite[T] {
  def fetchBatch(startCommitIndex: Int): Future[IndexedSeq[T]]
  var totalCount                              = rx.Var(0)
  var currentlyFetching                       = false
  var fetchQueue                              = List.empty[(Int, Promise[T])]
  var lastFetch: Option[(Int, IndexedSeq[T])] = None

  def get(commitIndex: Int): Future[T] = lastFetch match {
    case Some((lastStartIndex, lastFetchedCommits))
      if lastStartIndex <= commitIndex
        && commitIndex < lastStartIndex + lastFetchedCommits.length =>
      Future.successful(lastFetchedCommits(commitIndex - lastStartIndex))

    case _ =>
      val promise = Promise[T]()
      fetchQueue = (commitIndex -> promise) :: fetchQueue
//      kickOffFetchIfNecessary()
      promise.future
  }
}
