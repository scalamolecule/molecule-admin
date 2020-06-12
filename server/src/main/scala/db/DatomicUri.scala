package db

trait DatomicUri {
  val protocol = "free"
  val host     = s"localhost:4334"
  val base     = s"datomic:$protocol://$host"
}
