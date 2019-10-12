package moleculeadmin.shared.x.inspiration

case class FileTree[+T](name: String, value: T, children: IndexedSeq[FileTree[T]]) {

}

