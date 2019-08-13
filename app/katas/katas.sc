def duplicateCount(str: String): Int = {
  val dup = str.foldLeft(Map.empty[Char, Int]) {
    (m, char) =>
      val count = m.getOrElse(char, 0)
      m.updated(char, count + 1)
  }
  dup.count {
    case (ch, num) =>
      ch == ch
      num > 1
  }
}