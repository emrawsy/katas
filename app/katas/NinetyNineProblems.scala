package katas

object NinetyNineProblems {

  def Add(foo: String, bar: String) = {
    if (foo.isEmpty && bar.isEmpty) {
      0
    } else {
      foo + ", " + bar
    }
  }

  def convertToInt(input: Char): Int = {
    val convertChar = ('a' to 'z').zip(1 to 26).toMap
    convertChar(input)
  }

  def stringToChars(input: String): List[Int] = {
    input.toList.map(x => convertToInt(x))
  }

  def keyString(input: String, key: Int): String = {
    val multiplier = input.length / key.toString.length
    val remainder = input.length % key.toString.length
    val endOfKey = key.toString.take(remainder).toString
    val newKey = key.toString * multiplier + endOfKey
    newKey.toString
  }

}
