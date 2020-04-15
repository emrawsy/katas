package katas

import akka.util.Index

class BinaryChop {

  type Index = Int

  def chop(foo: Array[Int], key: Int) : Option[Index] = {
    def search(low: Index, high: Index): Option[Index] = {
      if (low > high) {
        None
      } else {
        val mid: Index = (low + (high - low) / 2)
        foo(mid) match {
          case mv if (mv == key) => Some(mid)
          case mv if (mv <= key) => search(mid + 1, high)
          case _ => search(low, mid -1)
        }
      }
    }
    search(0, foo.size -1)
  }
}
