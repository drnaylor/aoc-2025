package uk.co.drnaylor.aoc2025

import cats.Show

object ShowTypeclasses {
  
  extension[T: Show] (input: T) {
    def asString: String = summon[Show[T]].show(input) 
  }

  given Show[Int] with {
    override def show(t: Int): String = t.toString
  }

  given Show[Long] with {
    override def show(t: Long): String = t.toString
  }

  given Show[String] with {
    override def show(t: String): String = t
  }


}
