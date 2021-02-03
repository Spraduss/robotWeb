package library

 import org.junit.Assert._

import org.junit.Test

class ExpressionTest {
  @Test
  def flatten_word() = {
    assertEquals(List(List("a")), Word("a").convertToList())
  }

  @Test
  def flatten_word_or() = {
    assertEquals(List(List("a"), List("b")), Or(Word("a"), Word("b")).convertToList())
  }

  @Test
  def flatten_word_and() = {
    assertEquals(List(List("a", "b")), And(Word("a"), Word("b")).convertToList())
  }

  @Test
  def flatten_word_or_nested_or() = {
    assertEquals(
      List(List("a"), List("b"), List("c")),
      /**/
      Or(
        Word("a"),
        Or(Word("b"), Word("c"))
      ).convertToList()
    )
    assertEquals(
      List(List("a"), List("b"), List("c"), List("d")),
      /**/
      Or(
        Or(Word("a"), Word("b")),
        Or(Word("c"), Word("d"))
      ).convertToList()
    )
    assertEquals(
      List(List("a"), List("b"), List("c"), List("d")),
      /**/
      Or(
        Word("a"),
        Or(
          Or(Word("b"), Word("c")),
          Word("d")
        )
      ).convertToList()
    )
    assertEquals(
      List(List("a"), List("b"), List("c"), List("d"), List("e")),
      /**/
      Or(
        Word("a"),
        Or(
          Or(Word("b"), Word("c")),
          Or(Word("d"), Word("e"))
        )
      ).convertToList()
    )
  }

  @Test
  def flatten_word_or_nested_and() = {
    assertEquals(
      List(List("a"), List("b", "c")),
      /**/
      Or(
        Word("a"),
        And(Word("b"), Word("c"))
      ).convertToList()
    )
    assertEquals(
      List(List("a", "b"), List("c", "d")),
      /**/
      Or(
        And(Word("a"), Word("b")),
        And(Word("c"), Word("d"))
      ).convertToList()
    )
  }

  @Test
  def flatten_word_and_nested_or() = {
    assertEquals(
      List(List("a", "b"), List("a", "c")),
      /**/
      And(
        Word("a"),
        Or(Word("b"), Word("c"))
      ).convertToList()
    )

    assertEquals(
      List(List("a", "b"), List("a", "c")),
      /**/
      And(
        Or(Word("b"), Word("c")),
        Word("a"),
      ).convertToList()
    )

    assertEquals(
      List(List("a", "c"), List("a", "d"), List("b", "c"), List("b", "d")),
      /**/
      And(
        Or(Word("a"), Word("b")),
        Or(Word("c"), Word("d"))
      ).convertToList()
    )

  }

  @Test
  def flatten_word_and_nested_and() = {
    assertEquals(
      List(List("a", "b", "c")),
      /**/
      And(
        Word("a"),
        And(Word("b"), Word("c"))
      ).convertToList()
    )
    assertEquals(
      List(List("a", "b", "c", "d")),
      /**/
      And(
        And(Word("a"), Word("b")),
        And(Word("c"), Word("d"))
      ).convertToList()
    )

    assertEquals(
      List(List("a", "b", "c", "d")),
      /**/
      And(
        Word("a"),
        And(Word("b"), And(Word("c"), Word("d")))
      ).convertToList()
    )
    assertEquals(
      List(List("a", "b", "c", "d", "e")),
      /**/
      And(
        Word("a"),
        And(
          And(Word("b"), Word("c")),
          And(Word("d"), Word("e"))
        )
      ).convertToList()
    )
  }
}