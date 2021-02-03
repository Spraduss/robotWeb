package library

 import org.junit.Assert._

import org.junit.Test

class HtmlTest {
  @Test def getText_Text(): Unit = assertEquals("a", Text("a").getText)

  @Test def getText_Tag_One_element(): Unit = assertEquals("a", Tag("tag", List(), List(Text("a"))).getText)

  @Test def getText_Tag_Two_elements_one_text(): Unit = assertEquals("a", Tag("tag", List(), List(Text("a"), Tag("tag", List(), List()))).getText)

  @Test def getText_Tag_Two_elements_none_text(): Unit = assertEquals("", Tag("tag", List(), List(Tag("tag", List(), List()), Tag("tag", List(), List()))).getText)

  @Test def getText_Tag_Two_elements_both_text(): Unit = assertEquals("a", Tag("tag", List(), List(Text("a"), Text("b"))).getText)

  @Test def getAttributeValue_Text(): Unit = assertEquals("", Text("a").getAttributeValue("a"))

  @Test def getAttributeValue_Tag_present(): Unit = {
    assertEquals("v1", Tag("tag", List(("at1", "v1"), ("at2", "v2"), ("at3", "v3")), List()).getAttributeValue("at1"))
    assertEquals("v2", Tag("tag", List(("at1", "v1"), ("at2", "v2"), ("at3", "v3")), List()).getAttributeValue("at2"))
    assertEquals("v3", Tag("tag", List(("at1", "v1"), ("at2", "v2"), ("at3", "v3")), List()).getAttributeValue("at3"))
  }

  @Test def getAttributeValue_Tag_absent(): Unit = assertEquals("", Tag("tag", List(("at1", "v1"), ("at2", "v2"), ("at3", "v3")), List()).getAttributeValue("at4"))


  @Test
  def getFirstElementByTag_Text(): Unit = {
    assertEquals(None, Text("").getFirstElementByTag("a"))
  }

  @Test
  def getFirstElementByTag_Tag_single_Root_Present(): Unit = {
    assertEquals(Some(Tag("a", List(), List())), Tag("a", List(), List()).getFirstElementByTag("a"))
  }

  @Test
  def getFirstElementByTag_Tag_single_Root_Absent(): Unit = {
    assertEquals(None, Tag("b", List(), List()).getFirstElementByTag("a"))
  }

  @Test
  def getFirstElementByTag_Tag_single_Nested_Present(): Unit = {
    assertEquals(Some(Tag("a", List(("atr1", "vr1")), List())), Tag("b", List(), List(Tag("a", List(("atr1", "vr1")), List()), Tag("d", List(("atr2", "vr2")), List()), Tag("e", List(("atr3", "vr3")), List()))).getFirstElementByTag("a"))
  }

  @Test
  def getFirstElementByTag_Tag_multiple_Nested_Present(): Unit = {
    assertEquals(Some(Tag("a", List(("atr1", "vr1")), List())), Tag("b", List(), List(Tag("a", List(("atr1", "vr1")), List()), Tag("a", List(("atr2", "vr2")), List()), Tag("e", List(("atr3", "vr3")), List()))).getFirstElementByTag("a"))
  }

  @Test
  def getFirstElementByTag_Tag_single_Nested_Absent(): Unit = {
    assertEquals(None, Tag("b", List(), List(Tag("a", List(("atr1", "vr1")), List()), Tag("a", List(("atr2", "vr2")), List()), Tag("e", List(("atr3", "vr3")), List()))).getFirstElementByTag("z"))
  }


  @Test
  def findElementsByClassName_Text(): Unit = {
    assertEquals(List(), Text("").findElementsByClassName("a"))
  }

  @Test
  def findElementsByClassName_Tag_single_Root_Present(): Unit = {
    assertEquals(List(Tag("a", List(("class", "a")), List())), Tag("a", List(("class", "a")), List()).findElementsByClassName("a"))
    assertEquals(List(Tag("a", List(("class", "z a b c")), List())), Tag("a", List(("class", "z a b c")), List()).findElementsByClassName("a"))
  }

  @Test
  def findElementsByClassName_Tag_single_Root_Absent(): Unit = {
    assertEquals(List(), Tag("a", List(("class", "a")), List()).findElementsByClassName("e"))
    assertEquals(List(), Tag("a", List(("class", "z a b c")), List()).findElementsByClassName("e"))
  }

  @Test
  def findElementsByClassName_Tag_single_Nested_Present(): Unit = {
    assertEquals(List(Tag("d", List(("class", "c")), List())), Tag("b", List(), List(Tag("a", List(("class", "b")), List()), Tag("d", List(("class", "c")), List()), Tag("e", List(("class", "d")), List()))).findElementsByClassName("c"))
  }

  @Test
  def findElementsByClassName_Tag_multiple_Nested_Present(): Unit = {
    assertEquals(
      List(
        Tag("d", List(("class", "c w")), List()),
        Tag("e", List(("class", "c z")), List())
      ),
      // --- //
      Tag("b", List(),
        List(
          Tag("a", List(("class", "b")), List()),
          Tag("d", List(("class", "c w")), List()),
          Tag("e", List(("class", "c z")), List())
        )
      ).findElementsByClassName("c")
    )
  }

  @Test
  def findElementsByClassName_Tag_single_Nested_Absent(): Unit = {
    assertEquals(
      List(),
      // --- //
      Tag("b", List(),
        List(
          Tag("a", List(("class", "b")), List()),
          Tag("d", List(("class", "c w")), List()),
          Tag("e", List(("class", "c z")), List())
        )
      ).findElementsByClassName("a")
    )
  }




  @Test
  def findOneElementByClassName_Text(): Unit = {
    assertEquals(None, Text("").findOneElementByClassName("a"))
  }

  @Test
  def findOneElementByClassName_Tag_single_Root_Present(): Unit = {
    assertEquals(Some(Tag("a", List(("class", "a")), List())), Tag("a", List(("class", "a")), List()).findOneElementByClassName("a"))
    assertEquals(Some(Tag("a", List(("class", "z a b c")), List())), Tag("a", List(("class", "z a b c")), List()).findOneElementByClassName("a"))
  }

  @Test
  def findOneElementByClassName_Tag_single_Root_Absent(): Unit = {
    assertEquals(None, Tag("a", List(("class", "a")), List()).findOneElementByClassName("e"))
    assertEquals(None, Tag("a", List(("class", "z a b c")), List()).findOneElementByClassName("e"))
  }

  @Test
  def findOneElementByClassName_Tag_single_Nested_Present(): Unit = {
    assertEquals(Some(Tag("d", List(("class", "c")), List())), Tag("b", List(), List(Tag("a", List(("class", "b")), List()), Tag("d", List(("class", "c")), List()), Tag("e", List(("class", "d")), List()))).findOneElementByClassName("c"))
  }

  @Test
  def findOneElementByClassName_Tag_multiple_Nested_Present(): Unit = {
    assertEquals(
      Some(
        Tag("d", List(("class", "c w")), List())
      ),
      // --- //
      Tag("b", List(),
        List(
          Tag("a", List(("class", "b")), List()),
          Tag("d", List(("class", "c w")), List()),
          Tag("e", List(("class", "c z")), List())
        )
      ).findOneElementByClassName("c")
    )
  }

  @Test
  def findOneElementByClassName_Tag_single_Nested_Absent(): Unit = {
    assertEquals(
      None,
      // --- //
      Tag("b", List(),
        List(
          Tag("a", List(("class", "b")), List()),
          Tag("d", List(("class", "c w")), List()),
          Tag("e", List(("class", "c z")), List())
        )
      ).findOneElementByClassName("a")
    )
  }


}