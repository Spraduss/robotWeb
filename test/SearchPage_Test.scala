
 import org.junit.Assert._

import org.junit.Test

import SearchPage._

class SearchPage_Test {

  @Test
  def test_Vide {
    assertEquals("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=", extractKeywordsAnd(List()))
  }

  @Test
  def test_1element {
    assertEquals("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=+Rennes", extractKeywordsAnd(List("Rennes")))
  }

  @Test
  def test_3element {
    assertEquals("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=+Rennes+Voiture+C4", extractKeywordsAnd(List("Rennes", "Voiture", "C4")))

  }

  @Test
  def test2_Vide {
    assertEquals(List("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords="), extractKeywordsOr(List(List())))

  }

  @Test
  def test_2_1element {
    assertEquals(List("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=+Rennes"), extractKeywordsOr(List(List("Rennes"))))
  }

  @Test
  def test_2_And_Or {
    assertEquals(List("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=+Rennes+Voiture","https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=+Quimper"), extractKeywordsOr(List(List("Rennes","Voiture"),List("Quimper"))))
  }
  
  
  
}
 

