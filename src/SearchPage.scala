
object SearchPage {

  /*
   * Génère un URL
   * A partir des mots clés issus de la recherche
   * @param l la liste de listes contenant les mots cléfs
   * @return s l'url
   */
  def extractKeywordsAnd(l: List[String]): String = {
    var s = "https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords="
    for (i <- 0 to l.size - 1) {
      if (l.apply(i).size != 0)
        s += "+" + l.apply(i) //complete l'url avec les mots clés pour affiner la recherche
      else s += "" // renvoit sur la page d'accueil si on a pas de mots clés
    }
    s
  }

  /*
 * Regroupe les URLs dans une liste
 * Créer une liste d'URLs en fonction des mots clés
 * @param une liste de listes de mots clés
 * @return une liste d'URLs
 */
  def extractKeywordsOr(listKeywords: List[List[String]]): List[String] = {
    var result: List[String] = List()

    for (keywords <- listKeywords) {
      result = result :+ extractKeywordsAnd(keywords)
    }
    result
  }
}

