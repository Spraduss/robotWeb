package library

trait AnalysePage {
  /** A partir d'une URL de requête sur le site de référence et d'une expression exp,
   * retourne de pages issues de la requête et satisfaisant l'expression.
   *
   * @param url l'URL de la requête sur le site de référence
   * @param exp l'expression à vérifier sur les pages trouvées
   * @return la liste des couples (titre,ref) où ref est l'URL d'une page
   *         satisfaisant l'expression et titre est son titre. */

  def resultats(url: String, exp: Expression): List[(String, String)]

  val objFiltrageUrls: FiltrageURLs
  val objFiltrageHtml: FiltrageHtml
}


trait FiltrageURLs {
  /** A partir d'un document Html h, rend la liste des URLs accessibles à partir
   * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
   * d'annonces du site de référence
   *
   * @param h le document Html
   * @return la liste des URLs d'annonces contenues dans h
   */
  def filtreAnnonce(h: Html): List[String]
}

trait FiltrageHtml {
  /** A partir d'un document Html h et d'une requête e, dit si le document
   * satisfait l'expression e
   *
   * @param h le document Html
   * @param e l'expression
   * @return true si le document satisfait l'expression e
   */
  def filtreHtml(h: Html, e: Expression): Boolean
}

trait ProductionResultat {
  /** A partir d'une liste de couples (titre,URL), produit un document Html, qui
   * liste les solutions sous la forme de liens cliquables
   *
   * @param l la liste des couples solution (titre,URL)
   * @return le document Html listant les solutions
   */
  def resultat2html(l: List[(String, String)]): Html
}


trait Html2String {
  /** Produit la chaîne de caractère correspondant à un document Html
   *
   * @param h le document Html
   * @return la chaîne de caractère représentant h
   */
  def process(h: Html): String
}

