package library

/** Les documents HTML, vus comme des objets de type Html sont structurés de la façon suivante:
 * Un noeud HTML de la forme: <NomTag att1="val1" att2="val2"> n1 n2 ... </NomTag>, sera représenté par un objet
 * Tag("NomTag",List(("att1","val1"),("att2","val2")),List(n1,n2,...))
 *
 * Un élément de texte simple sera représenté par un objet Text(s) où s est une chaîne de caractères
 */

sealed trait Html {

  /**
   * Récupère le premier élément selon son tag
   * @param tag le tag de l'élément à rechercher
   * @return le premier élément dont le tag est "tag"
   */
  def getFirstElementByTag(tag: String): Option[Html] = {
    this match {
      case Tag(name, _, children) => if (name == tag) {
        Some(this)
      } else {
        children.map(child => child.getFirstElementByTag(tag)).collectFirst { case Some(value) => value }
      }
      case Text(_) => None
    }
  }

  /**
   * Récupère tous les éléments qui contiennent une certaine classe css
   * @param className le nom de la classe css
   * @return tous les éléments qui contiennent la classe css "className"
   */
  def findElementsByClassName(className: String): List[Html] = {
    this match {
      case Tag(_, _, children) => if (this.getAttributeValue("class").split(" ").contains(className)) {
        List(this)
      } else {
        children.flatMap(child => child.findElementsByClassName(className))
      }
      case Text(_) => Nil
    }
  }

  def findOneElementByClassName(className: String): Option[Html] = {
    this match {
      case Tag(name, _, children) => if (this.getAttributeValue("class").split(" ").contains(className)) {
        Some(this)
      } else {
        children.map(child => child.findOneElementByClassName(className)).collectFirst { case Some(value) => value }
      }
      case Text(_) => None
    }
  }

  /**
   * Récupère le texte d'un élément HTML
   * @return le texte d'un élément HTML
   */
  def getText: String = {
    this match {
      case Tag(_, _, children) => children.find(child => child match {
        case Tag(_, _, _) => false
        case Text(_) => true
      }) match {
        case Some(value) => value match {
          case Tag(_, _, _) => ""
          case Text(content) => content
        }
        case None => ""
      }
      case Text(content) => content
    }
  }

  /**
   * Récupère la valeur d'un attribut
   * @param attribute le nom de l'attribut
   * @return La valeur de l'attribut "attribute"
   */
  def getAttributeValue(attribute: String): String = {
    this match {
      case Tag(_, attributes, _) =>
        val tupleAttribute = attributes.find(value => value._1 == attribute)
        tupleAttribute match {
          case Some((_, attributeValue)) => attributeValue
          case None => ""
        }
      case Text(_) => ""
    }
  }
}

case class Tag(name: String, attributes: List[(String, String)], children: List[Html]) extends Html

case class Text(content: String) extends Html


/** Un exemple de document html qui correspond au code HTML suivant:
 * <html>
 * <head>
 * <meta content="text/html; charset=ISO-8859-1"></meta>
 * <title> MyPage </title>
 * </head>
 * <body>
 * &nbsp
 * <center>
 * <a href="http://www.irisa.fr"> Lien <img> </img> </a>
 * </center>
 * </body>
 * </html>
 */

object ExempleHtml {
  val exemple = Tag("html", List(),
    List(Tag("head", List(),
      List(Tag("meta", List(("content", "text/html"), ("charset", "iso-8859-1")), List()),
        Tag("title", List(), List(Text("MyPage"))))),
      Tag("body", List(), List(
        Text("&nbsp"),
        Tag("center", List(), List(
          Tag("a", List(("href", "http://www.irisa.fr")),
            List(Text("Lien"), Tag("img", List(), List())))))))))
}
