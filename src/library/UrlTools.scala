package library

import java.net.URL

import scala.collection.JavaConverters._
import org.htmlcleaner.BaseToken
import org.htmlcleaner.HtmlCleaner
import org.htmlcleaner.HtmlNode
import org.htmlcleaner.TagNode

import scala.collection.convert.ImplicitConversions.`list asScalaBuffer`


trait UrlTools {
  /** Retourne la structure HTML parsée dans l'URL donnée.
   *
   * @param url l'URL à lire
   * @return la structure Html trouvée à l'URL url
   * @throws TagNodeConversionException en cas d'URL mal formée ou problème de connexion internet
   */
  def fetch(url: String): Html

  /** Donne le nom d'hôte d'une URL ou la chaîne vide.
   *
   * @param url une URL
   * @return le nom de l'hôte ou la chaîne vide si l'analyse de l'URL échoue
   */
  def host(url: String): String

  /** Combine une URL de la forme http://.... avec une URL locale de la forme: local.html et produit
   * http://.../local.html
   *
   * @param base  l'URL base
   * @param local l'URL locale à combiner avec base
   * @return l'URL combinée ou la chaîne vide sinon.
   * @throws java.net.MalformedURLException si les URLs sont mal formées
   */
  def combineUrls(base: String, local: String): String
}

/** L'objet parseur d'URL */

object UrlProcessor extends UrlTools {

  // Use HtmlCleaner to obtain a valid (X)Html document 
  val cleaner = new HtmlCleaner
  val props = cleaner.getProperties
  props.setOmitUnknownTags(true)
  props.setOmitCdataOutsideScriptAndStyle(true)
  props.setOmitComments(true)
  props.setTranslateSpecialEntities(true)
  props.setTransResCharsToNCR(true)
  props.setRecognizeUnicodeChars(true)
  props.setUseCdataForScriptAndStyle(true)

  /** Donne le nom d'hôte d'une URL ou la chaîne vide.
   *
   * @param url une URL
   * @return le nom de l'hôte ou la chaîne vide si l'analyse de l'URL échoue
   */

  def host(url: String): String = {
    try {
      val host = (new URL(url)).getHost
      if (host == "") ""
      else "http://" + host + "/"
    }
    catch {
      case _: Throwable => ""
    }
  }

  /** Retourne la structure HTML parsée dans l'URL donnée.
   *
   * @param url l'URL à lire
   * @return la structure Html trouvée à l'URL url
   * @throws TagNodeConversionException en cas d'URL mal formée ou problème de connexion internet
   */

  def fetch(url: String): Html = {
    try {
      val u = new URL(url)
      val urlc = u.openConnection()
      // pour faire croire au site que l'on est un browser, on définit un User-Agent
      urlc.setRequestProperty("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:84.0) ")
      val inputFile = urlc.getInputStream()
      val rootNode = cleaner.clean(inputFile)
      tag2Html(rootNode)
    } catch {
      case _: java.net.MalformedURLException => println("Malformed URL: " + url); throw TagNodeConversionException("Malformed URL")
      case _: java.net.UnknownServiceException => println("Unknown Service on " + url); throw TagNodeConversionException("Unknown Service")
      case _: java.net.ConnectException => println("Connect Exception on " + url); throw TagNodeConversionException("Connect Exception")
      case _: java.io.IOException => println("IO Exception on " + url); throw TagNodeConversionException("IO Exception")
      case _: java.lang.NullPointerException => println("HtmlCleaner Bug on " + url); throw TagNodeConversionException("Bug HtmlCleaner!")
    }
  }

  /** converts Java list to Scala Lists
   *
   * @param l a Java list
   * @return a Scala list
   */

  private def javaList2ScalaList[T](l: java.util.List[T]): List[T] = l.toBuffer.toList


  /** converts any Token to an Html  tree (not cleant)
   *
   * @param tn the Token
   * @return the HtmlElement
   *
   *
   *
   *         Note that the default type of values returned by the cleaner is TagNode... however,
   *         this type is a subtype of HtmlNodes that contains text elements. Furthermore when using
   *         getAllChildren on a tagNode we get a list of ... BaseToken... we thus need this trick to
   *         recursively crawl in the html tree and grab both tags and contents...
   */

  private def tag2Html(tn: BaseToken): Html = {
    // getAllChildren gives all sons but not of Tag type, this is because
    // a text is not of this type
    // getAllChildTags gives only tags (and no texts)
    tn match {
      case tn: TagNode => {
        val children = javaList2ScalaList(tn.getAllChildren).asInstanceOf[List[BaseToken]]
        (tn.getName, children) match {
          case (name, _) => {
            val sons = children.map(tag2Html(_))
            var attributes = List[(String, String)]()
            for ((k, v) <- tn.getAttributes.asScala) {
              if (v != null) attributes = (k, v) :: attributes
            }
            Tag(name, attributes, sons)
          }
        }
      }
      case ht: HtmlNode => Text(tn.toString)
      case _ => Text("")
    }
  }


  /** Combine une URL de la forme http://.... avec une URL locale de la forme: local.html et produit
   * http://.../local.html
   *
   * @param base  l'URL base
   * @param local l'URL locale à combiner avec base
   * @return l'URL combinée ou la chaîne vide sinon.
   * @throws java.net.MalformedURLException si les URLs sont mal formées
   */

  def combineUrls(base: String, local: String): String = {
    // We use the URL constructor using a context (another URL) to expand relative urls
    val url = new URL(base)
    try {
      (new URL(url, local)).toString
    }
    catch {
      case _: java.net.MalformedURLException => ""
    }
  }
}

case class TagNodeConversionException(m: String) extends Exception
