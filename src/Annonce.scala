import library._

class Annonce(html: Html) {
  var url: String = ""
  var title: String = ""
  var coverImage: String = ""

  //  1. Récupération du lien de l'annonce
  val listingLink: Option[Html] = html.findOneElementByClassName("clad__ad_link")
  //  2. Récupération de l'image de l'annonce
  val listingImage: Option[Html] = html.findOneElementByClassName("clad__image")

  listingLink match {
    case Some(listingLinkValue) => {
      url = listingLinkValue.getAttributeValue("href")
      val listingTitle = listingLinkValue.getFirstElementByTag("h2")
      listingTitle match {
        case Some(listingTitleValue) => title = listingTitleValue.getText
        case None => throw new IllegalArgumentException("listing title not found")
      }

    }
    case None => throw new IllegalArgumentException("listing link not found")
  }

  listingImage match {
    case Some(listingImageValue) => coverImage = listingImageValue.getAttributeValue("src")
    case None => coverImage = ""
  }


  /**
   * Génère l'annonce au format HTML sous forme d'une String
   *
   * @return L'annonce au format HTML sous forme d'une String
   */
  def toHTMLString: String = {
    s"""
      |  <a href="${this.url}" target="_blank" class="listing">
      |    <div class="listing__overlay"></div>
      |    <img class="listing__image"
      |         src="${this.coverImage}"
      |         alt="">
      |    <main class="listing__content">
      |      <div></div>
      |      <div>${this.title}</div>
      |    </main>
      |  </a>
      |""".stripMargin
  }

}

object Annonce {
  /**
   * Création d'annonces depuis l'URL d'une page de recherche
   *
   * @param url l'url de la page de recherche
   * @return une liste d'annonces présent sur la page
   */
  def urlToAnnonces(url: String): List[Annonce] = {
    //    url is empty
    if (url.length <= 0) throw new IllegalArgumentException("url is empty")
    //    Bad URL format
    if (!url.contains("search.vivastreet.com/annonces")) throw new IllegalArgumentException("url is not a vivastreet search url")

    val pageHTML = UrlProcessor.fetch(url)
    val listings: List[Html] = pageHTML.findElementsByClassName("kiwii-clad-row")
    listings.map(listing => new Annonce(listing))
  }

  /**
   * Création d'annonces depuis l'URL de plusieurs pages de recherche
   *
   * @param urls les urls des pages de recherche
   * @return les annonces présentes sur les différentes pages
   */
  def urlsToAnnonces(urls: List[String]): List[Annonce] = {
    urls match {
      case Nil => Nil
      case url :: reste => urlToAnnonces(url) ++ urlsToAnnonces(reste)
    }
  }


  def annoncesToWebpage(search: String, annonces: List[Annonce]): String = {
    var stringAnnonces = ""

    annonces.foreach(annonce => stringAnnonces += annonce.toHTMLString)


    s"""
       |<!DOCTYPE html>
       |<html lang="fr">
       |<head>
       |  <meta charset="UTF-8">
       |  <title>Résultats de recherche pour: $search</title>
       |</head>
       |<body>
       |
       |<style>
       |    @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@700&display=swap');
       |
       |    body {
       |        font-family: 'Roboto', sans-serif;
       |    }
       |
       |    .listing-grid {
       |        display: grid;
       |        grid-template-columns: repeat(5, 1fr);
       |        grid-column-gap: 10px;
       |        grid-row-gap: 10px;
       |    }
       |
       |
       |    .listing {
       |        display: flex;
       |        flex-direction: column;
       |        border-radius: 10px;
       |        overflow: hidden;
       |        height: 200px;
       |        position: relative;
       |        cursor: pointer;
       |        transition: all 0.2s ease-in-out;
       |        text-decoration: none;
       |    }
       |
       |    .listing:hover {
       |        box-shadow: #676767 0px 0px 10px;
       |        transform: scale(1.02);
       |    }
       |
       |    .listing__overlay {
       |        width: 100%;
       |        height: 100%;
       |        background: rgb(0,0,0);
       |        background: linear-gradient(0deg, rgba(0,0,0,1) 0%, rgba(0,0,0,0) 100%);
       |        position: absolute;
       |        z-index: 1;
       |    }
       |    .listing__image {
       |        object-fit: cover;
       |        position: absolute;
       |        width: 100%;
       |        height: 100%;
       |        background-color: #3894c4;
       |    }
       |    .listing__content {
       |        width: 100%;
       |        height: 100%;
       |        display: flex;
       |        flex-direction: column;
       |        justify-content: space-between;
       |        z-index: 100;
       |        color: aliceblue;
       |        padding: 20px;
       |        box-sizing: border-box;
       |        font-size: 1.3em;
       |    }
       |</style>
       |
       |
       |
       |<h1>Résultats de recherche pour : $search</h1>
       |<h2>${annonces.length} résulats</h2>
       |
       |
       |<div class="listing-grid">
       |
       |$stringAnnonces
       |
       |
       |
       |
       |</div>
       |
       |
       |
       |</body>
       |</html>
       |""".stripMargin
  }

}