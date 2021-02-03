

import library.ExpressionParser
import java.awt.Desktop
import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import SearchPage.{extractKeywordsAnd, extractKeywordsOr}

object Application extends App {
  var isLoop = true

  while (isLoop) {
    val queryParsed = ExpressionParser.readExp

    val query = queryParsed.convertToList()

//    val links = query.map(keywordsElement => keywordsElement.mkString("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=", "+", "&cat_1=&geosearch_text=&searchGeoId=0"))

    val links = extractKeywordsOr(query)
    
    println("⏱ Chargement des annonces en cours....")
    val annonces: List[Annonce] = Annonce.urlsToAnnonces(links)

    println("🚮 Supression des annonces en double...")
    var annoncesFiltered: List[Annonce] = List()

    annonces.foreach(annonce => {
      if (!annoncesFiltered.exists(annonceToFind => annonceToFind.url equals annonce.url)) {
        annoncesFiltered = annoncesFiltered :+ annonce
      }
    }
    )
    println(s"✅ ${annoncesFiltered.length} annonce(s) ont été chargée(s) et ${annonces.length - annoncesFiltered.length} annonce(s) a/ont été(s) ignorée(s) car en double")

    val path = Paths.get("out")
    val fileName = query.map(queryElement => queryElement.mkString("&")).mkString("+")
    val pathWithFile = Paths.get(path.toString + s"/$fileName.html")
    Files.createDirectories(path)
    new PrintWriter(pathWithFile.toFile) {
      write(Annonce.annoncesToWebpage(query.map(queryElement => queryElement.mkString("(", " et ", ")")).mkString(" ou "), annoncesFiltered))
      close()
    }


    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      println(s"🌐 Le result a été ouvert dans votre navigateur. Il est aussi disponible ici: ${pathWithFile.toAbsolutePath}")
      Desktop.getDesktop.browse(pathWithFile.toUri)
    } else {
      println(s"💾 Nous n'avons pas réussi à ouvrir le result dans votre navigateur. Il est cependant disponible ici: ${pathWithFile.toAbsolutePath}")
    }

    println("Voulez vous effectuer une nouvelle recherche ? (y/N)")
    val answer = scala.io.StdIn.readLine()

    answer.toLowerCase match {
      case "y" =>
      case _ => println("👋 Bonne journée !"); isLoop = false
    }
  }


}
