package library

import scala.io.StdIn._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/** La case classe des expression */
sealed trait Expression {
  def convertToList(): List[List[String]] = {
    this match {
      case Word(text) => List(List(text))
      case And(e1, e2) => (e1, e2) match {
        case (Word(_), Or(orE1, orE2)) => Or(And(e1, orE1), And(e1, orE2)).convertToList()
        case (Or(orE1, orE2), Word(_)) => Or(And(e2, orE1), And(e2, orE2)).convertToList()
        case (Or(or1E1, or1E2), Or(or2E1, or2E2)) => Or(Or(And(or1E1, or2E1), And(or1E1, or2E2)), Or(And(or1E2, or2E1), And(or1E2, or2E2))).convertToList()
        case _ => List(e1.convertToList().flatten ++ e2.convertToList().flatten)
      }
      case Or(e1, e2) => e1.convertToList() ++ e2.convertToList()
    }
  }
}

case class Word(w: String) extends Expression

case class And(e1: Expression, e2: Expression) extends Expression

case class Or(e1: Expression, e2: Expression) extends Expression

/** Le parseur d'expressions */
object ExpressionParser {

  /** La méthode principale du parseur: lit (au clavier) une chaîne de caractères de la forme "((toto and titi) and (tata or tutu))" et produit une 
   * expression. La méthode itère le processus jusqu'à ce qu'une chaîne de caractère pouvant être transformée en une expression
   * est tapée.
   *
   * @return l'expression résultat du parsing.
   */
  def readExp = {
    var rep = ""
    var query: Expression = Word("")
    while (rep == "") {
      println("Donnez votre requète sous forme de mots clés et combinés avec and/or\nPar exemple: developpeur and (rennes or nantes) and (python or java)")
      rep = readLine()
      val p = LocalParser.parse(rep)
      if (p.successful) query = p.get
      else {
        println("Malformed query!"); rep = ""
      }
    }
    query
  }

  /** L'objet local qui implémente le parseur */
  object LocalParser extends StdTokenParsers {
    type Tokens = StdLexical

    val lexical = new StdLexical()

    lexical.reserved += ("and", "or")
    lexical.delimiters ++= List("(", ")")

    /** Lit au clavier une chaîne de caractères de la forme "((toto and titi) and (tata or tutu))" et produit une liste
     * des identifiants recontrés dans la chaîne.
     * @param s la chaîne de caractères à analyser
     * @return un objet de type parseResult qui contient l'expression résultat du parsing, si le parsing a réussi.
     */

    def parse(s: String): ParseResult[Expression] = {
      expr(new lexical.Scanner(s))
    }

    // the parser itself
    def expr: Parser[Expression] = andExp | orExp | unExp

    def factor: Parser[Expression] = "(" ~> expr <~ ")"

    def andExp: Parser[Expression] = (unExp ~ "and" ~ expr ^^ { case x ~ _ ~ y => And(x, y) })

    def orExp: Parser[Expression] = (unExp ~ "or" ~ expr ^^ { case x ~ _ ~ y => Or(x, y) })

    def unExp: Parser[Expression] = word | factor

    def word: Parser[Expression] = (
      ident ^^ { case s => Word(s) }
        | numericLit ^^ { case i => Word(i.toString) })
  }

}