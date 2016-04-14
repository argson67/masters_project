package yascc.util

import scala.collection.mutable. { Map => MMap }

import yascc.tree.Trees._

object FollowSetPrinter {
  private val literals = MMap.empty[String, String]
  private val regexes = MMap.empty[String, String]

  private def regexName(s: String): String =
    regexes.getOrElse(s, {
      val res = s"regex${regexes.size}"
      regexes(s) = res
      res
    })

  private def literalName(s: String): String =
    literals.getOrElse(s, {
      val res = s"literal${literals.size}"
      literals(s) = res
      res
    })

  def apply(rules: Seq[Rule]): String = {
    val fset = "val followSet = Map(%s)".format(rules map {
      r =>
        val followSet = r.followSet

        val pSet = followSet map {
          case NumberLiteral(num) =>
            "literal(\"%s\")".format(num)
          case StringLiteral(str) =>
            "%s // %s".format(literalName(str), str)
          case RegexLiteral(regex) =>
            "%s // %s".format(regexName(regex), regex)
          case CharLiteral(c) =>
            "%s // %s".format(literalName(c.toString), c)
          case Epsilon => "epsilon"
        }

        "(\"%s\" -> Set(\n  %s\n))".format(r.name, 
          pSet mkString "\n  ,")
      } mkString ",\n")

    val rs = regexes map {
      case (r, name) => s"private lazy val $name = regex(" + "\"" + r + "\".r)"
    } mkString "\n"

    val ls = literals map {
      case (l, name) => s"private lazy val $name = literal(" + "\"" + l + "\")"
    } mkString "\n" 

    rs + "\n\n" + ls + "\n\n" + fset
  }
}