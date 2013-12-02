package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait GrammarOut {
  self: Targets =>
    object GrammarTarget extends Target with Utils {
      private val header = "\\documentclass[10pt,letterpaper]{article}" <@>
        "\\usepackage{syntax}" <@>
        "\\begin{document}" <@>
        "\\section{Grammar}"

      private def latexQuotes(d: Doc) = "`" <> d <> "'"

      //private def latexDQuotes(d: Doc) = "\\\"" <> d <> "''"

      private def printRule(r: Rule): Doc = {
        angles(r.name) <+> "::=" <+> nest(printProductions(r.productions))
      }

      private def printProductions(prods: Seq[Production]): Doc = {
        prods match {
          case Seq() => empty
          case Seq(p) => printProdElem(p.body)
          case other => printProdElem(other.head.body) <@> vsep(other.tail map (p => "\\alt" <+> printProdElem(p.body)))
        }
      }

      private def printProdElem(e: ProductionElem): Doc = 
        printProdElem(e, false)

      private def printProdElem(e: ProductionElem, enclosed: Boolean): Doc = e match {
        // Production elements
        case Conjunction(elems) =>
          parens1(hsep(elems map printProdElem), enclosed && elems.size > 1)
        case Disjunction(elems) =>
          parens1(lsep(elems map printProdElem, space <> "|"), enclosed)
        case Opt(elem) =>
          printProdElem(elem, true) <> "?"
        case Commit(elem) =>
          printProdElem(elem, enclosed)
        case Discard(elem) =>
          printProdElem(elem, enclosed)
        case Rep(elem, sep, strict) =>
          val sym = if (strict) "+" else "*"
          sep match {
            case Some(s) => printProdElem(elem, true) <+> angles(sym) <+> printProdElem(s, true)
            case None => printProdElem(elem, true) <> sym
          }
        case Label(elem, label) =>
          printProdElem(elem, enclosed)
        case NonTerminal(name) =>
          angles(name)

        // Terminals
        case NumberLiteral(num) =>
          latexQuotes(num.toString)
        case StringLiteral(str) =>
          latexQuotes(str)
        case RegexLiteral(regex) =>
          "r" <> latexQuotes(regex)
        case CharLiteral(c) =>
          latexQuotes(c)
        case Epsilon => empty
      }

      private val footer = "\\end{document}"

      private val document = {
        val rulesDoc = rules map printRule reduceOption (_ <@@@> _) getOrElse(empty)
        val body = "\\begin{grammar}" <@> nest(rulesDoc) <@> "\\end{grammar}"
        header <@@@> body <@@@> footer
      }

      val filename = parserName + "Grammar.tex"
      def output = pretty(document, getSettingT[Int]("textWidth"))
    }
}