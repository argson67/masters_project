package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait ParserOut {
  self: Targets =>

    object ParserTarget extends Utils with PrettyPrinter {
      private def funCall(f: Doc, params: Doc*) = 
        parens(nest(lsep(params, comma)))

      private def recover(e: ProductionElem): Doc = e match {
        case NonTerminal(name) =>
          val recSet = funCall("Set", (lookupRule(name).get.r.followSet.toSeq map printProdElem): _*)
          funCall("recoverSkip", name, recSet)
        case NumberLiteral(num) =>
          funCall("recoverInsert", dquotes(num.toString))
        case StringLiteral(str) =>
          funCall("recoverInsert", dquotes(str))
        case CharLiteral(c) =>
          funCall("recoverInsert", dquotes(c))
        case other => printProdElem(other)
      }

      private def printConjunction(elems: List[ProductionElem], sep: Doc = empty, rec: Boolean = false): Doc = {
        val p = if (rec) {
          recover _
        } else {
          printProdElem _
        }

        elems match {
          case Nil => empty
          case last :: Nil => sep <> p(last)
          case Commit(Discard(x)) :: xs if xs != Nil => 
            sep <> p(x) <> printConjunction(xs, "~!>" <> space, true)
          case Discard(Commit(x)) :: xs if xs != Nil => 
            sep <> p(x) <> printConjunction(xs, "~!>" <> space, true)
          case Commit(x) :: xs if xs != Nil =>
            sep <> p(x) <> printConjunction(xs, "~!" <> space, true)
          case Discard(x) :: xs if xs != Nil =>
            sep <> p(x) <> printConjunction(xs, "~>" <> space)
          case x1 :: Discard(x2) :: Nil =>
            sep <> p(x1) <> printConjunction(List(x2), "<~" <> space)
          case x :: xs =>
            sep <> p(x) <> printConjunction(xs, tilde <> space)
        }
      }

      private def actionPattern(arity: Int, f: Doc = empty): Doc = {
        if (arity > 1) {
          val args = for (i <- 1 to arity) yield text(s"x$i")
          "case" <+> lsep(args, space <> tilde) <+> "=>" <+> f <> parens(lsep(args, comma))
        } else {
          "x" <+> "=>" <+> f <> parens("x")
        }
      }

      private def printAction(a: Action, arity: Int): Doc = {
        a match {
          case FunctionAction(fun) =>
            import yascc.util.Success
            val n = lookupType(fun) match { // yeah, yeah, redundant lookups
              case Success(v, _) =>
                fun.canonicalName <> ".apply"
              case _ =>
                text(fun.canonicalName)
            }

            braces(nest(actionPattern(arity, n)))

          case CustomAction(code, tpe) =>
            braces(nest(code))

          case DefaultAction =>
            braces(nest(actionPattern(arity)))
        }
      }

      private def wrap(d: Doc, wd: Doc): Doc = 
        d <> parens(wd)

      private def printProduction(p: Production): Doc = 
        parens(printProdElem(p.body) <+> "^^" <+> printAction(p.action, p.arity))

      private def printRule(r: Rule): Doc = {
        val name = r.option("name").flatten.getOrElse(r.name)
        val label = r.option("label").map(_.getOrElse(r.name))
        val tpe = lookupRule(r.name).get.tpe

        val positioned: Boolean = getSettingT("positioned") && tpe.isPositional
        val packrat: Boolean = getSettingT("packrat")

        val ruleT = (code: Doc) => label match {
          case Some(l) => if (name == l) {
            "lrule" <> parens(dquotes(name)) <+> braces(nest(code))
          } else {
            wrap("label" <> parens(dquotes(l)), 
              parens("rule" <> parens(dquotes(name)) <+> braces(nest(code))))
          }
          case None => "rule" <> parens(dquotes(name)) <+> braces(nest(code))
        }

        val posT = if (positioned) {
          (wrap("positioned", _: Doc)) compose ruleT
        } else {
          ruleT
        }

        val code = parens(nest(lsep(r.productions map printProduction, softline <> "|")))

        val parserType = (t: Doc) => (if (packrat) "PackratParser" else "Parser") <> brackets(t) 

        "lazy val" <+> r.name <> ":" <+> parserType(printType(tpe)) <+> "=" <@> nest(posT(code))
      }


      private def printProdElem(pe: ProductionElem): Doc = pe match {
        // Production elements
        case Conjunction(elems) =>
          parens(printConjunction(elems))
        case Disjunction(elems) =>
          parens(nest(lsep(elems map printProdElem, softline <> "|")))
        case Opt(elem) =>
          "opt" <> parens(printProdElem(elem))
        case Commit(elem) =>
          printProdElem(elem)
        case Discard(elem) =>
          printProdElem(elem)
        case Rep(elem, Some(Commit(s)), strict) =>
          val f = if (strict) "rep1sepc" else "repsepc"
          funCall(f, recover(elem), printProdElem(s), printProdElem(elem))
        case Rep(elem, Some(s), strict) =>
          val f = if (strict) "rep1sep" else "repsep"
          funCall(f, printProdElem(elem), printProdElem(s))
        case Rep(elem, None, strict) =>
          val f = if (strict) "rep1" else "rep"
          funCall(f, printProdElem(elem))
        case Label(elem, label) =>
          "label" <> parens(printProdElem(elem) <> comma <+> label)
        case NonTerminal(name) =>
          name

        // Terminals
        case NumberLiteral(num) =>
          dquotes(num.toString)
        case StringLiteral(str) =>
          dquotes(str)
        case RegexLiteral(regex) =>
          dquotes(regex) <> ".r"
        case CharLiteral(c) =>
          squotes(c)
        case Epsilon => empty
      }

      def apply() = ???
    }
}
