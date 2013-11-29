package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait ParserOut {
  self: Targets =>

    object ParserTarget extends Target with Utils with PrettyPrinter {
      private def funCall(f: Doc, params: Doc*) = 
        group(f <> parens1(nest(lsep2(params, comma))))

      private def recover(e: ProductionElem, enclosed: Boolean = false): Doc = e match {
        case NonTerminal(name) =>
          val recSet = funCall("Set", (lookupRule(name).get.r.followSet.toSeq map printProdElem): _*)
          funCall("recoverSkip", name, recSet)
        case NumberLiteral(num) =>
          funCall("recoverInsert", dquotes(num.toString))
        case StringLiteral(str) =>
          funCall("recoverInsert", dquotes(str))
        case CharLiteral(c) =>
          funCall("recoverInsert", dquotes(c))
        case other => printProdElem(other, enclosed)
      }

      private def printConjunction(elems: List[ProductionElem], rec: Boolean = false): Doc = {
        def mp(d: Doc, xs: List[Any]) = xs match {
          case _ :: Nil => d
          case _ => parens(d)
        }

        val p = if (rec) {
          recover(_: ProductionElem, true)
        } else {
          printProdElem(_: ProductionElem, true)
        }

        group(elems match {
          case Nil => empty
          case last :: Nil => p(last)
          case Commit(Discard(x)) :: xs if xs != Nil => 
            p(x) <+> "~!>" <+> mp(printConjunction(xs, true), xs)
          case Discard(Commit(x)) :: xs if xs != Nil => 
            p(x) <+> "~!>" <+> mp(printConjunction(xs, true), xs)
          case Commit(x) :: xs if xs != Nil =>
            p(x) <+> "~!" <+> mp(printConjunction(xs, true), xs)
          case Discard(x) :: xs if xs != Nil =>
            p(x) <+> "~>" <+> mp(printConjunction(xs), xs)
          case x1 :: Discard(x2) :: Nil =>
            p(x1) <+> "<~" <+> printConjunction(List(x2))
          case x :: xs =>
            p(x) <+> "~" <+> mp(printConjunction(xs), xs)
        })
      }

      private def matchCase(args: Seq[Doc]): Doc = args match {
        case Seq() => empty
        case Seq(x1) => x1
        case Seq(x1, x2) => x1 <+> tilde <+> x2
        case other => other.head <+> tilde <+> parens(matchCase(other.tail))
      }

      private def actionPattern(arity: Int, f: Doc = empty): Doc = {
        group(if (arity > 1) {
          val args = for (i <- 1 to arity) yield text(s"x$i")
          "case" <+> matchCase(args) <+> "=>" <+> f <> parens1(lsep2(args, comma))
        } else {
          "x" <+> "=>" <+> f <> "x"
        })
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

            braces1(actionPattern(arity, n))

          case CustomAction(code, tpe) =>
            braces1(code)

          case DefaultAction =>
            braces1(actionPattern(arity))
        }
      }

      private def wrap(d: Doc, wd: Doc): Doc = 
        d <> parens1(wd)

      private def printProduction(p: Production): Doc = 
        group(printProdElem(p.body) <+> "^^" <+> printAction(p.action, p.arity))

      private def printRule(r: Rule): Doc = {
        val name = r.option("name").flatten.getOrElse(r.name)
        val label = r.option("label").map(_.getOrElse(r.name))
        val tpe = lookupRule(r.name).get.tpe

        println(s"tpe = $tpe (${tpe.isPositional})")

        val positioned: Boolean = getSettingT("positioned") && tpe.isPositional
        val packrat: Boolean = getSettingT("packrat")

        val ruleT = (code: Doc) => label match {
          case Some(l) => if (name == l) {
            "lrule" <> parens1(dquotes(name)) <+> braces1(code)
          } else {
            wrap("label" <> parens1(dquotes(l)), 
              parens1("rule" <> parens1(dquotes(name)) <+> braces1(code)))
          }
          case None => "rule" <> parens1(dquotes(name)) <+> braces1(code)
        }

        val posT = if (positioned) {
          (wrap("positioned", _: Doc)) compose ruleT
        } else {
          ruleT
        }

        val code = (lsep(r.productions map printProduction, softline <> "|"))

        val parserType = (t: Doc) => (if (packrat) "PackratParser" else "Parser") <> brackets1(t) 

        "lazy val" <+> r.name <> ":" <+> parserType(printType(tpe)) <+> "=" <+> posT(code)
      }

      private def printProdElem(pe: ProductionElem): Doc =
        printProdElem(pe, false)

      private def printProdElem(pe: ProductionElem, enclosed: Boolean): Doc = {
        val res: Doc = pe match {
          // Production elements
          case Conjunction(elems) =>
            parens1(printConjunction(elems), enclosed && elems.size > 1)
          case Disjunction(elems) =>
            parens1(nest(lsep(elems map printProdElem, softline <> "|")), enclosed)
          case Opt(elem) =>
            funCall("opt", printProdElem(elem))
          case Commit(elem) =>
            printProdElem(elem, enclosed)
          case Discard(elem) =>
            printProdElem(elem, enclosed)
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
            "label" <> parens1(printProdElem(elem) <> comma <+> label)
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

        println(s"PRINTING $pe ($enclosed) -> ${pretty(res)}") 
        res
      }

      private val imports = List("yascc.combinators._")

      private def parserFile: Doc = {
        val packageDoc = "package" <+> packageName
        val importsDoc = imports map ("import" <+> _) reduce (_ <@> _)
        val withPackrat = if (getSettingT("packrat")) "with" <+> packratTrait <> space else empty

        val mixins = (if (numTrees > 0) List(treesName, treesImplName) else Nil) ++
          (if (numDefs > 0) List(utilsName, utilsImplName) else Nil)
        val mixinsDoc = mixins map (m => "with" <+> m) reduceOption (_ <+> _) getOrElse(empty)

        val objDoc = "object" <+> parserName <+> "extends" <+> parsersTrait <+> withPackrat <>
          mixinsDoc

        val applyDoc = "def apply(in: String) =" <+> braces1 {
          funCall("parseAll", startRule, "in") <+> "match" <+> braces1 {
            group("case Left(e) => println(e); scala.sys.error(\"Syntax error.\")" <@>
            "case Right(v) => v")
          }
        }

        val rulesDocs = rules map printRule reduce (_ <@@@> _)

        packageDoc <@@@> importsDoc <@@@> objDoc <+> braces1(rulesDocs <@@@> applyDoc)
      }

      def output = 
        pretty(parserFile, textWidth)

      val filename = parserName + ".scala"
    }
}
