package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait TreesOut {
  self: Targets =>

    abstract class AbstractTreesTarget extends Target with Utils with PrettyPrinter {
      private lazy val _types = allTypes.toList

      protected def printNode(t: TreeType): Doc

      protected val className: Doc

      private val imports = List("scala.util.parsing.input.Positional")

      private def printTrees: Doc = {
        val packageDoc = "package" <+> packageName
        val importsDoc = imports map ("import" <+> _) reduce (_ <@> _)
        val trees = _types map printNode reduceOption (_ <@@@> _) getOrElse empty

        packageDoc <@@@> importsDoc <@@@> className <+> braces1(trees)
      }

      def output: String = pretty(printTrees)
    }

    object TreesTarget extends AbstractTreesTarget {
      protected def printNode(t: TreeType): Doc = {
        t match {
          case Trait(name, Some(p)) =>
            "type" <+> name <+> "<:" <+> p.name <+> "with" <+> "Positional"
          case Trait(name, None) =>
            "type" <+> name <+> "<:" <+> "Positional"
          case CaseClass(name, params, Some(p)) =>
            "type" <+> name <+> "<:" <+> p.name <+> "with" <+> "Positional"
          case CaseClass(name, params, None) =>
            "type" <+> name <+> "<:" <+> "Positional"
          case _ => empty
        }
      }

      val filename = treesName + ".scala"
      protected val className = "trait" <+> treesName
    }

    object TreesImplTarget extends AbstractTreesTarget {
      private def printParams(params: List[(String, ScalaType)]) =
        group(parens1(lsep(params map {
          case (name, tpe) => name <> ":" <+> printType(tpe)
        }, comma)))

      protected def printNode(t: TreeType): Doc = {
        t match {
          case Trait(name, Some(p)) =>
            "trait" <+> name <+> "extends" <+> p.name <+> "with" <+> "Positional" <+> braces1(empty)
          case Trait(name, None) =>
            "trait" <+> name <+> "extends" <+> "Positional" <+> braces1(empty)
          case CaseClass(name, params, Some(p)) =>
            "case class" <+> name <> printParams(params) <+> "extends" <+> p.name <+> "with" <+> "Positional" <+> braces1(empty)
          case CaseClass(name, params, None) =>
            "case class" <+> name <> printParams(params) <+> "extends" <+> "Positional" <+> braces1(empty)
          case _ => empty
        }
      }

      val filename = treesImplName + ".scala"
      protected val className = "trait" <+> treesImplName <+> "extends" <+> treesName
    }
}