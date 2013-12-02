package yascc.output

import org.kiama.output.PrettyPrinter

import yascc.tree.Trees._

trait DefsOut {
  self: Targets =>

    abstract class AbstractDefsTarget extends Target with Utils with PrettyPrinter {
      private lazy val _defs = allValues.toList

      protected def printDef(name: String, tpe: ScalaType): Doc

      protected val className: Doc

      private def printDefs: Doc = {
        val packageDoc = "package" <+> packageName
        val defs = _defs map {
          case (n, tpe) => printDef(n, tpe)
        } reduceOption (_ <@@@> _) getOrElse empty

        packageDoc <@@@> className <+> braces1(defs)
      }

      def output: String = pretty(printDefs)
    }

    object DefsTarget extends AbstractDefsTarget {
      protected def printDef(name: String, tpe: ScalaType): Doc = {
        "def" <+> name <> ":" <+> printType(tpe)
      }

      val filename = utilsName + ".scala"
      protected val className = 
        if (numTrees > 0) {
          "trait" <+> utilsName <+> "extends" <+> treesName <+> "with" <+> treesImplName
        } else {
          "trait" <+> utilsName
        }
    }

    object DefsImplTarget extends AbstractDefsTarget {
      protected def printDef(name: String, tpe: ScalaType): Doc = {
        "def" <+> name <> ":" <+> printType(tpe) <+> "=" <+> "???"
      }

      val filename = utilsImplName + ".scala"
      protected val className = 
        if (numTrees > 0) {
          "trait" <+> utilsImplName <+> "extends" <+> utilsName <+>
            "with" <+> treesName <+> "with" <+> treesImplName
        } else {
          "trait" <+> utilsImplName <+> "extends" <+> utilsName
        }
    }
}
