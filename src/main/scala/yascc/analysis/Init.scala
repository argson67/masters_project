package yascc.analysis

import yascc.tree.Trees._
import yascc.util.Result

import org.kiama.attribution.Attribution._

trait Init {
  self: Phases =>

    object InitPhase extends Phase {
      import yascc.util.Implicits._

      private def initSection: Function1[Section, Result[Unit]] = {
        case Settings(defs) => Result.chain(defs)(initSetting)
        case Declarations(decls) => Result.chain(decls)(initDecl)
        case TreeDefs(defs) => Result.chain(defs)(initTreeDef)
        case Grammar(rules) => Result.chain(rules)(initRule)
      }

      private def initRule(r: Rule): Result[Unit] = ???

      private def initDecl(d: Declaration): Result[Unit] = ???

      private def initTreeDef(td: TreeDef): Result[Unit] = ???

      private def initSetting(s: Setting): Result[Unit] = ???

      private def init(f: File): Result[Unit] = 
        Result.chain(f.sections)(initSection)

      def apply(in: Result[Tree]): Result[Tree] = in flatMap {
        case f: File => in :+ init(f)
        case other => error(s"Root of the AST is not a File but '$other'")
      }
    }
}
