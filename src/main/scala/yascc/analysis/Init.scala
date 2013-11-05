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

      private def initRule(r: Rule): Result[Unit] = {
        registerRule(r)
        defineRule(r) match {
          case Some(e) => Result.unit.addError(e)
          case None => Result.unit
        }
      }

      private def initDecl(d: Declaration): Result[Unit] = {
        defineValue(d) match {
          case Some(e) => Result.unit.addError(e)
          case None => Result.unit
        }
      }

      private def initTreeDef(td: TreeDef): Result[Unit] = {
        val tpe = td match {
          case TreeBranch(name, children) =>
            Result.chain(children)(initTreeDef) +: Trait(name).success
          case TreeLeaf(name, params) =>
            CaseClass(name, params).success
        }

        tpe flatMap (tpe =>
          defineType(td.name, tpe) match {
            case Some(e) => Result.unit.addError(e)
            case None => Result.unit
        })
      }

      private def initSetting(s: Setting): Result[Unit] = {
        setSetting(s.name, s.value)
        Result.unit
      }

      private def init(f: File): Result[Unit] = 
        Result.chain(f.sections)(initSection)

      def apply(in: Result[Tree]): Result[Tree] = in flatMap {
        case f: File => in :+ init(f)
        case other => error(s"Root of the AST is not a File but '$other'")
      }
    }
}
