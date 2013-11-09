package yascc.analysis

import yascc.tree.Trees._
import yascc.util.{ Result, UResult }

trait Init {
  self: Phases =>

    object InitPhase extends Phase {
      import yascc.util.Implicits._

      def init(t: Tree) = query(t) {
        case Setting(name, value) => 
          setSetting(name, value)
          OK
        case decl: Declaration =>
          defineValue(decl)
        case TreeBranch(name, _) =>
          defineType(name, Trait(name) setPos t.pos)
        case TreeLeaf(name, params) =>
          defineType(name, CaseClass(name, params) setPos t.pos)
        case r: Rule =>
          registerRule(r)
          defineRule(r)
      }

      def countRefs(t: Tree) = query(t) {
        case NonTerminal(name) =>
          for   (r <- lookupRule(name, t.pos))
          yield (r.r.refCount += 1)
      }

      def apply(in: Result[Tree]): Result[Tree] = in flatMap {
        case f: File => in :+ init(f) :+ countRefs(f)
        case other => error(s"Root of the AST is not a File but '$other'")
      }
    }
}
