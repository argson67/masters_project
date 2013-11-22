package yascc.analysis

import org.kiama.attribution.Attribution

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
        case tb@TreeBranch(name, _) =>
          val parentOpt = tb.getParentType
          val myTpe = Trait(name, parentOpt)
          tb.tpe = myTpe
          defineType(name, myTpe setPos t.pos)
        case tl@TreeLeaf(name, params) =>
          val parentOpt = tl.getParentType
          val tpe = CaseClass(name, params, parentOpt) setPos t.pos
          val funTpe = FunctionType(params map (_._2), tpe)
          tl.tpe = tpe
          defineType(name, tpe) :+ defineValue(name, t.pos, funTpe)
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
        case f: File => 
          Attribution.initTree(f)
          in :+ init(f) :+ countRefs(f)
        case other => error(s"Root of the AST is not a File but '$other'")
      }
    }
}
