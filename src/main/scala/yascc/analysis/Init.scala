package yascc.analysis

import org.kiama.attribution.Attribution

import yascc.tree.Trees._
import yascc.util.{ Result, UResult }

trait Init {
  self: Phases =>

    object InitPhase extends Phase {
      import yascc.util.Implicits._

      private def init(t: Tree) = query(t) {
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

      private def countRefs(t: Tree) = query(t) {
        case NonTerminal(name) =>
          for   (r <- lookupRule(name, t.pos))
          yield (r.r.refCount += 1)
      }

      private def getStartRule(): Result[Unit] = {
        val marked = rules filter (_.hasOption("start"))
        if (marked.size == 1) {
          startRule = marked.head.name
          OK
        } else if (marked.size > 1) {
          failure("Multiple rules marked as start rules: " + (marked mkString ", "))
        } else {
          val noParents = rules filter (_.refCount == 0)
          if (noParents.size == 1) {
            startRule = noParents.head.name
            OK
          } else if (noParents.size > 1) {
            failure("Multiple rules have no parents; need explicit \"start\" annotation (also, wtf?): " + (noParents mkString ", "))
          } else {
            failure("Loopy rule structure; need explicit \"start\" annotation")
          }
        }
      }

      def apply(in: Result[Tree]): Result[Tree] = in flatMap {
        case f: File => 
          Attribution.initTree(f)
          val res = in :+ init(f) :+ countRefs(f)
          res :+ getStartRule()
        case other => error(s"Root of the AST is not a File but '$other'")
      }
    }
}
