package yascc.analysis

import yascc.tree.Trees._
import yascc.util.Result

trait Rewriting {
  self: Phases =>

  import yascc.util.Implicits._
  
  object RewritePhase extends Phase {

    // TODO: fix moar things?

    private def fixTree(t: Tree): Result[Tree] = rewrite(t) {
      case Commit(o@ Opt(_)) => o.failure("Cannot commit after an option", o.pos)
      case Rep(Commit(r), s, strict) => Rep(r, s, strict).failure("Commit inside a rep(sep)", r.pos)
      //case Rep(r, Some(Commit(s)), strict) => Rep(r, Some(s), strict).failure("Commit inside a repsep", s.pos)
      case Label(Commit(t), name) => Commit(Label(t, name))
      case Commit(r: Rep) => r.failure("Rep inside a commit. Don't do it.", r.pos)

      case Rep(Discard(x), s, strict) => Rep(x, s, strict).failure("Discard inside a rep. Don't do it.", t.pos)
    }

    def apply(in: Result[Tree]): Result[Tree] = {
      in flatMap fixTree
    }
  }
}