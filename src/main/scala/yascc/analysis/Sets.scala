package yascc.analysis

import yascc.tree.Trees._
import yascc.util.Result

trait Sets {
  self: Phases =>
    
    object SetsPhase extends Phase {
      def apply(in: Result[Tree]): Result[Tree] = ???
    }
}
