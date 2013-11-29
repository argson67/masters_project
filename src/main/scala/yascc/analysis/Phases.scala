package yascc.analysis

import yascc.Yascc
import yascc.symtab.SymbolTable
import yascc.settings.Settings
import yascc.tree.Trees._
import yascc.util.{ Result, Error, Warning }

trait Phases extends SymbolTable
with Settings
with Init
with Sets 
with Rewriting
with Dependencies
with Typechecker {
  self: Yascc =>

    trait Phase extends Function1[Result[Tree], Result[Tree]] {

    }

    def runPhase(p: Phase, in: Result[Tree]): Result[Tree] = p(in)
}
