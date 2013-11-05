package yascc.analysis

import yascc.Yascc
import yascc.symtab.SymbolTable
import yascc.tree.Trees._
import yascc.util.{ Result, Error, Warning }

trait Phases extends SymbolTable
with Init
with Sets {
  self: Yascc =>

    trait Phase extends Function1[Result[Tree], Result[Tree]] {

    }
}
