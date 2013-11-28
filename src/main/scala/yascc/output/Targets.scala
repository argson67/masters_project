package yascc.output

import yascc.Yascc
import yascc.symtab.SymbolTable
import yascc.settings.Settings

trait Targets extends SymbolTable with Settings {
  self: Yascc =>
}