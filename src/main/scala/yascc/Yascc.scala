package yascc

import input.FileParser
import tree.Trees._
import util.Result
import symtab.SymbolTable
import settings.Settings
import analysis.Phases

class Yascc() extends Phases
with SymbolTable
with Settings {
  val parser = new FileParser()

  def readFile(fileName: String): Result[Tree] = parser.readFile(fileName)
}
