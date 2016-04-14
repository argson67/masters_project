package yascc

import input.FileParser
import tree.Trees._
import util.Result
import symtab.SymbolTable
import settings.Settings
import analysis.Phases
import output.Targets

class Yascc() extends Phases
with SymbolTable
with Settings 
with Targets {
  val parser = new FileParser()

  def readFile(fileName: String): Result[Tree] = parser.readFile(fileName)

  private val phases: Seq[Result[Tree] => Result[Tree]] = 
    List(RewritePhase.apply, InitPhase.apply, RewriteTypesPhase.apply, SetsPhase.apply)//, TyperPhase.apply)

  def run(tree: Result[Tree]): Result[Tree] = 
    (phases reduce (_ andThen _))(tree)
}
