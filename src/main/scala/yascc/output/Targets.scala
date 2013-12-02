package yascc.output

import java.io.{ FileWriter, PrintWriter, File }

import yascc.Yascc
import yascc.symtab.SymbolTable
import yascc.settings.Settings

trait Targets extends SymbolTable 
with Settings
with Names
with ParserOut 
with TreesOut 
with DefsOut 
with GrammarOut {
  self: Yascc =>
  
  import scala.language.reflectiveCalls
  
  protected def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  protected def writeToFile(path: String, data: String): Unit = {
    val f = new File(path)
    f.getParentFile().mkdirs()
    f.createNewFile()
    val writer = new FileWriter(f)
    using(writer)(_.write(data))
  }

  //protected def appendToFile(path: String, data: String): Unit =
  //  using(new PrintWriter(new FileWriter(path, true)))(_.println(data))

  trait Target {
    val filename: String
    def output: String

    def apply(): Unit = {
      writeToFile(getSettingT[String]("packageName") + "/" + filename, output)
    }
  }
}