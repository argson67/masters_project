package yascc

import tree.TreePrinter

object Main {
  def main(args: Array[String]): Unit = {
    //println("Hello world.")
    if (args.size != 1) {
      println("Exactly one argument required (filename)")
    } else {
      val fname = args(0)
      val instance = new Yascc()
      val tree = instance.readFile(fname)
      val initTree = instance.runPhase(instance.InitPhase, tree)
      instance.runPhase(instance.SetsPhase, tree)

      println(initTree map TreePrinter.apply)
      println(instance.listAll)
      println(instance.rules.filter(_.refCount == 0))
      println(instance.rules map (r => s"${r.name}: ${r.firstSet}") mkString "\n")
    }
  }
}
