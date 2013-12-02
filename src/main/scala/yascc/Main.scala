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
      val res = instance.run(tree)

      println(res map TreePrinter.apply)
      println(instance.listAll)
      
      println(instance.rules.filter(_.refCount == 0))
      println(instance.rules map (r => s"first(${r.name}): ${r.firstSet}") mkString "\n")

      println(instance.rules map (r => s"follow(${r.name}): ${r.followSet}") mkString "\n")

      println(instance.rules map (r => s"type(${r.name}): ${instance.getType(r.name).get}") mkString "\n") 

      println(res.printErrors)
      if (res.isSuccess) {
        //println("Output: \n" + instance.ParserTarget(res.get))
        instance.ParserTarget()
        instance.TreesTarget()
        instance.TreesImplTarget()
        instance.DefsTarget()
        instance.DefsImplTarget()
        instance.GrammarTarget()
      }
    }
  }
}
