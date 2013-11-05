package yascc

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

      println(initTree map instance.prettyPrintTree)
      println(instance.listAll)
    }
  }
}
