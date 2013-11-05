package yascc

object Main {
  def main(args: Array[String]): Unit = {
    //println("Hello world.")
    if (args.size != 1) {
      println("Exactly one argument required (filename)")
    } else {
      val fname = args(0)
      val instance = new Yascc()
      val res = instance.readFile(fname) map instance.prettyPrintTree
      println(res)
    }
  }
}
