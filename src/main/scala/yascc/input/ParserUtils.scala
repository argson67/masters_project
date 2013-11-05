package yascc.input

import yascc.tree.Trees._

trait ParserUtils {
  self: FileParser =>

    def enclose[A, B, C](p: => Parser[A], start: Parser[B], end: Parser[C]) =  start ~> p <~ end

    def braces[T](p: => Parser[T]) = enclose(p, "{", "}")

    def brackets[T](p: => Parser[T]) = enclose(p, "[", "]")

    def parens[T](p: => Parser[T]) = enclose(p, "(", ")")

    private[ParserUtils] lazy val decInt = "([0-9]+)((l|L)*)".r
    private[ParserUtils] lazy val hexInt = "0[xX]([a-fA-F0-9]+)((l|L)*)".r
    private[ParserUtils] lazy val binInt = "0[bB]([01]+)((l|L)*)".r
    private[ParserUtils] lazy val octInt = "0([0-9]+)((l|L)*)".r 
    
    private[ParserUtils] lazy val real1 = "([0-9]+[Ee][+-]?[0-9]+)((f|F|l|L)?)".r
    private[ParserUtils] lazy val real2 = "([0-9]*\\.[0-9]+([Ee][+-]?[0-9]+)?)((f|F|l|L)?)".r
    private[ParserUtils] lazy val real3 = "([0-9]+\\.[0-9]*([Ee][+-]?[0-9]+)?)((f|F|l|L)?)".r

    private[ParserUtils] def intFromString(numStr: String, radix: Int, opts: String): NumberLiteral[_] = {
      val bi = BigInt(numStr, radix)
      if (opts.contains("l") || !bi.isValidInt) {
        NumberLiteral(bi.toLong)
      } else {
        NumberLiteral(bi.toInt)
      }
    }

    private[ParserUtils] def realFromString(numStr: String, opts: String): NumberLiteral[_] = {
      val bd = BigDecimal(numStr)
      if (opts.contains("l")) {
        NumberLiteral(bd)
      } else if (opts.contains("f")) {
        NumberLiteral(bd.toFloat)
      } else {
        NumberLiteral(bd.toDouble)
      }
    }

    lazy val string: PackratParser[String] =
      "\"(\\.|[^\\\"])*\"".r ^^ { x => x.toString.slice(1, x.size - 1) }

    lazy val regex: PackratParser[String] = 
      "r\"(\\.|[^\\\"])*\"".r ^^ { x => x.toString.slice(2, x.size - 1) }

    lazy val identifier = "[A-Za-z_][A-Za-z0-9_]+".r

    lazy val number: PackratParser[NumberLiteral[_]] = (
      real
      | integer)

    private[ParserUtils] lazy val integer: PackratParser[NumberLiteral[_]] =
      positioned(
          hexInt ^^ { 
            v =>
              val hexInt(numStr, optStr, _) = v
              intFromString(numStr, 16, optStr.toLowerCase())   
          }
        | octInt ^^ {
            v =>
              val octInt(numStr, optStr, _) = v
              intFromString(numStr, 8, optStr.toLowerCase())   
          }
        | binInt ^^ {
            v =>
              val binInt(numStr, optStr, _) = v
              intFromString(numStr, 2, optStr.toLowerCase())            
          }
        | decInt ^^ { 
            v =>
              val decInt(numStr, optStr, _) = v
              intFromString(numStr, 10, optStr.toLowerCase())   
          }
        | failure("Expected integer literal"))

    private[ParserUtils] lazy val real: PackratParser[NumberLiteral[_]] =
      positioned(
          real1 ^^ { 
            v =>
              val real1(numStr, _, optStr, _) = v
              realFromString(numStr, optStr.toLowerCase())  
          }
        | real2 ^^ { 
            v =>
              val real2(numStr, _, optStr, _) = v
              realFromString(numStr, optStr.toLowerCase())  
          }
        | real3 ^^ { 
            v =>
              val real3(numStr, _, optStr, _) = v
              realFromString(numStr, optStr.toLowerCase())  
          }
        | failure("Expected real literal"))
}
