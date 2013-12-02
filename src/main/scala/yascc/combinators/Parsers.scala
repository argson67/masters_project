package yascc.combinators

import java.util.regex.Pattern

import scala.language.implicitConversions

import scala.util.parsing.input._
import scala.util.matching.Regex

import scala.annotation.tailrec
import scala.collection.immutable.PagedSeq
import scala.collection.mutable.ListBuffer

import AnsiColor._

trait Parsers {
  type Elem = Char
  type Input = Reader[Elem]

  val MAX_BACKTRACE_DEPTH = 2

  trait Erroneous {
    val next: Input
    val expected: Set[String]
    val backtrace: List[(Position, String)]

    def err2str = {
      val errPos = next.pos
      val exp = if (expected.isEmpty) {
        "end of source"
      } else if (expected.size == 1) {
        expected.head
      } else {
        "one of: " + (expected mkString ", ")
      }

      (backtrace map { case (p, n) => s"While reading '$n' @ line ${p.line}, col ${p.column}:" } mkString "\n") +
      s"\n\tExpected %s; but found %s @ line ${errPos.line}, col ${errPos.column}".format(
      	exp, 
      	if (next.atEnd) "end of source" else s"'${next.first}'") + "\n" +
      errPos.longString
    }
  }

  case class ErrorDesc(next: Input, expected: Set[String], backtrace: List[(Position, String)] = Nil) extends Erroneous {
    def addBacktrace(p: Position, name: String) = 
      if (backtrace.length < MAX_BACKTRACE_DEPTH) {
        ErrorDesc(next, expected, (p, name) :: backtrace)
      } else {
        this
      }
  }

  sealed abstract class ParseResult[+T] {
    def get: T

    def next: Input

    def map[U](f: T => U): ParseResult[U]

    //def flatMapWithNext[U](f: T => Input => ParseResult[U]): ParseResult[U]

    val successful: Boolean

    def isEmpty = !successful

    def addBacktrace(pos: Position, name: String): ParseResult[T]

    def expected: Set[String]

    val errors: Set[ErrorDesc]

    def printErrors: String

    def tracePrint(spaces: String): String
  }

  case class Success[+T](value: T, next: Input, expected: Set[String] = Set.empty, errors: Set[ErrorDesc] = Set.empty) 
  extends ParseResult[T] {
    val get = value

    val successful = true

    def hasErrors: Boolean =
      !errors.isEmpty

    def addError(pos: Input, exp: Set[String]): ParseResult[T] =
      Success(value, next, expected, errors + ErrorDesc(pos, exp, Nil))

    def addBacktrace(pos: Position, name: String): ParseResult[T] = 
      Success(value, next, expected, errors map (_.addBacktrace(pos, name)))

    def map[U](f: T => U) = 
      Success(f(value), next, expected, errors)

    def printErrors: String = 
      s"[${RED}error${RESET}] " + (errors map (_.err2str) mkString "\n\n")

    def tracePrint(spaces: String): String = {
      val data = List("-> Success!", s"Value: $value", s"Next: ${next.pos}('${next.first}')", s"Expected: $expected", s"Errors: $errors")
      spaces + (data mkString s"\n$spaces")
    }
  }

  sealed abstract class NoSuccess extends ParseResult[Nothing] with Erroneous {
    def get = scala.sys.error("Cannot get value from NoSuccess parse")

    val backtrace: List[(Position, String)]

    val successful = false

    def map[U](f: Nothing => U) = this

    private def maybeNL(errs: Seq[_]): String = 
      if (errs.isEmpty) "" else "\n"

    def printErrors: String = 
      s"[${RED}error${RESET}] " + err2str + (if (errors.isEmpty) "" else "\n\n") +
      (errors map (_.err2str) mkString "\n\n")

    protected val myType: String

    def tracePrint(spaces: String): String = {
      val data = List(myType, s"Next: ${next.pos}('${next.first}')", s"Expected: $expected", s"Backtrace: $backtrace", s"Errors: $errors")
      spaces + (data mkString s"\n$spaces")
    }
  }

  object NoSuccess {
    def unapply(x: Any): Option[(Input, Set[String], List[(Position, String)], Set[ErrorDesc])] = x match {
      case Failure(next, expected, backtrace, errors) => Some((next, expected, backtrace, errors))
      case Error(next, expected, backtrace, errors) => Some((next, expected, backtrace, errors))
      case _ => None
    }
  }

  case class Failure(next: Input, expected: Set[String] = Set.empty, backtrace: List[(Position, String)] = Nil, errors: Set[ErrorDesc] = Set.empty)
  extends NoSuccess {
    def addBacktrace(pos: Position, name: String) =
      if (backtrace.length < MAX_BACKTRACE_DEPTH) {
        Failure(next, expected, (pos, name) :: backtrace, errors)
      } else {
        this
      }

    protected val myType = "-> Failure!"
  }

  case class Error(next: Input, expected: Set[String] = Set.empty, backtrace: List[(Position, String)] = Nil, errors: Set[ErrorDesc] = Set.empty) 
  extends NoSuccess {
    def addBacktrace(pos: Position, name: String) =
      if (backtrace.length < MAX_BACKTRACE_DEPTH) {
        Error(next, expected, (pos, name) :: backtrace, errors)
      } else {
        this
      }

    protected val myType = "-> Error!"
  }

  case class ~[+a, +b](_1: a, _2: b) {
    override def toString = "("+ _1 +"~"+ _2 +")"
  }

  def Parser[T](f: Input => ParseResult[T]): Parser[T] = new Parser[T] { 
    def apply(in: Input) = f(in) 
  }

  protected val whiteSpace = """\s+""".r

  def skipWhitespace = whiteSpace.toString.length > 0

  protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhitespace)
      (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => offset + matched.end
        case None => offset
      }
    else
      offset

  def rule[T](name: String, labeled: Boolean = false)(p: => Parser[T]) = Parser {
    in =>
      lazy val q = if (labeled) label(name)(p) else p
      q(in).addBacktrace(in.pos, name)
  }

  def lrule[T](name: String)(p: => Parser[T]) = rule(name, true)(p)

  def label[T](l: String)(p: => Parser[T]) = Parser {
    in =>
      p(in) match {
        case Failure(next, expected, backtrace, errors) if next.pos == in.pos =>
          Failure(next, Set(l), backtrace, errors)
        case Error(next, expected, backtrace, errors) if next.pos == in.pos =>
          Error(next, Set(l), backtrace, errors)
        case other => other
      }
  }

  def positioned[T <: Positional](p: => Parser[T]): Parser[T] = { 
    Parser {
      in =>
        val offset = in.offset
        val start = handleWhiteSpace(in.source, offset)
        p(in.drop (start - offset)).map(t => if (t.pos == NoPosition) t setPos in.pos else t)
    }
  }

  def commit[T](p: => Parser[T]): Parser[T] = Parser { in =>
    p(in) match {
      case NoSuccess(next, expected, bt, errs) =>
        Error(next, expected, bt, errs)
      case s => s
    }
  }

  def opt[T](p: => Parser[T]): Parser[Option[T]] = Parser { in =>
  	p(in) match {
  		case Success(v, nxt, exp, err) =>
  			Success(Some(v), nxt, exp, err)
  		case NoSuccess(_, exp, bt, err) =>
  			Success(None, in, exp, err)
  	}
  }

  def success[T](v: T) = Parser {
  	in => Success(v, in)
  }

  def failure(exp: Set[String]) = Parser {
  	in => Failure(in, exp)
  }

  def error(exp: Set[String]) = Parser {
  	in => Error(in, exp)
  }

  def rep1[T](first: => Parser[T], p0: => Parser[T]): Parser[List[T]] = Parser { in =>
    lazy val p = p0 // lazy argument
    val elems = new ListBuffer[T]

    def continue(in: Input, exp: Set[String], errs: Set[ErrorDesc]): ParseResult[List[T]] = {
      val p0 = p    // avoid repeatedly re-evaluating by-name parser
      @tailrec def applyp(in0: Input, exp: Set[String], errs: Set[ErrorDesc]): ParseResult[List[T]] = p0(in0) match {
        case Success(x, rest, newExp, newErrs) => elems += x ; applyp(rest, exp ++ newExp, errs ++ newErrs)
        case e @ Error(_, _, _, _)  => e  // still have to propagate error
        case _                      => Success(elems.toList, in0, exp, errs)
      }

      applyp(in, exp, errs)
    }

    first(in) match {
      case Success(x, rest, exp, errs) => 
        elems += x
        continue(rest, exp, errs)
      case ns: NoSuccess => ns
    }
  }

/*
  def cons[T](p: => Parser[_ <: T], q: => Parser[List[T]]): Parser[List[T]] = {
    p ~ q ^^ {
      case head ~ tail => head :: tail
    }
  }
*/

  def rep1[T](p: => Parser[T]): Parser[List[T]] =
    rep1(p, p)

  def rep[T](p: => Parser[T]): Parser[List[T]] = 
    rep1(p) | success(Nil)

  def rep1sep[T](p: => Parser[T], sep: Parser[Any]): Parser[List[T]] =
    p ~ rep(sep ~> p) ^^ {
      case first ~ rest => first :: rest
    }

  def rep1sepc[T](p: => Parser[T], sep: Parser[Any], first: Option[Parser[T]] = None): Parser[List[T]] = {
    first.getOrElse(p) ~ rep(sep ~!> p) ^^ {
      case f ~ rest => f :: rest
    }
  }

  def repsepc[T](p: => Parser[T], sep: Parser[Any], first: Option[Parser[T]] = None): Parser[List[T]] = 
    rep1sepc(p, sep, first) | success(Nil)

  def repsep[T](p: => Parser[T], sep: Parser[Any]): Parser[List[T]] =
    rep1sep(p, sep) | success(Nil)

  def parse[T](p: Parser[T], in: Input): (Either[String, T], Input) =
    p(in) match {
      case s@Success(value, next, _, _) =>
        (if (s.hasErrors) Left(s.printErrors) else Right(value), next)
      case ns: NoSuccess =>
        (Left(ns.printErrors), ns.next)
    }

  def parse[T](p: Parser[T], in: java.lang.CharSequence): (Either[String, T], Input) =
    parse(p, new CharSequenceReader(in))

  def parse[T](p: Parser[T], in: java.io.Reader): (Either[String, T], Input) =
    parse(p, new PagedSeqReader(PagedSeq.fromReader(in)))

  def parseAll[T](p: Parser[T], in: Input): Either[String, T] =
    parse(p, in) match {
      case (Right(v), next) => 
        if (next.atEnd) {
          Right(v)
        } else {
          val pos = next.pos
          Left(s"[${RED}error${RESET}] End of source expected but '${next.first}' found @ line ${pos.line}, col ${pos.column}")
        }
      case (other, _) => 
        other
    }

  def parseAll[T](p: Parser[T], in: java.lang.CharSequence): Either[String, T] = 
    parseAll(p, new CharSequenceReader(in))

  def parseAll[T](p: Parser[T], in: java.io.Reader): Either[String, T] = 
    parseAll(p, new PagedSeqReader(PagedSeq.fromReader(in)))

  implicit def literal(s: String): Parser[String] = Parser {
    in =>
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length) {
        Success(source.subSequence(start, j).toString, in.drop(j - offset), Set("\"%s\"".format(s)))
      } else {
        Failure(in.drop(start - offset), Set("\"%s\"".format(s)), Nil)
      }
  }

  def skipWS(in: Input): Input = {
    val source = in.source
    val offset = in.offset
    val start = handleWhiteSpace(source, offset)
    in.drop(start - offset)
  }

  /** A parser that matches a regex string */
  implicit def regex(r: Regex): Parser[String] = Parser {
    in =>
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(source.subSequence(start, start + matched.end).toString,
                  in.drop(start + matched.end - offset), Set("r\"%s\"".format(r.toString)))
        case None =>
          Failure(in.drop(start - offset), Set("r\"%s\"".format(r.toString)), Nil)
      }
  } 

  def char(c: Char): Parser[Char] = Parser {
    in =>
      if (!in.atEnd && in.first == c) {
        Success(in.first, in.rest, Set(s"'$c'"))
      } else {
        Failure(in, Set(s"'$c'"))
      }
  }

  def recoverInsert[T](p: => Parser[T], ins: => T, exp: => Set[String]): Parser[T] = Parser { in =>
  	p(in) match {
    	case NoSuccess(errNxt, errExp, _, errs) =>
      	val err = ErrorDesc(errNxt, errExp)
      	Success(ins, in, exp, errs + err)
    	case s => s
    }
  }

  def recoverInsert(s: String): Parser[String] =
    recoverInsert(literal(s), s, Set(s))

  private def skipUntil(ps: Seq[Parser[Any]], in: Input): Either[Input, Input] = {
  	if (in.atEnd) {
  		Left(in)
  	} else {
  		if (ps.exists(p => p(in).successful)) {
  			Right(in)
  		} else {
  			skipUntil(ps, in.rest)
  		}
  	}
  }

  import Defaults._

  def recoverSkip[T](p: => Parser[T], until: Seq[Parser[Any]])(implicit df: Default[T]): Parser[T] = Parser { in =>
  	p(in) match {
  		case ns@NoSuccess(errNxt, errExp, _, errs) =>
  			skipUntil(until, in) match {
  				case Right(nxt) => 
  					val err = ErrorDesc(errNxt, errExp)
  					Success(df.get, nxt, errExp, errs + err)
  				case Left(_) => 
  					ns
  			}
  		case s => s
  	}
  }

  private var traceLevel = 0

  def trace[T](p: => Parser[T])(name: String): Parser[T] = Parser {
    in =>
      val spaces = "  " * traceLevel + "** "
      println(s"${spaces}Attempting `$name' @ ${in.pos}:")
      traceLevel += 1
      val res = p(in)
      traceLevel -= 1
      println(s"${res.tracePrint(spaces)}")
      res
  }

  abstract class Parser[+T] extends (Input => ParseResult[T]) {
    def apply(in: Input): ParseResult[T]

    private def seq[U, V](q: => Parser[U], comb: (T, U) => V): Parser[V] = {
      lazy val p = q
      Parser { _in =>
        val in = skipWS(_in) 
        this(in) match {
          case Success(v1, next, exp1, err1) =>
            p(next) match {
              case Success(v2, next2, exp2, err2) =>
                val exp = if (in.pos == next.pos) exp1 ++ exp2 else exp2
                val err = err1 ++ err2
                Success(comb(v1, v2), next2, exp, err)
              case ns: NoSuccess => ns
            }
          case ns: NoSuccess => ns
        }
      }
    }

    def ~[U](q: => Parser[U]): Parser[~[T, U]] =
      seq(q, (x: T, y: U) => new ~(x, y))

    def ~>[U](q: => Parser[U]): Parser[U] =
      seq(q, (x: T, y: U) => y)

    def ~!>[U](q: => Parser[U]): Parser[U] = 
      this ~> commit(q)

    def <~[U](q: => Parser[U]): Parser[T] =
      seq(q, (x: T, y: U) => x)

    def ~![U](q: => Parser[U]): Parser[~[T, U]] =
      this ~ commit(q)

    def ^^[U](f: T => U): Parser[U] = Parser { in =>
      this(in).map(f)
    }

    def + = rep1(this)

    def ? = opt(this)

    def * = rep(this)

    def ^^^[U](r: => U): Parser[U] = {
      lazy val rr = r
      Parser { in => this(in).map(_ => rr) }
    }

    def |[U >: T](q: => Parser[U]): Parser[U] = {
      lazy val p = q
      Parser { _in =>
        val in = skipWS(_in) 
        this(in) match {
          case s@Success(v, next, exp1, err1) if next.pos == in.pos => // success that consumed no input
            p(in) match {
              case Success(_, next2, exp2, err2) if next2.pos == next.pos =>
                Success(v, next, exp1 ++ exp2, err1 ++ err2)
              case Failure(next2, exp2, _, err2) if next2.pos == next.pos =>
                Success(v, next, exp1 ++ exp2, err1 ++ err2)
              case _ =>
                s
            }
          case Failure(next, exp1, bt1, err1) =>
            p(in) match {
              case Success(v, next2, exp2, err2) if (next.pos == in.pos && next2.pos == in.pos) =>
                Success(v, next2, exp1 ++ exp2, err1 ++ err2)
              case Failure(next2, exp2, bt2, err2) if (next.pos == in.pos && next2.pos == in.pos) =>
                  Failure(next2, exp1 ++ exp2, Nil, err1 ++ err2)
              case other => other // error, or success or failure that consumed input
            }
          case other => // error, or success that consumed input
            other
        }
      }
    }
  }


}
