package yascc.util

import scala.util.parsing.input.{Position, NoPosition}

import scala.language.implicitConversions

import yascc.tree.Trees._

// TODO: Rename? Not really just for implicits anymore, moar like random utils

object Implicits {
  implicit class ToResult[T](obj: T) {
    def success: Result[T] = Success(obj)
    def failure(msg: String, pos: Position = NoPosition): Result[T] = Failure(obj, Seq(Error(msg, pos)))
    def fatalError(msg: String, pos: Position = NoPosition) = FatalError(Seq(Error(msg, pos)))
  }
  
  def error(msg: String, pos: Position = NoPosition): FatalError = FatalError(Seq(Error(msg, pos)))
  val OK = Result.unit
  def failure(msg: String, pos: Position = NoPosition): Failure[Unit] = Failure((), Seq(Error(msg, pos)))

  implicit def str2error(str: String): Error = Error(str)
  implicit def str2warning(str: String): Warning = Warning(str)

  //implicit def seqOfRes2ResOfSeq[A](seq: Seq[Result[A]]) = Result.sequence(seq)
  //implicit def optOfRes2ResOfOpt[A](opt: Option[Result[A]]) = Result.fromOption(opt)

  //implicit def optError2res(optError: Option[Error]): Result[Unit] = 
  //  optError.map(e => Result.unit.addError(e)).getOrElse(Result.unit)

  def query(t: Tree)(f: PartialFunction[Tree, Result[Unit]]): Result[Unit] = {
    def queryChild: PartialFunction[Any, Result[Unit]] = {
      case tree: Tree => query(tree)(f)
      case seqTree: Seq[_] => Result.chain(seqTree)(queryChild)
      case _ => OK
    }

    val res = if (f.isDefinedAt(t)) f(t) else OK
    val children = Result.chain(t.productIterator.toSeq)(queryChild)
    res :+ children
  }
}
