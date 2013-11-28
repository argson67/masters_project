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

  // Top-down query

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

  // Top-down collection

  def collect[T](t: Tree)(f: PartialFunction[Tree, Traversable[T]]): Traversable[T] = {
    def collectChild: PartialFunction[Any, Traversable[T]] = {
      case tree: Tree => collect(tree)(f)
      case seqTree: Seq[_] => seqTree flatMap collectChild
      case _ => Traversable.empty[T]
    }

    val res = if (f.isDefinedAt(t)) f(t) else Traversable.empty
    val children = t.productIterator.toSeq flatMap collectChild
    res ++ children
  }

  implicit def tToRes[T](x: T): Result[T] = x.success

  // Bottom-up tree rewriting

  // ugh... what a clusterfuck

  import scala.reflect.ClassTag

  private def any2AnyRef(x: Any): AnyRef = x match {
    case ar: AnyRef => ar
    case av => av.asInstanceOf[AnyRef]
  }

  def rewrite(t: Tree)(f: PartialFunction[Tree, Result[Tree]]): Result[Tree] = {
    def rewriteChild: PartialFunction[Any, Result[Any]] = {
      case t: Tree => rewrite(t)(f)
      case seqTree: Seq[_] => Result.sequence(seqTree map rewriteChild)
      case other => other.success
    }

    val children = Result.sequence(t.productIterator.toSeq map rewriteChild)
    val constructor = t.getClass.getConstructors()(0)
    val rewrittenT = children map {
      args =>
        constructor.newInstance((args map any2AnyRef): _*).asInstanceOf[Tree]
    }

    rewrittenT flatMap {
      t => 
        if (f.isDefinedAt(t)) f(t) else t.success
    }
  }
}
