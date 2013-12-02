package yascc.util

import yascc.tree.Trees._

abstract class SubDirection {
  val tpe: ScalaType

  def map(f: ScalaType => ScalaType): SubDirection
}

object SubDirection {
  def unapply(x: Any): Option[ScalaType] = x match {
    case SubType(t) => Some(t)
    case SuperType(t) => Some(t)
    case ExactType(t) => Some(t)
    case _ => None
  }
}

case class SubType(tpe: ScalaType) extends SubDirection {
  def map(f: ScalaType => ScalaType) = 
    SubType(f(tpe))
}
case class SuperType(tpe: ScalaType) extends SubDirection {
  def map(f: ScalaType => ScalaType) = 
    SuperType(f(tpe))
}
case class ExactType(tpe: ScalaType) extends SubDirection {
  def map(f: ScalaType => ScalaType) = 
    ExactType(f(tpe))
}

class Substitution(val sub: Map[TVar, SubDirection]) extends AnyVal {
  //private def substitute()

  def subType(t1: ScalaType, t2: ScalaType)(implicit lookupType: Identifier => ScalaType): Boolean = 
    (t1 :< t2).get

  def +(after: Substitution)(implicit lookupType: Identifier => ScalaType): Substitution = {
    val firstSub = (sub mapValues (v => v.map(x => x.substitute(after))))
    val newSub = (firstSub /: after.sub) {
      (soFar, e) => e match {
        case (k, v) =>
          val newV = soFar.get(k) match {
            case Some(soFarV) => // conflict
              (v, soFarV) match {
                case (SubType(t1), SubType(t2)) => 
                  if (subType(t1, t2)) SubType(t1) else SubType(t2)
                case (SuperType(t1), SuperType(t2)) => if (subType(t2, t1)) SuperType(t1) else SuperType(t2)
                case (SubType(t1), SuperType(t2)) => if (subType(t1, t2)) SuperType(t2) else SubType(t1)
                case (SuperType(t1), SubType(t2)) => if (subType(t2, t1)) SuperType(t1) else SubType(t2)
                case (ExactType(t1), x@SubDirection(t2)) if t1 == t2 => x
                case (x@SubDirection(t1), ExactType(t2)) if t1 == t2 => x
                case _ => v
              }
            case None => // no conflict
              v
          }
          soFar + (k -> newV)
      }
    }

    Substitution(newSub)
  }

  def apply(tv: TVar): ScalaType = 
    sub.get(tv).map(_.tpe).getOrElse(tv)
}

object Substitution {
  def apply(subs: (TVar, SubDirection)*) =
    new Substitution(subs.toMap)

  def apply(subMap: Map[TVar, SubDirection]) = 
    new Substitution(subMap)

  val empty = Substitution(Map.empty[TVar, SubDirection])
}

trait Substitutable[T] {
  def substitute(sub: Substitution): T
}
