package yascc.util

import yascc.tree.Trees._

class Substitution(val sub: Map[TVar, ScalaType]) extends AnyVal {
  def +(after: Substitution)(implicit lookupType: Identifier => ScalaType): Substitution = {
    val newSub = ((sub mapValues (_ substitute after)) /: after.sub) {
      (soFar, e) => e match {
        case (k, v) =>
          val newV = soFar.get(k) match {
            case Some(soFarV) => // conflict (go with the more specific type)??? 
              if ((v :< soFarV).get) v else soFarV
            case None => // no conflict
              v
          }
          soFar + (k -> newV)
      }
    }

    Substitution(newSub)
  }

  def apply(tv: TVar): ScalaType = 
    sub.getOrElse(tv, tv)
}

object Substitution {
  def apply(subs: (TVar, ScalaType)*) =
    new Substitution(subs.toMap)

  def apply(subMap: Map[TVar, ScalaType]) = 
    new Substitution(subMap)

  val empty = Substitution(Map.empty[TVar, ScalaType])
}

trait Substitutable[T] {
  def substitute(sub: Substitution): T
}
