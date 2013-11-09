package yascc.analysis

import yascc.tree.Trees._
import yascc.util.Result

trait Sets {
  self: Phases =>

  import yascc.util.Implicits._
  
    object SetsPhase extends Phase {
      private val nullSet = Set(Epsilon: Terminal)

      private def seqFirst(seq: Seq[ProductionElem]): Set[Terminal] = {
        seq.headOption map {
          h =>
            val hFirst = first(h)
            if (hFirst contains Epsilon) hFirst ++ seqFirst(seq.tail) else hFirst
        } getOrElse nullSet
      }

      private def first: Function1[ProductionElem, Set[Terminal]] = {
        case Conjunction(elems) =>
          seqFirst(elems)
        case Disjunction(elems) =>
          elems map first reduce (_ ++ _)
        case Opt(elem) =>
          first(elem) + Epsilon
        case Discard(elem) =>
          first(elem)
        case Rep(elem, sep, strict) =>
          if (strict) first(elem) else first(elem) + Epsilon
        case Label(elem, label) =>
          first(elem)
        case NonTerminal(name) =>
          // this is the only place where an error could occur, but really shouldn't
          // since this necessarily follows an init step, which checks for this bullshit
          lookupRule(name).get.r.firstSet

        case other: Terminal => Set(other)
      }

      private def seqFollow(seq: Seq[ProductionElem], after: Set[Terminal]): Boolean = {
        
      } 

      private def follow(e: ProductionElem, after: Set[Terminal]): Boolean = e match {
        case Conjunction(elems) =>

        case Disjunction(elems) =>
        case Opt(elem) =>
        case Discard(elem) =>
        case Rep(elem, sep, strict) =>
        case Label(elem, label) =>

        case NonTerminal(name) =>
          val r = lookupRule(name).get.r
          val old = r.followSet
          r.followSet ++= after
          (r.followSet == old)

        case _ => 
          false
      }

      private def calculateFirst(rule: Rule): Boolean = {
        val old = rule.firstSet
        rule.firstSet ++= rule.productions map (p => first(p.body)) reduce (_ ++ _)
        println(s"Calculating first (${rule.name}); old = $old; new = ${rule.firstSet}")
        old != rule.firstSet
      }

      private def calculateFirstAll = {
        def id[T](x: T) = x
        while (rules map calculateFirst exists id) { }
      }

      def apply(in: Result[Tree]): Result[Tree] = {
        calculateFirstAll
        in
      }
    }
}
