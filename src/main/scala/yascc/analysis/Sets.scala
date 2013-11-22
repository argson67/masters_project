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
        case Commit(elem) =>
          first(elem)
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
        seq.foldRight((after, false)) {
          (e, soFar) =>
            soFar match {
              case (after, res) =>
                val eFirst = first(e)
                val newAfter = if (eFirst contains Epsilon) after ++ eFirst - Epsilon else eFirst
                val newRes = follow(e, after) || res
                (newAfter, newRes)
            }
        }._2
      } 

      private def follow(e: ProductionElem, after: Set[Terminal]): Boolean = {
        //println(s"calling follow(${yascc.tree.TreePrinter(e)}, $after)")
        e match {
          case Conjunction(elems) =>
            seqFollow(elems, after)
          case Disjunction(elems) =>
            elems map (e => follow(e, after)) reduce (_ || _)
          case Opt(elem) =>
            follow(elem, after)
          case Commit(elem) =>
            follow(elem, after)
          case Discard(elem) =>
            follow(elem, after)
          case Rep(elem, sep, strict) =>
            sep match {
              case Some(s) =>
                val sFollow = follow(s, after ++ first(elem))
                val eFollow = follow(elem, after ++ first(s))
                sFollow || eFollow
              case None =>
                follow(elem, after ++ first(elem))
            }
          case Label(elem, label) =>
            follow(elem, after)

          case NonTerminal(name) =>
            val r = lookupRule(name).get.r
            val old = r.followSet
            r.followSet ++= after
            //println(s"Updating follow($name); old = $old, new = ${r.followSet}")
            (r.followSet != old)

          case other: Terminal => 
            false
        }
      }

      private def calculateFirst(rule: Rule): Boolean = {
        val old = rule.firstSet
        rule.firstSet ++= rule.productions map (p => first(p.body)) reduce (_ ++ _)
        //println(s"Calculating first (${rule.name}); old = $old; new = ${rule.firstSet}")
        old != rule.firstSet
      }

      private def calculateFollow(rule: Rule): Boolean = {
        val ruleFollow = rule.followSet
        rule.productions map (p => follow(p.body, ruleFollow)) reduce (_ || _)
      }

      private def calculateAll(f: Rule => Boolean): Unit = 
        while (rules map f exists identity) { }

      private def calculateFirstAll = calculateAll(calculateFirst)
      private def calculateFollowAll = calculateAll(calculateFollow)

      def apply(in: Result[Tree]): Result[Tree] = {
        calculateFirstAll
        calculateFollowAll
        in
      }
    }
}
