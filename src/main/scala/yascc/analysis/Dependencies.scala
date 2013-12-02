package yascc.analysis

import scala.collection.mutable.{ Set => MSet, Map => MMap, Stack => MStack, ListBuffer }

import yascc.tree.Trees._

trait Dependencies {
  self: Phases =>

  private class Graph[NodeT]() {
    private type EdgeT = (NodeT, NodeT)

    private val nodes = MSet.empty[NodeT]
    private val edges = MSet.empty[EdgeT]

    def addNode(n: NodeT): Unit =
      nodes.add(n)

    def addEdge(e: EdgeT): Unit =
      edges.add(e)

    private def successors(n: NodeT): MSet[NodeT] =
      edges filter (_._1 == n) map (_._2)

    private def predecessors(n: NodeT): MSet[NodeT] =
      edges filter (_._2 == n) map (_._1)

    /*
      http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

      algorithm tarjan is
        input: graph G = (V, E)
        output: set of strongly connected components (sets of vertices)

        index := 0
        S := empty
        for each v in V do
          if (v.index is undefined) then
            strongconnect(v)
          end if
        repeat

        function strongconnect(v)
          // Set the depth index for v to the smallest unused index
          v.index := index
          v.lowlink := index
          index := index + 1
          S.push(v)

          // Consider successors of v
          for each (v, w) in E do
            if (w.index is undefined) then
              // Successor w has not yet been visited; recurse on it
              strongconnect(w)
              v.lowlink := min(v.lowlink, w.lowlink)
            else if (w is in S) then
              // Successor w is in stack S and hence in the current SCC
              v.lowlink := min(v.lowlink, w.index)
            end if
          repeat

          // If v is a root node, pop the stack and generate an SCC
          if (v.lowlink = v.index) then
            start a new strongly connected component
            repeat
              w := S.pop()
              add w to current strongly connected component
            until (w = v)
            output the current strongly connected component
          end if
        end function
    */

    private def tarjan: List[Set[NodeT]] = {
      val index = MMap.empty[NodeT, Int]
      val lowlink = MMap.empty[NodeT, Int]

      // reset
      nodes.foreach {
        n =>
          index(n) = -1
          lowlink(n) = -1
      }

      //println(s"TARJAN:\n\tnodes: $nodes\n\tedges: $edges\n")
      
      var idx = 0
      val s = MStack.empty[NodeT]
      val sccs: ListBuffer[Set[NodeT]] = ListBuffer.empty

        def strongConnect(n: NodeT): Unit = {
          // Set the depth index for n to the smallest unused index
          index(n) = idx
          lowlink(n) = idx
          idx += 1
          s.push(n)

          // Consider successors of v
          successors(n) foreach {
            w =>
              if (index(w) == -1) {
                // Successor w has not yet been visited; recurse on it
                strongConnect(w)
                lowlink(n) = List(lowlink(n), lowlink(w)).min
              } else if (s contains w) {
                // Successor w is in stack S and hence in the current SCC
                lowlink(n) = List(lowlink(n), index(w)).min
              }
          }

          // If v is a root node, pop the stack and generate an SCC
          if (lowlink(n) == index(n)) {
            val scc = MSet.empty[NodeT]
            var w = s.pop()
            scc.add(w)
            while (w != n) {
              w = s.pop()
              scc.add(w)
            }

            sccs.append(scc.toSet)
          }
        }

      nodes foreach {
        n =>
          if (index(n) == -1) {
            strongConnect(n)
          }
      }

      sccs.toList
    }

    private def sccsToDAG(sccs: List[Set[NodeT]]): Graph[Set[NodeT]] = {
      val nodeMap: Map[NodeT, Set[NodeT]] = sccs flatMap {
        scc => scc map (n => (n -> scc))
      } toMap

      val res = new Graph[Set[NodeT]]()
      sccs foreach (res addNode _)
      edges foreach {
        case (from, to) if nodeMap(from) != nodeMap(to) => res.addEdge((nodeMap(from), nodeMap(to)))
        case _ =>
      }

      res
    }

    private def topSort = {
      /*
        http://en.wikipedia.org/wiki/Topological_sorting#Algorithms

        L ← Empty list that will contain the sorted nodes
        while there are unmarked nodes do
            select an unmarked node n
            visit(n) 
        function visit(node n)
            if n has a temporary mark then stop (not a DAG)
            if n is not marked (i.e. has not been visited yet) then
                mark n temporarily
                for each node m with an edge from n to m do
                    visit(m)
                mark n permanently
                add n to head of L
      */

      val unmarked = MSet.empty ++ nodes
      val tempMarked = MSet.empty[NodeT]
      val res = ListBuffer.empty[NodeT]

        def visit(n: NodeT): Unit = {
          /*
          if (tempMarked contains n) {
            compiler.error(DepcheckerError(s"Dependency graph is not a DAG! ($n)"))
          }*/ 

          assert(!tempMarked.contains(n), s"Dependency graph is not a DAG ($n)")

          if (unmarked contains n) {
            unmarked.remove(n)
            tempMarked.add(n)
            successors(n) foreach visit
            tempMarked.remove(n)
            res.append(n)
          }
        }

      while (!unmarked.isEmpty) {
        visit(unmarked.head)
      }

      res.toList
    }

    def getOrder: List[Set[NodeT]] =
      sccsToDAG(tarjan).topSort
  }

  private def collectDeps: PartialFunction[Tree, Set[String]] = {
    case NonTerminal(name) => Set(name)
  }

  def getTypingOrder(rules: Seq[Rule]): List[Set[String]] = {
    import yascc.util.Implicits.collect

    val g = new Graph[String]()

    rules foreach {
      r => 
        g addNode r.name
        val deps = collect(r)(collectDeps).toSet
        deps foreach (d => g.addEdge(r.name -> d))
    }

    g.getOrder
  }
}
