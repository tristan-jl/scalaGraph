package Graph

import cats._
import cats.implicits._

import scala.annotation.tailrec

case class Graph[T](graph: Map[Vertex[T], Set[Vertex[T]]])
    extends Map[Vertex[T], Set[Vertex[T]]]
    with Monoid[Graph[T]] {
  type VertexT = Vertex[T]

  override def get(key: VertexT): Option[Set[VertexT]] = graph.get(key)

  override val keys: Set[VertexT] = graph.keys.toSet

  override val values: Iterable[Set[VertexT]] = graph.values

  override def removed(key: VertexT): Graph[T] =
    Graph(graph.flatMap {
      case (k, v) =>
        if (k == key) None
        else Some(k -> (v - key))
    })

  override def updated[V1 >: Set[VertexT]](key: VertexT, valueSet: V1): Graph[T] =
    Graph(
      graph
        + (key -> valueSet.asInstanceOf[Set[VertexT]])
        ++ (valueSet
          .asInstanceOf[Set[VertexT]] -- keys).map(i => i -> graph.getOrElse(i, Set())).toMap)

  override def iterator: Iterator[(VertexT, Set[VertexT])] = graph.iterator

  override def empty: Graph[T] = Graph(Map[VertexT, Set[VertexT]]())

  override def combine(x: Graph[T], y: Graph[T]): Graph[T] =
    Graph(
      (x.keys ++ y.keys)
        .foldLeft(Map[VertexT, Set[VertexT]]()) { (acc, vertex) =>
          acc + (vertex -> (x.graph.getOrElse(vertex, Set()) ++ y.graph.getOrElse(vertex, Set())))
        })

  val isAdjacent: (VertexT, VertexT) => Boolean = (x, y) =>
    graph.get(x).exists(_.contains(y)) || graph.get(y).exists(_.contains(x))

  val neighbours: VertexT => Option[Set[VertexT]] = (x: VertexT) => graph.get(x)

  val sources: Set[VertexT] = keys -- values.flatten.toSet

  def sourceVertices(vertex: VertexT): Set[VertexT] =
    graph.filter { case (_, v) => v contains vertex }.keys.toSet

  def depthFirstSearch(startVertex: VertexT): Set[VertexT] = {
    def helper(vertex: VertexT, visited: Set[VertexT] = Set()): Set[VertexT] = {
      if (visited.contains(vertex))
        visited
      else
        graph(vertex).diff(visited).foldLeft(Set(vertex) ++ visited)((a, b) => helper(b, a))
    }

    helper(startVertex)
  }

  val topologicalSort: Option[List[VertexT]] = {
    @tailrec
    def helper(
        graph: Map[VertexT, Set[VertexT]],
        sorted: List[VertexT] = List()): Option[List[VertexT]] = {
      val (sinks, subGraph) = graph.partition(_._2.isEmpty)

      (sinks.isEmpty, subGraph.isEmpty) match {
        case (true, true) => sorted.pure[Option]
        case (true, false) => None
        case _ =>
          helper(
            subGraph.map { case (k, v) => k -> (v -- sinks.keys) },
            sinks.keys.toList ++ sorted)
      }
    }

    helper(graph)
  }

  val is_cyclic: Boolean = topologicalSort.isEmpty
}

object Graph {
  def apply[T](mapping: List[(T, T)]): Graph[T] = {
    type VertexT = Vertex[T]
    val mappingSet: Set[(VertexT, VertexT)] = mapping.map {
      case (k, v) => (Vertex(k), Vertex(v))
    }.toSet

    val graph: Map[VertexT, Set[VertexT]] = mappingSet
      .flatMap { case (i, j) => Set(i, j) }
      .map(i => (i, mappingSet.filter(_._1 == i).map(_._2)))
      .toMap

    Graph(graph)
  }

  def apply[T](tuples: (T, T)*): Graph[T] = Graph(tuples.toList)

  def unapply[T](graph: Graph[T]): Option[Map[Vertex[T], Set[Vertex[T]]]] =
    graph.graph.pure[Option]
}
