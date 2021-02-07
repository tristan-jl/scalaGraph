package Graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphTest extends AnyFlatSpec with Matchers {
  private val simpleGraph = Graph(1 -> 2, 1 -> 3, 2 -> 3)

  implicit def tToVertex[T](input: T): Vertex[T] = Vertex(input)

  "A Graph" should "return the adjacency set of a given key" in {
    simpleGraph.get(1) should contain(Set(2, 3): Set[Vertex[Int]])
  }

  it should "return None when given a vertex not in the graph" in {
    simpleGraph.get(4) should be(None)
  }

  it should "return the vertices/keys of the graph as a Set" in {
    simpleGraph.keys should be(Set(1, 2, 3): Set[Vertex[Int]])
  }

  it should "return the adjacency sets for each vertex" in {
    simpleGraph.values should contain theSameElementsAs (List(Set(2, 3), Set(3), Set()): List[Set[Vertex[Int]]])
  }

  it should "return a new graph without the given vertex when a vertex is removed" in {
    simpleGraph.removed(3) should be(Graph(1 -> 2))
  }

  it should "return a new graph with a new value for the given vertex when updated" in {
    simpleGraph.updated(Vertex(3), Set(Vertex(4))) should be(Graph(1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4))
  }

  it should "return an iterator" in {
    simpleGraph.iterator.to(LazyList) should contain theSameElementsAs LazyList((Vertex(1), Set(Vertex(2), Vertex(3))), (Vertex(2), Set(Vertex(3))), (Vertex(3), Set()))
  }

  it should "be empty as required" in {
    simpleGraph.empty should be(Graph(Map[Vertex[Int], Set[Vertex[Int]]]()))
  }
}