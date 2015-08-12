trait Graph {
  type Edge;
  type Node <: NodeIntf;

  trait NodeIntf {
    def self: Node;
    def connectWith(node: Node): Edge;
  }

  implicit def convert(node: NodeIntf): Node = {
    node.self;
  }

  def nodes: List[Node];
  def edges: List[Edge];
  def addNode: Node;
}

abstract class DirectedGraph extends Graph {
  type Edge <: EdgeImpl;

  class EdgeImpl(origin: Node, dest: Node) {
    def from = origin;
    def to = dest;
  }

  abstract class NodeImpl extends NodeIntf {
    def connectWith(node: Node): Edge = {
      val edge = newEdge(this, node);
      edges = edge :: edges;
      edge;
    }
  }

  protected def newNode: Node;
  protected def newEdge(from: Node, to: Node): Edge;
  var nodes: List[Node] = Nil;
  var edges: List[Edge] = Nil;

  def addNode: Node = {
    val node = newNode;
    nodes = node :: nodes;
    node;
  }
}

class ConcreteDirectedGraph extends DirectedGraph {
  type Edge = EdgeImpl;
  type Node = NodeImpl0;

  class NodeImpl0 extends NodeImpl {
    def self = {
      this;
    }
  }

  protected def newNode: Node = {
    new NodeImpl0;
  }

  protected def newEdge(f: Node, t: Node): Edge = {
    new EdgeImpl(f, t);
  }
}

object ExplicitThis {
  def main(args: Array[String]): Unit = {
    val g: Graph = new ConcreteDirectedGraph;
    val n1 = g.addNode;
    val n2 = g.addNode;
    val n3 = g.addNode;
    n1.connectWith(n2);
    n2.connectWith(n3);
    n1.connectWith(n3);
  }
}

