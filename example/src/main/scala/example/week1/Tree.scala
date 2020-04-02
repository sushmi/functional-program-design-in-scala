package example.week1

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree {
  override def toString: String = {
    def iter(tree: Tree): String = tree match {
      case leaf: Leaf => leaf.toString
      case inner: Inner => "Inner(" + iter(inner.left) + ", " + iter(inner.right) + ")"
    }

    "Inner(" + iter(left) + ", " + iter(right) + ")"
  }
}

case class Leaf(x: Int) extends Tree {
  override def toString: String = s"Leaf($x)"
}

