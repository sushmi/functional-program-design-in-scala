import example.week1._

import scala.util.Random

object generators {

  val integers = new Generator[Int] {
    override def generate: Int = Random.nextInt()
  }

  val booleans = integers.map(_ >= 0)

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  def inners: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)

}

generators.trees.generate
generators.trees.generate
generators.trees.generate