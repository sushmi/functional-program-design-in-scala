package example.week1

import scala.util.Random

object FunctionalRandomGenerator extends App {
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

  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
    for(_ <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + " tests")
  }
}
