package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /* Ex.
  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- Random.nextInt()
      v <- Random.nextInt()
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

    lazy val genHeap1: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)),(9, genHeap))
  } yield insert(n, h)
   */

  /**
   * This is I copied from other guy on Github.
   *  And it's working
   */
//  lazy val genHeap: Gen[H] = for {
//    n <- arbitrary[A]
//    h <- frequency((1, Gen.const(empty)),(9, genHeap))
//  } yield insert(n, h)


  /**
   * FIXME: If possible
   * This is what I wrote, seems plausible to me
   * but doesn't work
   */
    lazy val genHeap: Gen[H] = oneOf(const(empty),
    for {
      n <- arbitrary[A]
      h <- genHeap
    } yield insert(n, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /*
  If you insert any two elements into an empty heap,
  finding the minimum of the resulting heap should get the smallest of the two elements back.
  */
  property("the minimum of the heap should be the smallest of the two elements inserted") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)
    val min = if(a <= b) a else b
    findMin(h2) == min
  }
  /*
If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
*/
  property("inserting an element and deleting the minimum on an empty heap should result into empty heap") =
    forAll { a: Int =>
      val h = insert(a, empty)
      val h2 = deleteMin(h)
      isEmpty(h2) == true
    }

  /*
   Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
   (Hint: recursion and helper functions are your friends.)
   */
  property("should get a sorted sequence of elements when continually finding and deleting minima") =
    forAll { h: H =>
      def isSorted(h: H): Boolean = {
        if (isEmpty(h)) true
        else {
          val min = findMin(h)
          val hh = deleteMin(h)
          isEmpty(hh) || (min <= findMin(hh) && isSorted(hh))
        }
      }

      isSorted(h)
    }

  /*
  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
   */
  property("finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (h1: H, h2: H) =>
      val minOfMeld = findMin( meld(h1, h2))
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val minOfTwoHeap = if(min1 < min2) min1 else min2

      minOfMeld == minOfTwoHeap
    }

  property("two heaps should be of equal if removing min elements till both heaps empty") =
    forAll { (h1: H, h2: H) =>
      def isHeapEqual(g: H, h: H): Boolean =
        if (isEmpty(g) && isEmpty(h)) true
        else {
          val mg = findMin(g)
          val mh = findMin(h)
          mg == mh && isHeapEqual(deleteMin(g), deleteMin(h))
        }
      isHeapEqual(meld(h1, h2),
        meld(deleteMin(h1), insert(findMin(h1), h2)))
    }

  property("The minimal value of 2 heaps should be the minimal after dispacing it from heap 1 to 2 and melding both") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = if(m1 < m2) m1 else m2
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }
}
