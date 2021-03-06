package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math._
import collection.immutable._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("hint1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == min(a, b)
  }

  property("hint2") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  val sizedList = Gen.sized { size =>
    Gen.listOfN(size, Gen.choose(0, 100))
  }

  property("hint3") = forAll(sizedList) { list =>
    val heap = insertAll(list, empty)
    val sortedList = getElementsFrom(heap)
    sortedList.sorted == sortedList
  }

  val sizedLists = for {
    n <- sizedList
    m <- sizedList
  } yield (n, m)

  property("min4") = forAll(sizedLists) { lists =>
    (lists._1.length > 0 && lists._2.length > 0) ==> {
      val h1 = insertAll(lists._1, empty)
      val h2 = insertAll(lists._2, empty)
      findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
    }
  }

  property("meld") = forAll(sizedLists) { lists =>
    (lists._1.length > 0 && lists._2.length > 0) ==> {
      val h1 = insertAll(lists._1, empty)
      val h2 = insertAll(lists._2, empty)
      def heapEqual(h1: H, h2: H): Boolean = {
        if (isEmpty(h1) && isEmpty(h2)) true
        else {
          val m1 = findMin(h1)
          val m2 = findMin(h2)
          m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
        }
      }
      heapEqual(meld(h1, h2),
              meld(deleteMin(h1), insert(findMin(h1), h2)))
    }
  }

  def insertAll(listToAdd: List[Int], heap: H): H = {
    if (listToAdd.isEmpty) { heap }
    else {
      val h = insert(listToAdd.head, heap)
      insertAll(listToAdd.tail, h)
    }
  }

  def getElementsFrom(h: H): List[Int] = { getElementsFrom(h, List[Int]())  }

  def getElementsFrom(h: H, list:List[Int]): List[Int] = {
    if (isEmpty(h)) { list }
    else {
      val newList = list :+ (findMin(h))
      getElementsFrom(deleteMin(h), newList)
    }
  }

  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
