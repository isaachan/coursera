package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math._
import collection.mutable._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == min(a, b)
  }

  property("min2") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  val sizedList = Gen.sized { size =>
    Gen.listOfN(size, Gen.choose(0, 100))
  }

  property("min3") = forAll(sizedList) { list =>
    val heap = insertAll(list, empty)
    val sortedList = getElementsFrom(heap)
    sortedList.sorted == sortedList
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
