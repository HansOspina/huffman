package com.hansospina

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import com.hansospina.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

    val t3 = Fork(
      Leaf('a', 8),
      Fork(
        Fork(
          Leaf('b', 3),
          Fork(
            Leaf('c', 1),
            Leaf('d', 1),
            List('c', 'd'),
            2
          ),
          List('b', 'c', 'd'),
          5
        ),
        Fork(
          Fork(
            Leaf('e', 1),
            Leaf('f', 1),
            List('e', 'f'),
            2
          ),
          Fork(
            Leaf('g', 1),
            Leaf('h', 1),
            List('g', 'h'),
            2
          ),
          List('e', 'f', 'g', 'h'),
          4
        ),
        List('b', 'c', 'd', 'e', 'f', 'g', 'h'),
        9
      ),
      List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
      17
    )
  }


  val _ = new TestTrees {}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }


  //  test("test createCodeTree") {
  //    assert(createCodeTree(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')) === Nil)
  //  }

  test("decode french code") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }


  test("HuffmanSuite::singleton") {
    singleton(Nil ::: List(Leaf(' ', 3)))
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("combine 2 leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(
      Leaf('e', 1),
      Leaf('t', 2),
      List('e', 't'),
      3
    )))
  }

  test("combine 1 leaf list") {
    val leaflist = List(Leaf('t', 2))
    assert(combine(leaflist) === leaflist)
  }

  test("combine 2 fork list") {
    val leaflist = List(Fork(Leaf('e', 1), Leaf('t', 1), List('e', 't'), 2), Fork(Leaf('q', 1), Leaf('b', 1), List('q', 'b'), 2))
    //assert(combine(leaflist) === leaflist)
  }

  test("combine 3 fork list") {
    val leaflist = List(
      Fork(Leaf('e', 1), Leaf('t', 1), List('e', 't'), 2),
      Fork(Leaf('q', 1), Leaf('b', 1), List('q', 'b'), 2),
      Fork(Leaf('n', 1), Leaf('s', 1), List('n', 's'), 2)
    )
    assert(combine(leaflist) === List(
      Fork(
        Fork(Leaf('e', 1), Leaf('t', 1), List('e', 't'), 2),
        Fork(Leaf('q', 1), Leaf('b', 1), List('q', 'b'), 2),
        List('e', 't', 'q', 'b'),
        4
      ),
      Fork(Leaf('n', 1), Leaf('s', 1), List('n', 's'), 2)
    ))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abbababababababbabababababababababababababbababab".toList)) === "abbababababababbabababababababababababababbababab".toList)
    }
  }


  test("encode with t3") {
    new TestTrees {

      assert(encode(t3)("accf".toList) === List(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1))
    }
  }


  test("encode with empty") {
    new TestTrees {
      assert(encode(t3)("".toList) === List())
    }
  }

  test("test times of empty") {
    assert(times(Nil) === List())
  }

  test("test times") {
    assert(makeOrderedLeafList(times("ababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababaababababababababababababababababababababababababababababababababababa".toList)) === List(Leaf('b', 1224), Leaf('a', 1260)))
  }

  test("decode with t3") {
    new TestTrees {
      assert(decode(t3, List(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0)) === "accfa".toList)
    }
  }

}
