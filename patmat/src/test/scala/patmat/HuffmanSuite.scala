package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("intermediateTimes"){
    assert(times(string2Chars("hello, world")) === List(('h',1), ('e',1), ('l',3), ('o',2), (',',1), (' ',1), ('w',1), ('r',1), ('d',1)))
    assert(times(string2Chars("")) === List())
    assert(times(string2Chars("wwwwwww")) === List(('w',7)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist1 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val leaflist2 = List(Leaf('e', 4), Leaf('t', 5), Leaf('x', 6))
    assert(combine(leaflist1) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    assert(combine(leaflist2) === List(Leaf('x',6),Fork(Leaf('e',4),Leaf('t',5),List('e', 't'),9)))
    assert(until(singleton, combine)(leaflist1) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)))
    //assert(createCodeTree(string2Chars("Paolo Costa nato a Genova in provincia di Genova"))=== List())
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decodedSecret == List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abdadbb".toList)) === "abdadbb".toList)
      assert(decode(t2, quickEncode(t2)("abdadbb".toList)) === "abdadbb".toList)

    }
  }

  test("CodeTable works as expected") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }
}
