package advent

object Day08 {
  case class Tree(meta: List[Int], children: List[Tree]) {
    def fold[B](z: B)(f: (B, Tree) => B): B = {
      val x = f(z, this)
      children.foldLeft(x) { (b, t) => t.fold(b)(f) }
    }
  }

  def day08(): Unit = {
    val input = loadTree("data/Day08.txt")
    println(s"Day08.part1 = ${part1(input)}")
    println(s"Day08.part2 = ${part2(input)}")
  }

  def part1(tree: Tree): Int = {
    tree.fold(0) { (s, t) =>
      s + t.meta.sum
    }
  }

  def part2(tree: Tree): Int =
    nodeValue(tree)



  def nodeValue(tree: Tree): Int =
    tree match {
      case Tree(m, Nil) => m.sum
      case Tree(m, k) =>
        m.fold(0) { (a, i) => if(k.isDefinedAt(i-1)) a + nodeValue(k(i-1)) else a}
    }


  def parseTree(input: List[Int]): (List[Int], Tree) =
    input match {
      case Nil         => throw new Exception("Bad input")
      case _ :: Nil    => throw new Exception("Bad Input")
      case k :: m :: t =>
        val (r, ks) = parseN(k, t)    // parse k children
        val (meta, rr) = r.splitAt(m) // parse m meta
        (rr, Tree(meta, ks))

    }

  def parseN(n: Int, input: List[Int]): (List[Int], List[Tree]) = {
    def helper(remaining: Int, accum: List[Tree], in: List[Int]): (List[Int], List[Tree]) =
      if(remaining == 0) (in, accum.reverse)
      else {
        val (r, c) = parseTree(in)
        helper(remaining-1, c :: accum, r)
      }

    helper(n, List(), input)
  }

  def loadTree(file: String): Tree =
    parseTree(loadData(file))._2

  def loadData(file: String): List[Int] = {
    io.Source.fromFile(file)
      .getLines()
      .toList
      .map(_.split(" "))
      .flatten.map(_.toInt)
  }
}
