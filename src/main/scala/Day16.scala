package advent

object Day16 {

  def runBeforeAndAfter(sample: BeforeAfterExample) = {
    def mkAll = List(Addr.tupled, Addi.tupled, Mulr.tupled, Muli.tupled, Banr.tupled, Bani.tupled, Borr.tupled, Bori.tupled, Setr.tupled,
      Seti.tupled, Gtir.tupled, Gtri.tupled, Gtrr.tupled, Eqir.tupled, Eqri.tupled, Eqrr.tupled)


    sample match { case BeforeAfterExample(RawInstruction(_, a, b, c), before, after) =>
      val is =mkAll.map(i => i.apply((a, b, c)))
      is.map(i => (i, evalInstruction(ProgramState(before), i)))
        .filter{ case (i, s) => s.regs == after}
    }
  }

  def runBeforeAndAfterX(sample: BeforeAfterExample) = {
    def mkAll = List(Mulr.tupled, Borr.tupled)


    sample match { case BeforeAfterExample(RawInstruction(_, a, b, c), before, after) =>
      val is =mkAll.map(i => i.apply((a, b, c)))
      is.map(i => (i, evalInstruction(ProgramState(before), i)))
        .filter{ case (i, s) => s.regs == after}
    }
  }

  sealed trait Instruction
  case class Addr(a: Int, b: Int, c: Int) extends Instruction
  case class Addi(a: Int, b: Int, c: Int) extends Instruction
  case class Mulr(a: Int, b: Int, c: Int) extends Instruction
  case class Muli(a: Int, b: Int, c: Int) extends Instruction
  case class Banr(a: Int, b: Int, c: Int) extends Instruction
  case class Bani(a: Int, b: Int, c: Int) extends Instruction
  case class Borr(a: Int, b: Int, c: Int) extends Instruction
  case class Bori(a: Int, b: Int, c: Int) extends Instruction
  case class Setr(a: Int, b: Int, c: Int) extends Instruction
  case class Seti(a: Int, b: Int, c: Int) extends Instruction
  case class Gtir(a: Int, b: Int, c: Int) extends Instruction
  case class Gtri(a: Int, b: Int, c: Int) extends Instruction
  case class Gtrr(a: Int, b: Int, c: Int) extends Instruction
  case class Eqir(a: Int, b: Int, c: Int) extends Instruction
  case class Eqri(a: Int, b: Int, c: Int) extends Instruction
  case class Eqrr(a: Int, b: Int, c: Int) extends Instruction

  case class RawInstruction(opcode: Int, a: Int, b: Int, c: Int)

  case class ProgramState(regs: Vector[Int])

  def runProgram(state: ProgramState, prog: List[Instruction]): ProgramState =
    prog match {
      case Nil     => state
      case i :: is => runProgram(evalInstruction(state, i), is)
    }


  def evalGT(a: Int, b: Int): Int =
    if(a > b) 1
    else 0

  def evalEQ(a: Int, b: Int): Int =
    if(a == b) 1
    else 0

  def evalInstruction(state: ProgramState, i: Instruction): ProgramState = {
    state match { case ProgramState(regs) =>
      i match {
        case Addr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) + regs(b)))
        case Addi(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) + b))
        case Mulr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) * regs(b)))
        case Muli(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) * b))
        case Banr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) & regs(b)))
        case Bani(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) & b))
        case Borr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) | regs(b)))
        case Bori(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a) | b))
        case Setr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, regs(a)))
        case Seti(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, a))
        case Gtir(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, evalGT(a, regs(b))))
        case Gtri(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, evalGT(regs(a), b)))
        case Gtrr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, evalGT(regs(a), regs(b))))
        case Eqir(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, evalEQ(a, regs(b))))
        case Eqri(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, evalEQ(regs(a), b)))
        case Eqrr(a: Int, b: Int, c: Int) => state.copy(regs = regs.updated(c, evalEQ(regs(a), regs(b))))
      }
    }
  }

  case class BeforeAfterExample(i: RawInstruction, before: Vector[Int], after: Vector[Int])

  val beforeRegex = """Before:\s+\[(\d+), (\d+), (\d+), (\d+)\]""".r
  def parseBefore(l: String): Vector[Int] =
    l match {
      case beforeRegex(a, b, c, d) => Vector(a.toInt, b.toInt, c.toInt, d.toInt)
    }

  val afterRegex = """After:\s+\[(\d+), (\d+), (\d+), (\d+)\]""".r
  def parseAfter(l: String): Vector[Int] =
    l match {
      case afterRegex(a, b, c, d) => Vector(a.toInt, b.toInt, c.toInt, d.toInt)
    }

  def parseInst(l: String): RawInstruction =
    l.split(" ").map(_.toInt).toList match {
      case op :: a :: b :: c :: Nil => RawInstruction(op, a, b, c)
      case _ => throw new Exception("Bad instruction.")
    }

  def parseBeforeAndAndfer(ls: Seq[String]): BeforeAfterExample = {
    val lines = ls.toVector
    println(s"lines=$lines")
    BeforeAfterExample(parseInst(lines(1)), parseBefore(lines(0)), parseAfter(lines(2)))
  }

  def parseInstruction(l: String): Instruction = {
    l.split(" ").map(_.toInt).toList match {
      case 0 :: a :: b :: c :: Nil => Seti(a, b, c)
      case 1 :: a :: b :: c :: Nil => Eqir(a, b, c)
      case 2 :: a :: b :: c :: Nil => Setr(a, b, c)
      case 3 :: a :: b :: c :: Nil => Gtir(a, b, c)
      case 4 :: a :: b :: c :: Nil => Addi(a, b, c)
      case 5 :: a :: b :: c :: Nil => Muli(a, b, c)
      case 6 :: a :: b :: c :: Nil => Mulr(a, b, c)
      case 7 :: a :: b :: c :: Nil => Gtrr(a, b, c)
      case 8 :: a :: b :: c :: Nil => Bani(a, b, c)
      case 9 :: a :: b :: c :: Nil => Gtri(a, b, c)
      case 10 :: a :: b :: c :: Nil => Bori(a, b, c)
      case 11 :: a :: b :: c :: Nil => Banr(a, b, c)
      case 12 :: a :: b :: c :: Nil => Borr(a, b, c)
      case 13 :: a :: b :: c :: Nil => Eqri(a, b, c)
      case 14 :: a :: b :: c :: Nil => Eqrr(a, b, c)
      case 15 :: a :: b :: c :: Nil => Addr(a, b, c)
      case _ => throw new Exception(s"Bad instruction $l")
    }
  }

  def loadBeforeAfter(file: String): List[BeforeAfterExample] =
    io.Source.fromFile(file).getLines().grouped(4).map(parseBeforeAndAndfer).toList

  def loadProgram(file: String): List[Instruction] =
    io.Source.fromFile(file)
      .getLines().filterNot(_.isEmpty)
      .map(parseInstruction).toList



  // val opCodes: Map[Int, Instruction] = Map(
  //   (14 -> Eqrr)
  //   (13 -> Eqri)
  //   ( 2 -> (Setr) or Gtir )
  //   ( 1 -> Eqrr or (Eqir))
  //   ( 9 -> Gtri )
  //   ( 7 -> Gtrr )
  //   ( 3 => Gtir )
  //   ( 8 -> Bani )
  //   (11 -> Banr)
  //   ( 0 -> Seti )
  //   ( 5 -> Muli )
  //   (10 -> Bori )
  //   (15 -> Addr )
  //   ( 4 -> Addi )
  //   (12 -> Borr )
  //   ( 6 -> Mulr )
  // )

}
