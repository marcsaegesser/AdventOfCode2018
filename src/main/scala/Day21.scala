package advent

import scala.annotation.tailrec


object Day21 {
  sealed trait Directive
  case class SetIPReg(reg: Int) extends Directive

  sealed trait Instruction
  case class Addr(a: Long, b: Long, c: Int) extends Instruction
  case class Addi(a: Long, b: Long, c: Int) extends Instruction
  case class Mulr(a: Long, b: Long, c: Int) extends Instruction
  case class Muli(a: Long, b: Long, c: Int) extends Instruction
  case class Banr(a: Long, b: Long, c: Int) extends Instruction
  case class Bani(a: Long, b: Long, c: Int) extends Instruction
  case class Borr(a: Long, b: Long, c: Int) extends Instruction
  case class Bori(a: Long, b: Long, c: Int) extends Instruction
  case class Setr(a: Long, b: Long, c: Int) extends Instruction
  case class Seti(a: Long, b: Long, c: Int) extends Instruction
  case class Gtir(a: Long, b: Long, c: Int) extends Instruction
  case class Gtri(a: Long, b: Long, c: Int) extends Instruction
  case class Gtrr(a: Long, b: Long, c: Int) extends Instruction
  case class Eqir(a: Long, b: Long, c: Int) extends Instruction
  case class Eqri(a: Long, b: Long, c: Int) extends Instruction
  case class Eqrr(a: Long, b: Long, c: Int) extends Instruction

  case class ProgramState(ip: Int, ipReg: Option[Int], regs: Vector[Long], code: Vector[Instruction])
  def mkProgram(ipReg: Option[Int], code: Vector[Instruction]) =
    ProgramState(0, ipReg, Vector(0, 0, 0, 0, 0, 0), code)

  def showState(state: ProgramState): String =
    state match { case ProgramState(ip, ipReg, regs, code) =>
      s"""$ip [${regs.mkString(" ")}] ${code.lift(ip).getOrElse("")}"""
    }


  @tailrec
  def runMachine(state: ProgramState): ProgramState = {
    if(!state.code.isDefinedAt(state.ip)) state
    else runMachine(stepMachine(state))
  }

  @tailrec
  def runMachineAndShow(state: ProgramState): ProgramState = {
    Thread.sleep(500)
    if(!state.code.isDefinedAt(state.ip)) state
    else runMachineAndShow(stepAndShow(state))
  }

  def stepMachine(state: ProgramState): ProgramState =
    state match { case ProgramState(ip, ipReg, regs, code) =>
      if(!code.isDefinedAt(ip)) state
      else {
        val r1 = ipReg.map(r => regs.updated(r, ip.toLong)).getOrElse(regs)
        val r2 = evalInstruction(code(ip), r1)
        val nextIP = ipReg.map(r => r2(r).toInt).getOrElse(ip)
        state.copy(ip=nextIP+1, regs=r2)
      }
    }

  def stepAndShow(state: ProgramState): ProgramState = {
    val next = stepMachine(state)
    println(showState(next))
    next
  }

  def evalGT(a: Long, b: Long): Long =
    if(a > b) 1
    else 0

  def evalEQ(a: Long, b: Long): Long =
    if(a == b) 1
    else 0

  def evalInstruction(i: Instruction, regs: Vector[Long]): Vector[Long] = {
      i match {
        case Addr(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) + regs(b.toInt))
        case Addi(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) + b)
        case Mulr(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) * regs(b.toInt))
        case Muli(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) * b)
        case Banr(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) & regs(b.toInt))
        case Bani(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) & b)
        case Borr(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) | regs(b.toInt))
        case Bori(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt) | b)
        case Setr(a: Long, b: Long, c: Int) => regs.updated(c, regs(a.toInt))
        case Seti(a: Long, b: Long, c: Int) => regs.updated(c, a)
        case Gtir(a: Long, b: Long, c: Int) => regs.updated(c, evalGT(a, regs(b.toInt)))
        case Gtri(a: Long, b: Long, c: Int) => regs.updated(c, evalGT(regs(a.toInt), b))
        case Gtrr(a: Long, b: Long, c: Int) => regs.updated(c, evalGT(regs(a.toInt), regs(b.toInt)))
        case Eqir(a: Long, b: Long, c: Int) => regs.updated(c, evalEQ(a, regs(b.toInt)))
        case Eqri(a: Long, b: Long, c: Int) => regs.updated(c, evalEQ(regs(a.toInt), b))
        case Eqrr(a: Long, b: Long, c: Int) => regs.updated(c, evalEQ(regs(a.toInt), regs(b.toInt)))
      }
  }

  val directiveRegex = """#(\w+)\s+(\d)""".r
  def parseDirective(l: String): Directive =
    l match {
      case directiveRegex("ip", r) => SetIPReg(r.toInt)
    }

  val instrRegex = """(\w+)\s+(\d+)\s+(\d+)\s+(\d+)""".r
  def parseInstruction(l: String): Instruction =
    l match {
      case instrRegex("addr", a, b, c) => Addr(a.toLong, b.toLong, c.toInt)
      case instrRegex("addi", a, b, c) => Addi(a.toLong, b.toLong, c.toInt)
      case instrRegex("mulr", a, b, c) => Mulr(a.toLong, b.toLong, c.toInt)
      case instrRegex("muli", a, b, c) => Muli(a.toLong, b.toLong, c.toInt)
      case instrRegex("banr", a, b, c) => Banr(a.toLong, b.toLong, c.toInt)
      case instrRegex("bani", a, b, c) => Bani(a.toLong, b.toLong, c.toInt)
      case instrRegex("borr", a, b, c) => Borr(a.toLong, b.toLong, c.toInt)
      case instrRegex("bori", a, b, c) => Bori(a.toLong, b.toLong, c.toInt)
      case instrRegex("setr", a, b, c) => Setr(a.toLong, b.toLong, c.toInt)
      case instrRegex("seti", a, b, c) => Seti(a.toLong, b.toLong, c.toInt)
      case instrRegex("gtri", a, b, c) => Gtri(a.toLong, b.toLong, c.toInt)
      case instrRegex("gtir", a, b, c) => Gtir(a.toLong, b.toLong, c.toInt)
      case instrRegex("gtrr", a, b, c) => Gtrr(a.toLong, b.toLong, c.toInt)
      case instrRegex("eqir", a, b, c) => Eqir(a.toLong, b.toLong, c.toInt)
      case instrRegex("eqri", a, b, c) => Eqri(a.toLong, b.toLong, c.toInt)
      case instrRegex("eqrr", a, b, c) => Eqrr(a.toLong, b.toLong, c.toInt)
    }

  def parseLine(l: String): Either[Directive, Instruction] =
    if(l.startsWith("#")) Left(parseDirective(l))
    else                  Right(parseInstruction(l))

  def loadData(file: String): ProgramState = {
    val ls = io.Source.fromFile(file).getLines().map(parseLine)
    val (ds, is) =
      ls.foldLeft((Vector.empty[Directive], Vector.empty[Instruction])) { case ((ds, is), x) =>
        x match {
          case Left(d)  => (ds :+ d, is)
          case Right(i) => (ds, is :+ i)
        }
      }
    println(s"ds=$ds")
    val ipReg = ds.lastOption.map {
      _ match {
        case SetIPReg(r) => r
      }
    }
    mkProgram(ipReg, is)
  }
  
}
