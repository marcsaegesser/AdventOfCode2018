package advent

import scala.annotation.tailrec

// object Fubar {
//   def puzzle() = {
//     var r4 = 0
//     var r5 = r4 | 0x10000
//     r4 = 0x1AF0C5
//     var r1 = r5 & 0xFF
//     r4 = r4 + r1
//     r4 = r4 & 0xFFFFFF
//     r4 = r4 * 0x01016B
//     r4 = r4 & 0xFFFFFF

//   }
// }

object Day21 {
  sealed trait Directive
  case class SetIPReg(reg: Int) extends Directive

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

  sealed trait State
  case object Runnable extends State
  case object Break    extends State
  case object Halt     extends State

  case class ProgramState(ip: Int, ipReg: Option[Int], regs: Vector[Int], state: State, bps: Set[Int], wps: Set[Int], vs: Set[Int], code: Vector[Instruction])
  def mkProgram(ipReg: Option[Int], code: Vector[Instruction]) =
    ProgramState(0, ipReg, Vector(0, 0, 0, 0, 0, 0), Runnable, Set(), Set(), Set(), code)

  def setRegister(state: ProgramState, r: Int, v: Int): ProgramState =
    state.copy(regs=state.regs.updated(r, v))

  def addBreakpoint(state: ProgramState, bp: Int): ProgramState =
    state.copy(bps = state.bps + bp)

  def removeBreakpoint(state: ProgramState, bp: Int): ProgramState =
    state.copy(bps = state.bps - bp)

  def addWatch(state: ProgramState, wp: Int): ProgramState =
    state.copy(wps = state.wps + wp)

  def showState(state: ProgramState): String =
    state match { case ProgramState(ip, ipReg, regs, state, bps, wps, _, code) =>
      s"""$ip [${regs.mkString(" ")}] ${code.lift(ip).getOrElse("")}"""
    }


  @tailrec
  def runMachine(state: ProgramState): ProgramState = {
    if(state.state != Runnable) state
    else if(state.bps.contains(state.ip)) {
      println(showState(state))
      state.copy(state=Break)
    }
    else runMachine(stepMachine(state))
 }

  @tailrec
  def runMachineAndShow(state: ProgramState): ProgramState = {
    Thread.sleep(500)
    if(state.state != Runnable) state
    else runMachineAndShow(stepAndShow(state))
    // if(state.state == Halt) state
    // else if(!state.code.isDefinedAt(state.ip)) state.copy(state=Halt)
    // else if(state.bps.contains(state.ip)) state.copy(state=Break)
    // else runMachineAndShow(stepAndShow(state))
  }

  def stepMachine(state: ProgramState): ProgramState =
    state match { case ProgramState(ip, ipReg, regs, st, bps, wps, vs, code) =>
      if(!code.isDefinedAt(ip)) state.copy(state=Halt)
      else {
        if(wps.contains(ip)) println(showState(state))
        if(ip == 28) {
          println(showState(state))
          if(vs.contains(regs(4))) {
            state.copy(state=Halt)
          } else {
            val r1 = ipReg.map(r => regs.updated(r, ip.toInt)).getOrElse(regs)
            val r2 = evalInstruction(code(ip), r1)
            val nextIP = ipReg.map(r => r2(r).toInt).getOrElse(ip)
            state.copy(ip=nextIP+1, regs=r2, vs=vs+regs(4))
          }
        } else {
          val r1 = ipReg.map(r => regs.updated(r, ip.toInt)).getOrElse(regs)
          val r2 = evalInstruction(code(ip), r1)
          val nextIP = ipReg.map(r => r2(r).toInt).getOrElse(ip)
          state.copy(ip=nextIP+1, regs=r2)
        }
      }
    }

  def stepAndShow(state: ProgramState): ProgramState = {
    val next = stepMachine(state)
    println(showState(next))
    next
  }

  def continue(state: ProgramState): ProgramState =
    state.copy(state=Runnable)

  def evalGT(a: Int, b: Int): Int =
    if(a > b) 1
    else 0

  def evalEQ(a: Int, b: Int): Int =
    if(a == b) 1
    else 0

  def evalInstruction(i: Instruction, regs: Vector[Int]): Vector[Int] = {
      i match {
        case Addr(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) + regs(b.toInt))
        case Addi(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) + b)
        case Mulr(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) * regs(b.toInt))
        case Muli(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) * b)
        case Banr(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) & regs(b.toInt))
        case Bani(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) & b)
        case Borr(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) | regs(b.toInt))
        case Bori(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt) | b)
        case Setr(a: Int, b: Int, c: Int) => regs.updated(c, regs(a.toInt))
        case Seti(a: Int, b: Int, c: Int) => regs.updated(c, a)
        case Gtir(a: Int, b: Int, c: Int) => regs.updated(c, evalGT(a, regs(b.toInt)))
        case Gtri(a: Int, b: Int, c: Int) => regs.updated(c, evalGT(regs(a.toInt), b))
        case Gtrr(a: Int, b: Int, c: Int) => regs.updated(c, evalGT(regs(a.toInt), regs(b.toInt)))
        case Eqir(a: Int, b: Int, c: Int) => regs.updated(c, evalEQ(a, regs(b.toInt)))
        case Eqri(a: Int, b: Int, c: Int) => regs.updated(c, evalEQ(regs(a.toInt), b))
        case Eqrr(a: Int, b: Int, c: Int) => regs.updated(c, evalEQ(regs(a.toInt), regs(b.toInt)))
      }
  }

  val directiveRegex = """#(\w+)\s+(\d)""".r
  def parseDirective(l: String): Directive =
    l match {
      case directiveRegex("ip", r) => SetIPReg(r.toInt)
    }

  val instrRegex = """(\w+)\s+(\d+)\s+(\d+)\s+(\d+).*""".r
  def parseInstruction(l: String): Instruction =
    l match {
      case instrRegex("addr", a, b, c) => Addr(a.toInt, b.toInt, c.toInt)
      case instrRegex("addi", a, b, c) => Addi(a.toInt, b.toInt, c.toInt)
      case instrRegex("mulr", a, b, c) => Mulr(a.toInt, b.toInt, c.toInt)
      case instrRegex("muli", a, b, c) => Muli(a.toInt, b.toInt, c.toInt)
      case instrRegex("banr", a, b, c) => Banr(a.toInt, b.toInt, c.toInt)
      case instrRegex("bani", a, b, c) => Bani(a.toInt, b.toInt, c.toInt)
      case instrRegex("borr", a, b, c) => Borr(a.toInt, b.toInt, c.toInt)
      case instrRegex("bori", a, b, c) => Bori(a.toInt, b.toInt, c.toInt)
      case instrRegex("setr", a, b, c) => Setr(a.toInt, b.toInt, c.toInt)
      case instrRegex("seti", a, b, c) => Seti(a.toInt, b.toInt, c.toInt)
      case instrRegex("gtri", a, b, c) => Gtri(a.toInt, b.toInt, c.toInt)
      case instrRegex("gtir", a, b, c) => Gtir(a.toInt, b.toInt, c.toInt)
      case instrRegex("gtrr", a, b, c) => Gtrr(a.toInt, b.toInt, c.toInt)
      case instrRegex("eqir", a, b, c) => Eqir(a.toInt, b.toInt, c.toInt)
      case instrRegex("eqri", a, b, c) => Eqri(a.toInt, b.toInt, c.toInt)
      case instrRegex("eqrr", a, b, c) => Eqrr(a.toInt, b.toInt, c.toInt)
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
