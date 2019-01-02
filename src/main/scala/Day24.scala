package advent

import math.Integral.Implicits._
import scala.annotation.tailrec

object Day24 {
  def day24(): Unit = {
    val state = loadData("data/Day24.txt")
    println(s"Day24.part1 = ${showResult(part1(state))}")
    val (boost, units) = part2(state)
    println(s"Day24.part2 = ${units} immune system units remain with a boost of ${boost}")
  }

  def part1(state: State): CombatResult = {
    checkResult(runCombat(state))
  }

  def part2(state: State): (Int, Int) = {
    val (boost, result) = findMinBoost(state)
    (boost, result(ImmuneSystem).groups.values.map(_.units).sum)
  }

  /** Find the minimum boost to the immune system that causes it to win.
    */
  def findMinBoost(state: State): (Int, State) = {
    Stream
      .from(1)
      .map( boost => (boost, runCombat(boostImmuneSystem(state, boost))))
      .dropWhile { case (_, s) =>
        checkResult(s) match {
          case Winner(ImmuneSystem, _) => false
          case _                       => true
        }
      }
      .head
  }

  sealed trait CombatResult
  case class Winner(winner: String, units: Int)             extends CombatResult
  case class Stalemate(immuneUnits: Int, infectionUnits: Int) extends CombatResult

  def showResult(result: CombatResult): String =
    result match {
      case Winner(w, u)                           => s"$w wins with $u units."
      case Stalemate(immuneUnits, infectionUnits) => s"Stalemat:  ImmuneSystemUnits = $immuneUnits, InfectionUnits=$infectionUnits"
    }

  def checkResult(state: State): CombatResult = {
    val immuneUnits = state(ImmuneSystem).groups.values.map(_.units).sum
    val infectionUnits = state(Infection).groups.values.map(_.units).sum

    if(immuneUnits > 0 && infectionUnits > 0) Stalemate(immuneUnits, infectionUnits)
    else if(immuneUnits > 0)                 Winner(ImmuneSystem, immuneUnits)
    else                                     Winner(Infection, infectionUnits)
  }


  sealed trait AttackType
  case object Radiation  extends AttackType
  case object Fire       extends AttackType
  case object Cold       extends AttackType
  case object Bludeoning extends AttackType
  case object Slashing   extends AttackType

  def mkAttack(s: String): AttackType =
    s match {
      case "radiation"   => Radiation
      case "fire"        => Fire
      case "cold"        => Cold
      case "bludgeoning" => Bludeoning
      case "slashing"    => Slashing
    }

  case class Group(id: Int, units: Int, hp: Int, attack: AttackType, damage: Int, initiative: Int, immunities: Set[AttackType], weaknesses: Set[AttackType]) {
    def effectivePower: Int = units * damage
  }

  val Infection    = "Infection"
  val ImmuneSystem = "Immune System"

  case class Army(name: String, groups: Map[Int, Group])

  type State = Map[String, Army]

  case class TargetSelection(attackInitiative: Int, attackArmy: String, attackGroup: Int, defendArmy: String, defendGroup: Int, dammage: Int)

  def boostImmuneSystem(state: State, boost: Int): State = {
    val immuneArmy = state(ImmuneSystem)
    val boosted =
      immuneArmy.groups.map { case (id, g) =>
        (id, g.copy(damage=g.damage+boost))
      }
    state.updated(ImmuneSystem, immuneArmy.copy(groups=boosted))
  }

  @tailrec
  def runCombat(state: State): State = {
    if(isCombatOver(state)) state
    else{
      val nextState = runAttack(state, selectTargets(state))
      if(nextState != state) runCombat(nextState)
      else                  nextState
    }
  }

  def isCombatOver(state: State): Boolean =
    state(ImmuneSystem).groups.size == 0 || state(Infection).groups.size == 0

  def attackDamage(attackGroup: Group, defendGroup: Group): Int =
    if(defendGroup.weaknesses.contains(attackGroup.attack))      attackGroup.effectivePower * 2
    else if(defendGroup.immunities.contains(attackGroup.attack)) 0
    else                                                         attackGroup.effectivePower


  def selectTargets(state: State) = {
    @tailrec
    def helper(accum: List[TargetSelection], attackArmy: String, attack: List[Group], defendArmy: String, defend: List[Group]): List[TargetSelection] = {
      if(attack.isEmpty || defend.isEmpty) accum
      else {
        val target =
          defend
            .map(g => (attackDamage(attack.head, g), g))
            .filterNot(_._1 == 0).sortBy { case (d, g) => (-d, -g.effectivePower, -g.initiative) }
            .headOption
            .map(t => TargetSelection(attack.head.initiative, attackArmy, attack.head.id, defendArmy, t._2.id, t._1))

        target match {
          case Some(t) => helper(t :: accum, attackArmy, attack.tail, defendArmy, defend.filterNot(_.id == t.defendGroup))
          case None    => helper(accum, attackArmy, attack.tail, defendArmy, defend)
        }
      }
    }

    val immuneAttack = state(ImmuneSystem).groups.values.toList.sortBy(g => (-g.effectivePower, -g.initiative))
    val ts = helper(List(), ImmuneSystem, immuneAttack, Infection, state(Infection).groups.values.toList)

    val infectionAttack = state(Infection).groups.values.toList.sortBy(g => (-g.effectivePower, -g.initiative))
    helper(ts, Infection, infectionAttack, ImmuneSystem, state(ImmuneSystem).groups.values.toList)
  }

  def runAttack(state: State, targets: List[TargetSelection]): State = {
    def helper(s: State, ts: List[TargetSelection]): State = {
      if(s(ImmuneSystem).groups.size == 0)   s
      else if(s(Infection).groups.size == 0) s
      else {
        ts match {
          case Nil       => s
          case TargetSelection(_, aa, ag, da, dg, d) :: rest =>
            if(s(aa).groups.contains(ag)) {
              val dArmy = s(da)
              val dGroup = dArmy.groups(dg)
              val aGroup = s(aa).groups(ag)
              val damage = attackDamage(aGroup, dGroup)
              val unitsKilled = damage / dGroup.hp
              val unitsRemaining = dGroup.units - unitsKilled
              // println(s"$aa($ag) -> $da($dGroup) ==> $unitsKilled/$unitsRemaining")
              val nextState =
                if(unitsRemaining > 0) s.updated(da, dArmy.copy(groups=dArmy.groups.updated(dg, dGroup.copy(units=unitsRemaining))))
                else                   s.updated(da, dArmy.copy(groups=dArmy.groups - dg))
              helper(nextState, rest)
            } else {
              helper(s, rest)   // The attacking group has already been destroyed
            }
        }
      }
    }

    helper(state, targets.sortBy(-_.attackInitiative))
  }

  def showArmy(army: Army): String = {
    val builder = StringBuilder.newBuilder
    builder ++= s"${army.name}\n"
    army.groups.values.toList.sortBy(_.id).foldLeft(builder) { case (sb, g) => sb ++= s"${g.id} ${g.units} units with ${g.hp} HP. Attack = ${g.damage}\n" }

    builder.toString
  }

  def showState(state: State): String = {
    val builder = StringBuilder.newBuilder
    builder ++= showArmy(state(ImmuneSystem))
    builder ++= "\n"
    builder ++= showArmy(state(Infection))
    builder ++= "\n"

    builder.toString
  }

  val nameRegex = """([\w\s]+):""".r
  val groupRegex = """(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  val attributesRegex2 = """\((.*);\s(.*)\) """.r
  val attributesRegex1 = """\((.*)\) """.r
  val immuneRegex = """immune to (.*)""".r
  val weakRegex = """weak to (.*)""".r

  def parseGroup(id: Int, line: String): Group = {
    val groupRegex(n, h, as, d, a, i) = line
    val (is, ws) = Option(as).map(parseAttributes).getOrElse((Set.empty[AttackType], Set.empty[AttackType]))
    Group(id, n.toInt, h.toInt, mkAttack(a), d.toInt, i.toInt, is, ws)
  }

  def parseAttributes(attrs: String): (Set[AttackType], Set[AttackType]) = {
    val as =
      attrs match {
        case attributesRegex2(a1, a2) => List(a1, a2)
        case attributesRegex1(a1) => List(a1)
      }

    (parseImmune(as), parseWeak(as))
  }

  def parseImmune(ss: List[String]): Set[AttackType] =
    ss.map {
      case immuneRegex(is) => is.split(",").map(i => mkAttack(i.trim)).toList
      case weakRegex(_) => List()
    }.flatten.toSet

  def parseWeak(ss: List[String]): Set[AttackType] =
    ss.map {
      case immuneRegex(_) => List()
      case weakRegex(ws)  => ws.split(",").map(w => mkAttack(w.trim)).toList
    }.flatten.toSet

  def parseArmy(lines: List[String]): Army = {
    val nameRegex(name) = lines.head
    val groups = Stream.from(1).zip(lines.tail).map { case (i, l) => parseGroup(i, l) }.toList
    Army(name, groups.map(g => (g.id, g)).toMap)
  }

  def loadData(file: String): State = {
    val lines = io.Source.fromFile(file).getLines.toList
    val a1 = parseArmy(lines.takeWhile(!_.isEmpty))
    val a2 = parseArmy(lines.dropWhile(!_.isEmpty).filterNot(_.isEmpty))
    Map(a1.name -> a1, a2.name -> a2)
  }
}
