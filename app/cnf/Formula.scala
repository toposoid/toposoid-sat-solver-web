/*
 * Copyright 2021 Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//This source is based on the following.
//https://tamura70.gitlab.io/lect-proplogic/org/scala-proplogic.html

package cnf

object FormulaUtils{
  /**
   * Converts an RPN-formatted formula to a Formula type. Target the subFormulaMap of FlattenedKnowledgeTree.
   * @param symbol
   * @param stack
   * @return
   */
  def makeSubFormula (symbol: String, stack: List[Formula]): List[Formula] = stack match {
    case List() => Bool(symbol) :: stack
    case List(_) => Bool(symbol) :: stack
    case x::y::ys => symbol match {
      case "AND" => And(x, y) :: ys
      case "OR" => Or(x, y) :: ys
      case "IMP" => Imp(y, x) :: ys
      case "true" => True :: stack
      case "false" => False :: stack
      case _ => Bool(symbol) :: stack
    }
  }

  /**
   * Converts an RPN-formatted formula to a Formula type. Target the formula of FlattenedKnowledgeTree.
   * @param formulaMap
   * @param symbol
   * @param stack
   * @return
   */
  def makeFormula (formulaMap:Map[String, Formula], symbol: String, stack: List[Formula]): List[Formula] = stack match {
    case List() => Bool(symbol) :: stack
    case List(_) => Bool(symbol) :: stack
    case x::y::ys => symbol match {
      case "AND" => And(getSubFormula(formulaMap, x), getSubFormula(formulaMap,y)) :: ys
      case "OR" => Or(getSubFormula(formulaMap, x), getSubFormula(formulaMap, y)) :: ys
      case _ => Bool(symbol) :: stack
    }
  }

  /**
   * Merge subFormulaMap and formula of FlattenedKnowledgeTree
   * @param formulaMap
   * @param formula
   * @return
   */
  def getSubFormula(formulaMap:Map[String, Formula], formula:Formula):Formula={
    formulaMap.isDefinedAt(formula.toString) match {
      case true => formulaMap.get(formula.toString).get
      case false => formula
    }
  }

  /**
   * Assign truth values to formulas
   * @param satResultMap
   * @param formula
   * @return
   */
  def evaluateFormula(satResultMap:Map[String, Boolean], formula:Formula): Boolean = {
    val assignment: Map[Bool, Boolean] = satResultMap.foldLeft(Map.empty[Bool, Boolean]) { (acc, x) => acc ++ Map(Bool(x._1) -> x._2) }
    formula.value(assignment).get
  }

}

abstract class Formula extends Serializable {
  def && (that: Formula) = And(this, that)
  def || (that: Formula) = Or(this, that)
  def ==> (that: Formula) = Imp(this, that)
  def <==> (that: Formula) = And(Imp(this, that), Imp(that, this))
  def unary_! = Neg(this)
  def isSubformula(f: Formula): Boolean = f match {
    case False | True | Bool(_) =>
      this == f
    case And(f1,f2) =>
      this == f || isSubformula(f1) || isSubformula(f2)
    case Or(f1,f2) =>
      this == f || isSubformula(f1) || isSubformula(f2)
    case Imp(f1,f2) =>
      this == f || isSubformula(f1) || isSubformula(f2)
    case Neg(f1) =>
      this == f || isSubformula(f1)
  }
  def subformulas = Set(this)
  def bools: Set[Bool] = Set()
  def value(assignment: Map[Bool,Boolean]): Option[Boolean]
  def assignments(vs: Set[Bool]): Iterator[Map[Bool,Boolean]] =
    if (vs.isEmpty)
      Iterator(Map())
    else
      assignments(vs.tail).flatMap {
        as => Iterator(as + (vs.head -> false), as + (vs.head -> true))
      }
  def assignments: Iterator[Map[Bool,Boolean]] =
    assignments(bools)
  def satisfiable =
    assignments.exists(this.value(_) == Some(true))
  def valid =
    assignments.forall(this.value(_) == Some(true))
  def satAssignments =
    assignments.filter(this.value(_) == Some(true))
  def equiv(that: Formula) =
    (this <==> that).valid
  def toNNF(implicit negative: Boolean = false): Formula
  def toDNF: Set[Set[Literal]]
  def toCNF: Set[Set[Literal]]
}

object False extends Formula {
  override def toString = "False"
  def value(assignment: Map[Bool,Boolean]) = Some(false)
  def toNNF(implicit negative: Boolean = false) =
    if (negative) True else this
  def toDNF = Set()
  def toCNF = Set(Set())
}

object True extends Formula {
  override def toString = "True"
  def value(assignment: Map[Bool,Boolean]) = Some(true)
  def toNNF(implicit negative: Boolean = false) =
    if (negative) False else this
  def toDNF = Set(Set())
  def toCNF = Set()
}
case class Bool(name: String) extends Formula {
  override def bools = Set(this)
  def value(assignment: Map[Bool,Boolean]) = assignment.get(this)
  def toNNF(implicit negative: Boolean = false) =
    if (negative) Neg(this) else this
  def toDNF = Set(Set(Literal(this, false)))
  def toCNF = Set(Set(Literal(this, false)))
  override def toString = name
}

object Bool {
  var count = 0
  def apply() = synchronized {
    count += 1
    new Bool("_" + count)
  }
}
case class And(f1: Formula, f2: Formula) extends Formula {
  override def subformulas =
    f1.subformulas | f2.subformulas | Set(this)
  override def bools =
    f1.bools | f2.bools
  def value(assignment: Map[Bool,Boolean]) =
    (f1.value(assignment), f2.value(assignment)) match {
      case (Some(true), Some(true)) => Some(true)
      case (Some(false), _) | (_, Some(false)) => Some(false)
      case _ => None
    }
  def toNNF(implicit negative: Boolean = false) =
    if (negative)
      Or(Neg(f1), Neg(f2)).toNNF(false)
    else
      (f1.toNNF(negative), f2.toNNF(negative)) match {
        case (_, False) | (False, _) => False
        case (f1, True) => f1
        case (True, f2) => f2
        case (f1, f2) => And(f1, f2)
      }
  def toDNF =
    for {
      c1 <- f1.toDNF; c2 <- f2.toDNF
      if c1.forall(lit => ! c2.contains(lit.complement))
    } yield c1 | c2
  def toCNF = {
    val cnf = f1.toCNF | f2.toCNF
    if (cnf.contains(Set())) Set(Set()) else cnf
  }
}
case class Or(f1: Formula, f2: Formula) extends Formula {
  override def subformulas =
    f1.subformulas | f2.subformulas | Set(this)
  override def bools =
    f1.bools | f2.bools
  def value(assignment: Map[Bool,Boolean]) =
    (f1.value(assignment), f2.value(assignment)) match {
      case (Some(false), Some(false)) => Some(false)
      case (Some(true), _) | (_, Some(true)) => Some(true)
      case _ => None
    }
  def toNNF(implicit negative: Boolean = false) =
    if (negative)
      And(Neg(f1), Neg(f2)).toNNF(false)
    else
      (f1.toNNF(negative), f2.toNNF(negative)) match {
        case (f1, False) => f1
        case (False, f2) => f2
        case (_, True) | (True, _) => True
        case (f1, f2) => Or(f1, f2)
      }
  def toDNF = {
    val dnf = f1.toDNF | f2.toDNF
    if (dnf.contains(Set())) Set(Set()) else dnf
  }
  def toCNF =
    for {
      c1 <- f1.toCNF; c2 <- f2.toCNF
      if c1.forall(lit => ! c2.contains(lit.complement))
    } yield c1 | c2
}
case class Imp(f1: Formula, f2: Formula) extends Formula {
  override def subformulas =
    f1.subformulas | f2.subformulas | Set(this)
  override def bools =
    f1.bools | f2.bools
  def value(assignment: Map[Bool,Boolean]) =
    (f1.value(assignment), f2.value(assignment)) match {
      case (Some(true), Some(false)) => Some(false)
      case (Some(false), _) | (_, Some(true)) => Some(true)
      case _ => None
    }
  def toNNF(implicit negative: Boolean = false) =
    Or(Neg(f1), f2).toNNF(negative)
  def toDNF = toNNF.toDNF
  def toCNF = toNNF.toCNF
}
case class Neg(f1: Formula) extends Formula {
  override def subformulas =
    f1.subformulas | Set(this)
  override def bools =
    f1.bools
  def value(assignment: Map[Bool,Boolean]) =
    f1.value(assignment) match {
      case Some(v1) => Some(! v1)
      case None => None
    }
  def toNNF(implicit negative: Boolean = false) =
    f1.toNNF(! negative)
  def toDNF = toNNF match {
    case Neg(Bool(p)) =>
      Set(Set(Literal(Bool(p), true)))
    case nnf =>
      nnf.toDNF
  }
  def toCNF = toNNF match {
    case Neg(Bool(p)) =>
      Set(Set(Literal(Bool(p), true)))
    case nnf =>
      nnf.toCNF
  }
}

object Formula extends Serializable {
  implicit def symbol2bool(p: Symbol) = Bool(p.name)
}
case class Literal(p: Bool, negative: Boolean) {
  def complement = Literal(p, ! negative)
  override def toString =
    if (negative) "-" + p else "+" + p
}

case class Clause(literals: Set[Literal] = Set()) {
  def isEmpty =
    literals.isEmpty
  def tautology =
    literals.exists(lit => literals.contains((lit.asInstanceOf[Literal]).complement))
  def contains(lit: Literal) =
    literals.contains(lit)
  def + (lit: Literal) =
    Clause(literals + lit)
  override def toString =
    literals.mkString("{", ", ", "}")
}

object Tseitin {

  def flattenAnd(f: Formula): Set[Formula] = f match {
    case And(f1, f2) =>
      flattenAnd(f1) | flattenAnd(f2)
    case _ =>
      Set(f)
  }

  def flattenOr(f: Formula): Set[Formula] = f match {
    case Or(f1, f2) =>
      flattenOr(f1) | flattenOr(f2)
    case _ =>
      Set(f)
  }

  def transform(formula: Formula): Set[Clause] = {
    var map: Map[Set[Formula], Bool] = Map()
    Bool.count = 0
    def transformNNF(f: Formula): Set[Clause] =
      flattenAnd(f).map(g => {
        val lits = flattenOr(g).map {
          case p: Bool =>
            Literal(p, false)
          case Neg(p: Bool) =>
            Literal(p, true)
          case f: And => {
            val conj = flattenAnd(f)
            if (!map.contains(conj)) {
              val p = Bool()
              map += conj -> p
            }
            Literal(map(conj), false)
          }
        }
        Clause(lits)
      })

    formula.toNNF match {
      case False =>
        Set(Clause())
      case True =>
        Set()
      case nnf => {
        var cnf = transformNNF(nnf)
        while (!map.isEmpty) {
          val (fs, p) = map.head
          map -= fs
          cnf |= fs.flatMap(f => transformNNF(Or(Neg(p), f)))
        }
        cnf
      }
    }
  }

}

