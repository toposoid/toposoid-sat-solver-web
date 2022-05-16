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

package controllers

import cnf.FormulaUtils.{evaluateFormula, makeFormula, makeSubFormula}
import cnf.{Clause, Formula, Literal, Tseitin}
import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.protocol.model.sat.{FlattenedKnowledgeTree, FormulaSet, SatSolverResult}
import com.typesafe.scalalogging.LazyLogging

import javax.inject._
import play.api._
import play.api.libs.json.Json
import play.api.mvc._

import java.io.PrintWriter
import scala.math.{abs, log10}
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{Process, ProcessLogger}
import io.jvm.uuid.UUID
import java.nio.file.{Paths, Files}

/**
 * This controller creates an `Action` to apply the satisfiability problem (SAT) to the formula.
 */
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController with LazyLogging {

  /**
   * This function receives an RPN-formatted formula as JSON and processes it as follows.
   *　1. Parse the RPN format and generate a Fomula type formula.
   *　2. Convert Fomula type logical expression to CNF format by Tseitin algorithm.
   *　3. Search for a solution with SatSolver.
   *　4. Convert the result to Json and return Response.
   * @return
   */
  def execute()  = Action(parse.json) { request =>
    try {
      val json = request.body
      val flattenedKnowledgeTree : FlattenedKnowledgeTree = Json.parse(json.toString).as[FlattenedKnowledgeTree]
      val regulationCnf:Set[Clause] = getCnfExpression(flattenedKnowledgeTree.regulation)
      val hypothesisCnf:Set[Clause] = getCnfExpression(flattenedKnowledgeTree.hypothesis)
      val cnfFile = convertCnf(regulationCnf, hypothesisCnf)
      val(status:Int, output:List[String], error:List[String]) = this.executeProcess(Seq(conf.getString("maxsat.solver"), cnfFile))
      Ok(Json.toJson(this.getSatSolverResult(status, output, error, flattenedKnowledgeTree.hypothesis))).as(JSON)

    }catch{
      case e: Exception => {
        logger.error(e.toString, e)
        BadRequest(Json.obj("status" ->"Error", "message" -> e.toString()))
      }
    }
  }

  private def getCnfExpression(formulaSet:FormulaSet): Set[Clause] ={
    val convertSubFormulaMap:Map[String, Formula] = formulaSet.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
      (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
    }
    val formula:Formula = convertSubFormulaMap.size match{
      case 1 => convertSubFormulaMap.head._2
      case _ =>{
        formulaSet.formula.split(" ").foldLeft(List.empty[Formula]){
          (acc, x) => makeFormula(convertSubFormulaMap, x, acc)
        }.head
      }
    }
    Tseitin.transform(formula)
  }

  /**
   *
   * @param regulationCnf
   * @param hypothesisCnf
   * @return
   */
  private def convertCnf(regulationCnf:Set[Clause], hypothesisCnf:Set[Clause]): String = {

    val maxAtomNumber:Int = (regulationCnf ++ hypothesisCnf).flatMap(_.literals).filterNot(x => x.toString.startsWith("-_") || x.toString.startsWith("+_")).map(x => abs(x.toString.toInt)).max

    val dummyValuesReg:Set[Literal] = regulationCnf.flatMap(_.literals).filter(x => x.toString.startsWith("-_") || x.toString.startsWith("+_"))
    val maxDummyValReg:Int = dummyValuesReg.size match {
      case 0 => 0
      case _  => dummyValuesReg.map(_.toString.replace("_", "").toInt).max
    }

    val dummyValuesHypo:Set[Literal] = hypothesisCnf.flatMap(_.literals).filter(x => x.toString.startsWith("-_") || x.toString.startsWith("+_"))
    val maxDummyValHypo:Int = dummyValuesHypo.size match {
      case 0 => 0
      case _  => dummyValuesHypo.map(_.toString.replace("_", "").toInt).max
    }

    val convertCnfExpressionReg:Set[Set[Int]] = regulationCnf.map(_.literals.map( x => convertDummyVal(x.toString, maxAtomNumber)))
    val convertCnfExpressionHypo:Set[Set[Int]] = hypothesisCnf.map(_.literals.map( x => convertDummyVal(x.toString, maxAtomNumber + maxDummyValReg)))
    val cnfHeader:String =  "p wcnf %d %d 100\n".format(maxAtomNumber + maxDummyValReg + maxDummyValHypo, regulationCnf.size + hypothesisCnf.size)

    val cnfFilename:String = conf.getString("maxsat.cnfFilePath") + "/"  + UUID.random.toString
    val fileIO = new PrintWriter(cnfFilename)
    fileIO.write(cnfHeader)
    convertCnfExpressionReg.foreach(x => fileIO.write("100 " + x.mkString(" ") + " 0\n") )
    convertCnfExpressionHypo.foreach(x => fileIO.write("10 " + x.mkString(" ") + " 0\n") )
    fileIO.close()

    val source = scala.io.Source.fromFile(cnfFilename, "UTF-8")
    val lines = source.getLines
    logger.info(lines.mkString("\t"))
    Files.deleteIfExists(Paths.get(cnfFilename))
    cnfFilename
  }

  /**
   *　This function converts the result of SatSolever to SatSolverResult type
   * @param status
   * @param output
   * @param error
   * @return
   */
  private def getSatSolverResult(status:Int, output:List[String], error:List[String], formulaSet:FormulaSet): SatSolverResult ={
    logger.debug("processResult:" +  status.toString)
    if(error.size == 0){
      logger.info("OPTIMUM FOUND")
      val solverStatus = output.filter(_.startsWith("s ")).head.replace("s ", "")
      if(solverStatus.indexOf("OPTIMUM") != -1){
        val solverResult:Map[String, Boolean] = output.filter(_.startsWith("v ")).head.split(" ").filterNot(_.equals("v")).foldLeft(Map.empty[String, Boolean]){
          (acc, x) => acc ++ Map(x.replace("-", "") -> (!x.startsWith("-")))
        }
        val convertSubFormulaMap:Map[String, Formula] = formulaSet.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
          (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
        }
        val subFormulaResultMap:Map[String, Boolean] = convertSubFormulaMap.foldLeft(Map.empty[String, Boolean]){
          (acc, x) => acc ++ Map(x._1 -> evaluateFormula(solverResult, x._2))
        }
        SatSolverResult(solverResult, subFormulaResultMap, "OPTIMUM FOUND")
      }else{
        logger.info("Unsatisfied")
        SatSolverResult(Map.empty[String, Boolean],Map.empty[String, Boolean], "UNSATISFIED")
      }
    }else{
      logger.error(error.mkString(" "))
      SatSolverResult(Map.empty[String, Boolean], Map.empty[String, Boolean], "ERROR")
    }
  }

  /**
   *　This function converts a dummy variable added by Tseitin to an integer type.
   * @param atom
   * @param maxNumber
   * @return
   */
  private def convertDummyVal(atom:String, maxNumber:Int): Int = {
    if(atom.contains("+_")) {
      atom.replace("_", "").toInt + maxNumber
    } else if(atom.contains("-_")) {
      atom.replace("_", "").toInt - maxNumber
    } else{
      atom.toInt
    }
  }

  /**
   * Execution of external process
   * @param cmd
   * @return
   */
  private def executeProcess(cmd: Seq[String]):(Int, List[String], List[String] ) = {
    val out = ArrayBuffer[String]()
    val err = ArrayBuffer[String]()

    val logger = ProcessLogger(
      (o: String) => out += o,
      (e: String) => err += e)

    val status = Process(cmd) ! logger
    (status, out.toList, err.toList)
  }


}