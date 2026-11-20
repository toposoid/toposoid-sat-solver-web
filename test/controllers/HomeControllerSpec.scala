/*
 * Copyright (C) 2025  Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package controllers

import cnf.FormulaUtils.{evaluateFormula, makeFormula, makeSubFormula}
import cnf.{And, Formula}
import com.ideal.linked.toposoid.common.{TRANSVERSAL_STATE, TransversalState}
import com.ideal.linked.toposoid.protocol.model.sat.{FlattenedKnowledgeTree, SatSolverResult}
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.Play.materializer
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test._
import play.api.libs.json.Json
import scala.concurrent.duration.DurationInt
import org.apache.pekko.util.Timeout
/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with DefaultAwaitTimeout with Injecting {

  val transversalState:String = Json.toJson(TransversalState(userId="test-user", username="guest", roleId=0, csrfToken = "")).toString()
  override implicit def defaultAwaitTimeout: Timeout = 600.seconds

  "HomeController POST1" should {
    "returns an appropriate response" in {
      val controller: HomeController = inject[HomeController]

      val jsonStr:String = """{
                             |    "regulation": {
                             |        "formula": "1 2 OR 3 OR",
                             |        "subFormulaMap": {
                             |            "1": "1 2 AND 1 3 NOT AND AND",
                             |            "2": "1 2 NOT AND 1 3 AND AND",
                             |            "3": "1 NOT 2 AND 2 3 AND AND"
                             |        }
                             |    },
                             |    "hypothesis": {
                             |        "formula": "1",
                             |        "subFormulaMap": {
                             |            "1": "1 3 NOT IMP 2 1 IMP AND 3 2 NOT IMP AND"
                             |        }
                             |    }
                             |}""".stripMargin

      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalState)
        .withJsonBody(Json.parse(jsonStr))
      val result  = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult = contentAsJson(result).toString()
      val satSolverResult:SatSolverResult = Json.parse(jsonResult).as[SatSolverResult]
      satSolverResult.satResultMap.foreach(x => println(x._1 + ":" +  x._2) )

      val flattenedKnowledgeTree:FlattenedKnowledgeTree = Json.parse(jsonStr).as[FlattenedKnowledgeTree]

      val convertSubFormulaMap1:Map[String, Formula] = flattenedKnowledgeTree.regulation.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
        (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
      }
      val formula1:Formula = flattenedKnowledgeTree.regulation.formula.split(" ").foldLeft(List.empty[Formula]){
        (acc, x) => makeFormula(convertSubFormulaMap1, x, acc)
      }.head
      assert(evaluateFormula(satSolverResult.satResultMap, formula1))
      val convertSubFormulaMap2:Map[String, Formula] = flattenedKnowledgeTree.hypothesis.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
        (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
      }
      val formula2:Formula = flattenedKnowledgeTree.hypothesis.formula.split(" ").foldLeft(List.empty[Formula]){
        (acc, x) => makeFormula(convertSubFormulaMap2, x, acc)
      }.head
      assert(evaluateFormula(satSolverResult.satResultMap, formula2))
      satSolverResult.satResultMap.foreach(x => println(x._1 + ":" + x._2))
    }

  }


  "HomeController POST2" should {
    "returns an appropriate response" in {
      val controller: HomeController = inject[HomeController]
      val jsonStr:String = """{
                             |    "regulation": {
                             |        "formula": "1 2 OR 3 OR",
                             |        "subFormulaMap": {
                             |            "1": "1 2 AND 1 3 NOT AND AND",
                             |            "2": "1 2 NOT AND 1 3 AND AND",
                             |            "3": "1 NOT 2 AND 2 3 AND AND"
                             |        }
                             |    },
                             |    "hypothesis": {
                             |        "formula": "1 6 AND 10 OR",
                             |        "subFormulaMap": {
                             |            "1": "1 2 AND 3 4 OR 4 true AND AND 3 true AND AND IMP",
                             |            "6": "true 7 AND 8 true OR IMP",
                             |            "10": "10 11 AND 10 12 AND AND 13 14 OR IMP"
                             |        }
                             |    }
                             |}""".stripMargin

      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalState)
        .withJsonBody(Json.parse(jsonStr))
      val result  = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult = contentAsJson(result).toString()
      val satSolverResult:SatSolverResult = Json.parse(jsonResult).as[SatSolverResult]
      satSolverResult.satResultMap.foreach(x => println(x._1 + ":" + x._2) )

      val flattenedKnowledgeTree:FlattenedKnowledgeTree = Json.parse(jsonStr).as[FlattenedKnowledgeTree]
      val convertSubFormulaMap1:Map[String, Formula] = flattenedKnowledgeTree.regulation.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
        (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
      }
      val formula1:Formula = flattenedKnowledgeTree.regulation.formula.split(" ").foldLeft(List.empty[Formula]){
        (acc, x) => makeFormula(convertSubFormulaMap1, x, acc)
      }.head
      assert(evaluateFormula(satSolverResult.satResultMap, formula1))

      val convertSubFormulaMap2:Map[String, Formula] = flattenedKnowledgeTree.hypothesis.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
        (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
      }
      val formula2:Formula = flattenedKnowledgeTree.hypothesis.formula.split(" ").foldLeft(List.empty[Formula]){
        (acc, x) => makeFormula(convertSubFormulaMap2, x, acc)
      }.head
      assert(evaluateFormula(satSolverResult.satResultMap, formula2))

      satSolverResult.satResultMap.foreach(x => println(x._1 + ":" + x._2))
    }

  }
  
    "HomeController POST3" should {
    "returns an appropriate response" in {
      val controller: HomeController = inject[HomeController]
      
      val jsonStr:String = """
              |{
              |  "regulation": {
              |    "formula": "fe12c9e1-5535-42a2-aa57-c3a9c2d96109 ac1265f8-41f6-4a40-baa1-de18c15dd39a OR fe12c9e1-5535-42a2-aa57-c3a9c2d96109 d49d6230-4db9-4ac2-a4bb-88fb598725ba OR",
              |    "subFormulaMap": {
              |      "fe12c9e1-5535-42a2-aa57-c3a9c2d96109": "true 2 AND 2 3 NOT AND AND",
              |      "ac1265f8-41f6-4a40-baa1-de18c15dd39a": "4 5 NOT AND 5 NOT 6 AND AND",
              |      "d49d6230-4db9-4ac2-a4bb-88fb598725ba": "true NOT 2 AND 2 3 AND AND"
              |    }
              |  },
              |  "hypothesis": {
              |    "formula": "e5fcaa90-5217-4d46-a2f3-38c17efdad94 06ede8fe-3ae5-4855-a776-13d51bf41ec6 AND e5fcaa90-5217-4d46-a2f3-38c17efdad94 40b7df23-794d-41f3-97b3-0988ec374ab7 AND",
              |    "subFormulaMap": {
              |      "e5fcaa90-5217-4d46-a2f3-38c17efdad94": "true 3 NOT IMP",
              |      "06ede8fe-3ae5-4855-a776-13d51bf41ec6": "2 true IMP",
              |      "40b7df23-794d-41f3-97b3-0988ec374ab7": "3 2 NOT IMP"
              |    }
              |  }
              |}      
      """.stripMargin

      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json", TRANSVERSAL_STATE.str -> transversalState)
        .withJsonBody(Json.parse(jsonStr))
      val result  = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult = contentAsJson(result).toString()
      val satSolverResult:SatSolverResult = Json.parse(jsonResult).as[SatSolverResult]
      satSolverResult.satResultMap.foreach(x => println(x._1 + ":" + x._2) )

      val flattenedKnowledgeTree:FlattenedKnowledgeTree = Json.parse(jsonStr).as[FlattenedKnowledgeTree]
      val convertSubFormulaMap1:Map[String, Formula] = flattenedKnowledgeTree.regulation.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
        (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
      }
      val formula1:Formula = flattenedKnowledgeTree.regulation.formula.split(" ").foldLeft(List.empty[Formula]){
        (acc, x) => makeFormula(convertSubFormulaMap1, x, acc)
      }.head
      assert(evaluateFormula(satSolverResult.satResultMap, formula1))

      val convertSubFormulaMap2:Map[String, Formula] = flattenedKnowledgeTree.hypothesis.subFormulaMap.foldLeft(Map.empty[String, Formula]) {
        (acc, x) => acc ++ Map(x._1 -> x._2.split(" ").foldLeft(List.empty[Formula]){(acc, x) => makeSubFormula(x, acc)}.head)
      }
      val formula2:Formula = flattenedKnowledgeTree.hypothesis.formula.split(" ").foldLeft(List.empty[Formula]){
        (acc, x) => makeFormula(convertSubFormulaMap2, x, acc)
      }.head
      assert(evaluateFormula(satSolverResult.satResultMap, formula2))

      satSolverResult.satResultMap.foreach(x => println(x._1 + ":" + x._2))
    }

  }

  
  /*
  val jsonStr:String = """{
                         |  "formula": "1",
                         |  "subFormulaMap": {
                         |    "1": "1 3 NOT IMP 2 1 IMP AND 3 2 NOT IMP AND"
                         |  }
                         |}""".stripMargin

  val jsonStr:String = """{
                         |  "formula": "1",
                         |  "subFormulaMap": {
                         |    "1": "1 2 NOT IMP 2 3 NOT IMP AND 3 1 IMP AND"
                         |  }
                         |}""".stripMargin
  val jsonStr:String = """{
                         |  "formula": "1",
                         |  "subFormulaMap": {
                         |    "1": "1 3 NOT IMP 2 3 IMP AND 3 1 NOT IMP AND"
                         |  }
                         |}""".stripMargin
  */

}
