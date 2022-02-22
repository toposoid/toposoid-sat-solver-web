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

import com.ideal.linked.toposoid.protocol.model.sat.SatSolverResult
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.Play.materializer
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test._
import play.api.libs.json.Json
/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "HomeController POST2" should {
    "returns an appropriate response" in {
      val controller: HomeController = inject[HomeController]
      val jsonStr:String = """{
                             |  "formula": "1 6 AND 10 OR",
                             |  "subFormulaMap": {
                             |    "1": "1 2 AND 3 4 OR 4 true AND AND 3 true AND AND IMP",
                             |    "6": "true 7 AND 8 true OR IMP",
                             |    "10": "10 11 AND 10 12 AND AND 13 14 OR IMP"
                             |  }
                             |}""".stripMargin
      val fr = FakeRequest(POST, "/execute")
        .withHeaders("Content-type" -> "application/json")
        .withJsonBody(Json.parse(jsonStr))
      val result  = call(controller.execute(), fr)
      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      val jsonResult = contentAsJson(result).toString()
      val satSolverResult:SatSolverResult = Json.parse(jsonResult).as[SatSolverResult]
      satSolverResult.satResultMap.foreach(x => println(x._1, x._2) )
    }
  }

}
