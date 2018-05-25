/*
 * Copyright 2012-2015 Comcast Cable Communications Management, LLC
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

package com.comcast.money.core

import com.comcast.money.api.SpanId
import org.scalatest.{ Matchers, WordSpec }
import Formatters.StringHexHelpers

class FormattersSpec extends WordSpec with Matchers {

  private val expectedTraceId = "a"
  private val expectedTraceIdHex = "61"
  private val expectedParentSpanId = 1
  private val expectedSpanId = 2
  "Http Formatting" should {
    "convert from a money  http header" in {
      val spanId = new SpanId()
      val test = Formatters.toHttpHeader(spanId)
      Formatters.fromHttpHeader(test).get shouldBe spanId
    }

    "convert from all X-B3 http headers" in {
      val expectedLongTraceid = "a" * 32
      val expectedLongTraceidHex = "61" * 32
      val expectedMaxParentSpanIdVal = Long.MaxValue
      val expectedMinSpanIdVal = Long.MinValue

      val actualSpanId = Formatters.fromB3HttpHeaders(expectedLongTraceidHex, Option(expectedMaxParentSpanIdVal.toHexString), Option(expectedMinSpanIdVal.toHexString)).get
      actualSpanId.traceId shouldBe expectedLongTraceid
      actualSpanId.parentId shouldBe expectedMaxParentSpanIdVal
      actualSpanId.selfId() shouldBe expectedMinSpanIdVal
    }

    "convert from 2 X-B3 http headers" in {
      val actualSpanId = Formatters.fromB3HttpHeaders(expectedTraceIdHex, Option(expectedParentSpanId.toHexString), None).get
      actualSpanId.traceId shouldBe expectedTraceId
      actualSpanId.parentId shouldBe expectedParentSpanId.toLong
    }

    "convert from 1 X-B3 http header" in {
      val actualSpanId = Formatters.fromB3HttpHeaders(expectedTraceIdHex, None, None).get
      actualSpanId.traceId shouldBe expectedTraceId
    }

    "convert to x-b3 headers" in {
      val spanId = new SpanId(expectedTraceId, expectedParentSpanId, expectedSpanId)
      Formatters.toB3Headers(spanId)(
        _ shouldBe expectedTraceId.toHexString,
        _ shouldBe expectedParentSpanId.toHexString,
        _ shouldBe expectedSpanId.toHexString
      )
    }
  }

}
