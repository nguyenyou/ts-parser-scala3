package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import utest.*

object ParserTests extends TestSuite {
  def tests = Tests {
    test("parseString - simple interface") {
      val input = """interface User {
        |  id: number;
        |  name: string;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile != null)
          assert(parsedFile.members.nonEmpty)
          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("parseString - empty input") {
      val result = parseString("")
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile != null)
          assert(parsedFile.members.isEmpty)
          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse of empty input but got error: $error")
      }
    }

    test("parseString - invalid syntax") {
      val input  = "interface User { invalid syntax"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
          error
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseFileContent - with filename") {
      val input  = """declare function hello(): string;"""
      val result = parseFileContent("test.d.ts", input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile != null)
          assert(parsedFile.members.nonEmpty)
          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("cleanedString - removes BOM") {
      val inputWithBOM = "\uFEFF" + "interface Test {}"
      val cleaned      = cleanedString(inputWithBOM)
      assert(!cleaned.startsWith("\uFEFF"))
      assert(cleaned == "interface Test {}")
    }

    test("cleanedString - normalizes line endings") {
      val inputWithCRLF = "interface Test {\r\n  id: number;\r\n}"
      val cleaned       = cleanedString(inputWithCRLF)
      assert(!cleaned.contains("\r\n"))
      assert(cleaned.contains("\n"))
      assert(cleaned == "interface Test {\n  id: number;\n}")
    }

    test("cleanedString - trims whitespace") {
      val inputWithWhitespace = "  \n  interface Test {}  \n  "
      val cleaned             = cleanedString(inputWithWhitespace)
      assert(cleaned == "interface Test {}")
    }

    test("parseString - complex namespace") {
      val input = """declare namespace MyLib {
        |  interface Config {
        |    url: string;
        |    timeout?: number;
        |  }
        |  
        |  function create(config: Config): void;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile != null)
          assert(parsedFile.members.nonEmpty)
          // Verify we have a namespace declaration
          val hasNamespace = parsedFile.members.exists {
            case _: TsDeclNamespace => true
            case _                  => false
          }
          assert(hasNamespace)
          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }
  }
}
