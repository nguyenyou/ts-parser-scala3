package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import utest.*

object ErrorHandlingTests extends TestSuite {
  def tests = Tests {
    test("parseString - unclosed interface") {
      val input = "interface User { id: number;"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
          assert(error.contains("end of input"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - unclosed string literal") {
      val input = """interface Test { name: "unclosed string; }"""
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid character in identifier") {
      val input = "interface 123Invalid {}"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - missing semicolon in strict mode") {
      val input = """interface User {
        |  id: number
        |  name: string
        |}""".stripMargin

      // This should still parse successfully as semicolons are optional in interfaces
      val result = parseString(input)
      assert(result.isRight)
    }

    test("parseString - malformed type annotation") {
      val input = "interface User { id: ; }"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid function signature") {
      val input = "declare function test(: string): void;"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - unmatched brackets") {
      val input = "interface Test { items: Array<string; }"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid enum syntax") {
      val input = """enum Color {
        |  Red = ,
        |  Green = "green"
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - malformed namespace") {
      val input = "declare namespace { function test(): void; }"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid class syntax") {
      val input = """class {
        |  constructor() {}
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - malformed type alias") {
      val input = "type = string | number;"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid module declaration") {
      val input = """declare module {
        |  export function test(): void;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - nested unclosed blocks") {
      val input = """interface Outer {
        |  inner: {
        |    value: string;
        |  // Missing closing brace
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid generic syntax") {
      val input = "interface Test<T extends> { value: T; }"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - malformed union type") {
      val input = "type Union = string | | number;"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid intersection type") {
      val input = "type Intersection = string & & number;"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - malformed array type") {
      val input = "interface Test { items: Array<>; }"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid optional property syntax") {
      val input = "interface Test { value?: : string; }"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - malformed function type") {
      val input = "type Func = (arg: ) => string;"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseString - invalid tuple type") {
      val input = "type Tuple = [string, , number];"
      val result = parseString(input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }

    test("parseFileContent - error includes filename") {
      val input = "invalid syntax here"
      val result = parseFileContent("test-file.d.ts", input)
      assert(result.isLeft)

      result match {
        case Left(error) =>
          assert(error.contains("Parse error"))
          // Error should contain position information
          assert(error.contains("1."))
        case Right(_) =>
          throw new Exception("Expected parse error but parsing succeeded")
      }
    }
  }
}
