package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import utest.*

object TreeTests extends TestSuite {
  def tests = Tests {
    test("TsParsedFile - basic properties") {
      val input = """interface Example {
        |  value: string;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          // Test basic properties
          assert(parsedFile.members.nonEmpty)
          assert(parsedFile.directives.isEmpty)
          assert(!parsedFile.isStdLib)

          // Test that we can access the interface
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          assert(interface.name.value == "Example")
          assert(interface.members.length == 1)

          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("TsParsedFile - with directives") {
      val input = """/// <reference no-default-lib="true"/>
        |interface Test {
        |  id: number;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          // Should have directives
          assert(parsedFile.directives.nonEmpty)
          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("TsTypeRef - basic type references") {
      val input = """interface User {
        |  id: number;
        |  name: string;
        |  active: boolean;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          val members   = interface.members

          // Check that we have the expected number of properties
          assert(members.length == 3)

          // Check property types
          val idProp = members(0).asInstanceOf[TsMemberProperty]
          assert(idProp.name.value == "id")
          assert(idProp.tpe.isDefined)

          val nameProp = members(1).asInstanceOf[TsMemberProperty]
          assert(nameProp.name.value == "name")
          assert(nameProp.tpe.isDefined)

          val activeProp = members(2).asInstanceOf[TsMemberProperty]
          assert(activeProp.name.value == "active")
          assert(activeProp.tpe.isDefined)

          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("TsDeclFunction - function declarations") {
      val input = """declare function process(input: string, options?: any): Promise<string>;"""

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)

          val func = parsedFile.members.head.asInstanceOf[TsDeclFunction]
          assert(func.name.value == "process")
          assert(func.signature.params.length == 2)
          assert(func.signature.resultType.isDefined)

          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("TsDeclNamespace - namespace declarations") {
      val input = """declare namespace Utils {
        |  function helper(): void;
        |  interface Config {
        |    debug: boolean;
        |  }
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)

          val namespace = parsedFile.members.head.asInstanceOf[TsDeclNamespace]
          assert(namespace.name.value == "Utils")
          assert(namespace.members.length == 2)

          // Check that we have a function and an interface
          val hasFunction  = namespace.members.exists(_.isInstanceOf[TsDeclFunction])
          val hasInterface = namespace.members.exists(_.isInstanceOf[TsDeclInterface])
          assert(hasFunction)
          assert(hasInterface)

          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }

    test("TsTree - asString method") {
      val input = """interface TestInterface {
        |  testMethod(): void;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          val asString  = interface.asString
          assert(asString.contains("TsDeclInterface"))
          assert(asString.contains("TestInterface"))

          parsedFile
        case Left(error) =>
          throw new Exception(s"Expected successful parse but got error: $error")
      }
    }
  }
}
