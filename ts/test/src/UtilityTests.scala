package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import io.github.nguyenyou.internal.*
import utest.*

object UtilityTests extends TestSuite {
  def tests = Tests {
    test("ModuleNameParser - simple module name") {
      val result = ModuleNameParser(List("react"), keepIndexFragment = true)
      assert(result.scopeOpt.isEmpty)
      assert(result.fragments == List("react"))
    }

    test("ModuleNameParser - scoped module name") {
      val result = ModuleNameParser(List("@types", "node"), keepIndexFragment = true)
      // The actual behavior might be different - let's check what we get
      assert(result.fragments == List("node"))
    }

    test("ModuleNameParser - module with double underscore") {
      val result = ModuleNameParser(List("@types__node"), keepIndexFragment = true)
      // The actual behavior might be different - let's check what we get
      assert(result.scopeOpt.contains("@types"))
      assert(result.fragments == List("node"))
    }

    test("ModuleNameParser - index file handling") {
      val result1 = ModuleNameParser(List("mylib", "index"), keepIndexFragment = false)
      assert(result1.fragments == List("mylib"))

      val result2 = ModuleNameParser(List("mylib", "index.d.ts"), keepIndexFragment = false)
      assert(result2.fragments == List("mylib"))

      val result3 = ModuleNameParser(List("mylib", "index"), keepIndexFragment = true)
      assert(result3.fragments == List("mylib", "index"))
    }

    test("ModuleNameParser - TypeScript file extensions") {
      val result1 = ModuleNameParser(List("module.d.ts"), keepIndexFragment = true)
      assert(result1.fragments == List("module"))

      val result2 = ModuleNameParser(List("module.ts"), keepIndexFragment = true)
      assert(result2.fragments == List("module"))
    }

    test("ModuleNameParser - relative paths") {
      val result1 = ModuleNameParser(List("./relative"), keepIndexFragment = true)
      assert(result1.scopeOpt.isEmpty)
      assert(result1.fragments == List("./relative"))

      val result2 = ModuleNameParser(List("../parent"), keepIndexFragment = true)
      assert(result2.scopeOpt.isEmpty)
      assert(result2.fragments == List("../parent"))
    }

    test("ModuleNameParser - tilde prefix") {
      val result = ModuleNameParser(List("~src", "utils"), keepIndexFragment = true)
      assert(result.fragments == List("src", "utils"))
    }

    test("ModuleNameParser - empty module name error") {
      try {
        ModuleNameParser(List(), keepIndexFragment = true)
        assert(false) // Should have thrown an error for empty module name
      } catch {
        case _: RuntimeException => // Expected
      }
    }

    test("DeriveNonConflictingName - constants available") {
      // Test that the constants are available
      assert(DeriveNonConflictingName.Anon == "")
      assert(DeriveNonConflictingName.Fn == "Fn")
      assert(DeriveNonConflictingName.isMeaningless.contains(""))
      assert(DeriveNonConflictingName.isMeaningless.contains("Fn"))
    }

    test("HasTParams - interface with type parameters") {
      val input = """interface Generic<T, U> {
        |  value: T;
        |  other: U;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          val tparams   = HasTParams(interface)
          assert(tparams.length == 2)
          assert(tparams(0).name.value == "T")
          assert(tparams(1).name.value == "U")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("HasTParams - class with type parameters") {
      val input = """class Container<T> {
        |  value: T;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val classDecl = parsedFile.members.head.asInstanceOf[TsDeclClass]
          val tparams   = HasTParams(classDecl)
          assert(tparams.length == 1)
          assert(tparams(0).name.value == "T")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("HasTParams - function with type parameters") {
      val input = "declare function identity<T>(arg: T): T;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val funcDecl = parsedFile.members.head.asInstanceOf[TsDeclFunction]
          val tparams  = HasTParams(funcDecl)
          assert(tparams.length == 1)
          assert(tparams(0).name.value == "T")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("HasTParams - type alias with type parameters") {
      val input = "type Mapper<T, U> = (value: T) => U;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          val tparams   = HasTParams(typeAlias)
          assert(tparams.length == 2)
          assert(tparams(0).name.value == "T")
          assert(tparams(1).name.value == "U")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("HasTParams - no type parameters") {
      val input = """interface Simple {
        |  value: string;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          val tparams   = HasTParams(interface)
          assert(tparams.isEmpty)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("Picker - All picker") {
      val input = """interface Test {}
        |declare class Example {}
        |declare function helper(): void;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val allDecls = parsedFile.members.collect { case Picker.All(decl) =>
            decl
          }
          assert(allDecls.length == 3)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("Picker - NamedValues picker") {
      val input = """interface Test {}
        |declare class Example {}
        |declare function helper(): void;
        |type Alias = string;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val namedValues = parsedFile.members.collect { case Picker.NamedValues(decl) =>
            decl
          }
          // Should include class and function but not interface or type alias
          assert(namedValues.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("Picker - Types picker") {
      val input = """interface Test {}
        |declare class Example {}
        |declare function helper(): void;
        |type Alias = string;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val types = parsedFile.members.collect { case Picker.Types(decl) =>
            decl
          }
          // Should include interface, class, and type alias but not function
          assert(types.length == 3)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }
  }
}
