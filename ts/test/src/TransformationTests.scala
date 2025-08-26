package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import io.github.nguyenyou.internal.*
import io.github.nguyenyou.logging.Logger
import utest.*

object TransformationTests extends TestSuite {
  def tests = Tests {
    test("TsTreeTraverse - collect interfaces") {
      val input = """interface User {
        |  id: number;
        |}
        |class Manager {
        |  users: User[];
        |}
        |interface Config {
        |  debug: boolean;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interfaces = TsTreeTraverse.collect(parsedFile) { case interface: TsDeclInterface =>
            interface
          }
          assert(interfaces.length == 2)
          assert(interfaces(0).name.value == "User")
          assert(interfaces(1).name.value == "Config")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("TsTreeTraverse - collect type references") {
      val input = """interface User {
        |  id: number;
        |  profile: Profile;
        |  settings: UserSettings;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val typeRefs = TsTreeTraverse.collect(parsedFile) { case typeRef: TsTypeRef =>
            typeRef
          }
          // Should find number, Profile, UserSettings type references
          assert(typeRefs.length >= 3)
          val typeNames = typeRefs.map(_.name.parts.last.value).toSet
          assert(typeNames.contains("number"))
          assert(typeNames.contains("Profile"))
          assert(typeNames.contains("UserSettings"))
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("TsTreeTraverse - collect function signatures") {
      val input = """interface API {
        |  getUser(id: string): Promise<User>;
        |  updateUser(id: string, data: Partial<User>): Promise<User>;
        |}
        |declare function helper(): void;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val signatures = TsTreeTraverse.collect(parsedFile) { case sig: TsFunSig =>
            sig
          }
          // Should find 3 function signatures: 2 in interface + 1 function declaration
          assert(signatures.length == 3)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("TsTreeTraverse - collect type parameters") {
      val input = """interface Generic<T, U extends string> {
        |  process<V>(input: T, config: U): V;
        |}
        |class Container<K, V> {
        |  get<R extends V>(key: K): R;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val typeParams = TsTreeTraverse.collect(parsedFile) { case param: TsTypeParam =>
            param
          }
          // Should find T, U, V, K, V, R type parameters
          assert(typeParams.length == 6)
          val paramNames = typeParams.map(_.name.value).toSet
          assert(paramNames.contains("T"))
          assert(paramNames.contains("U"))
          assert(paramNames.contains("V"))
          assert(paramNames.contains("K"))
          assert(paramNames.contains("R"))
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("TsTreeTraverse - collect identifiers") {
      val input = """interface User {
        |  id: number;
        |  name: string;
        |}
        |class Manager {
        |  users: User[];
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val identifiers = TsTreeTraverse.collect(parsedFile) { case ident: TsIdentSimple =>
            ident
          }
          // Should find identifiers like User, id, name, Manager, users
          assert(identifiers.length >= 5)
          val identNames = identifiers.map(_.value).toSet
          assert(identNames.contains("User"))
          assert(identNames.contains("Manager"))
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("FillInTParams - interface with type parameters") {
      val input = """interface Container<T> {
        |  value: T;
        |  getValue(): T;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]

          // Fill in T with string
          val stringType = TsTypeRef(NoComments, TsQIdent.of("string"), Empty)
          val filled     = FillInTParams(interface, IArray(stringType))

          // Should have no type parameters after filling
          assert(filled.tparams.isEmpty)
          // Original should still have type parameters
          assert(interface.tparams.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("FillInTParams - class with type parameters") {
      val input = """class Repository<T, K> {
        |  find(key: K): T | null;
        |  save(entity: T): K;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val classDecl = parsedFile.members.head.asInstanceOf[TsDeclClass]

          // Fill in T with User, K with string
          val userType   = TsTypeRef(NoComments, TsQIdent.of("User"), Empty)
          val stringType = TsTypeRef(NoComments, TsQIdent.of("string"), Empty)
          val filled     = FillInTParams(classDecl, IArray(userType, stringType))

          // Should have no type parameters after filling
          assert(filled.tparams.isEmpty)
          // Original should still have type parameters
          assert(classDecl.tparams.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("FillInTParams - function signature with type parameters") {
      val input = "declare function identity<T>(arg: T): T;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val funcDecl  = parsedFile.members.head.asInstanceOf[TsDeclFunction]
          val signature = funcDecl.signature

          // Fill in T with number
          val numberType = TsTypeRef(NoComments, TsQIdent.of("number"), Empty)
          val filled     = FillInTParams(signature, IArray(numberType))

          // Should have no type parameters after filling
          assert(filled.tparams.isEmpty)
          // Original should still have type parameters
          assert(signature.tparams.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("FillInTParams - type alias with type parameters") {
      val input = "type Mapper<T, U> = (input: T) => U;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]

          // Fill in T with string, U with number
          val stringType = TsTypeRef(NoComments, TsQIdent.of("string"), Empty)
          val numberType = TsTypeRef(NoComments, TsQIdent.of("number"), Empty)
          val filled     = FillInTParams(typeAlias, IArray(stringType, numberType))

          // Should have no type parameters after filling
          assert(filled.tparams.isEmpty)
          // Original should still have type parameters
          assert(typeAlias.tparams.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("FillInTParams - inline type parameters in function") {
      val input = """declare function process<T extends string, U = number>(
        |  input: T,
        |  config?: U
        |): Promise<T>;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val funcDecl  = parsedFile.members.head.asInstanceOf[TsDeclFunction]
          val signature = funcDecl.signature

          // Inline type parameters using defaults/bounds
          val inlined = FillInTParams.inlineTParams(signature)

          // Should have no type parameters after inlining
          assert(inlined.tparams.isEmpty)
          // Original should still have type parameters
          assert(signature.tparams.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("RemoveComment - function members") {
      val input = """interface API {
        |  /** Get user by ID */
        |  getUser(id: string): User;
        |  /** Update user data */
        |  updateUser(id: string, data: Partial<User>): User;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          val functions = interface.members.collect { case func: TsMemberFunction =>
            func
          }

          // Test removing comments from first function only
          val withoutComments = RemoveComment.keepFirstOnly(functions)

          // First function should keep comments, others should have them removed
          assert(withoutComments.length == functions.length)
        // This is a basic test - in practice, comments would be compared
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }
  }
}
