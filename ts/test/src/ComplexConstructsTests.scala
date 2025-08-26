package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import utest.*

object ComplexConstructsTests extends TestSuite {
  def tests = Tests {
    test("parseString - generic interface with constraints") {
      val input = """interface Repository<T extends Entity> {
        |  find(id: string): T | null;
        |  save(entity: T): Promise<T>;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          assert(interface.name.value == "Repository")
          assert(interface.tparams.length == 1)
          assert(interface.members.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - conditional types") {
      val input = "type NonNullable<T> = T extends null | undefined ? never : T;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "NonNullable")
          assert(typeAlias.tparams.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - mapped types") {
      val input = "type Readonly<T> = { readonly [P in keyof T]: T[P]; };"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "Readonly")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - intersection types") {
      val input = "type Combined = User & Admin & { permissions: string[]; };"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "Combined")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - union types with literals") {
      val input = """type Status = "pending" | "approved" | "rejected";"""

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "Status")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - tuple types") {
      val input = "type Coordinates = [number, number, number?];"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "Coordinates")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - function overloads") {
      val input = """declare function createElement(tag: "div"): HTMLDivElement;
        |declare function createElement(tag: "span"): HTMLSpanElement;
        |declare function createElement(tag: string): HTMLElement;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 3)
          parsedFile.members.foreach { member =>
            val func = member.asInstanceOf[TsDeclFunction]
            assert(func.name.value == "createElement")
          }
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - abstract class") {
      val input = """declare abstract class Shape {
        |  abstract area(): number;
        |  perimeter(): number;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val classDecl = parsedFile.members.head.asInstanceOf[TsDeclClass]
          assert(classDecl.name.value == "Shape")
          assert(classDecl.isAbstract)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - class with inheritance") {
      val input = """class Circle extends Shape implements Drawable {
        |  radius: number;
        |  constructor(radius: number);
        |  area(): number;
        |  draw(): void;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val classDecl = parsedFile.members.head.asInstanceOf[TsDeclClass]
          assert(classDecl.name.value == "Circle")
          assert(classDecl.parent.isDefined)
          assert(classDecl.implements.nonEmpty)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - interface with index signature") {
      val input = """interface Dictionary<T> {
        |  [key: string]: T;
        |  length: number;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          assert(interface.name.value == "Dictionary")
          assert(interface.members.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - interface with call signature") {
      val input = """interface Callable {
        |  (arg: string): number;
        |  property: boolean;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          assert(interface.name.value == "Callable")
          assert(interface.members.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - interface with construct signature") {
      val input = """interface Constructable {
        |  new (arg: string): Instance;
        |  prototype: Instance;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          assert(interface.name.value == "Constructable")
          assert(interface.members.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - const enum") {
      val input = """const enum Direction {
        |  Up,
        |  Down,
        |  Left,
        |  Right
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val enumDecl = parsedFile.members.head.asInstanceOf[TsDeclEnum]
          assert(enumDecl.name.value == "Direction")
          assert(enumDecl.isConst)
          assert(enumDecl.members.length == 4)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - namespace with nested declarations") {
      val input = """declare namespace MyLibrary {
        |  namespace Utils {
        |    function helper(): void;
        |  }
        |  interface Config {
        |    debug: boolean;
        |  }
        |  class Manager {
        |    config: Config;
        |  }
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val namespace = parsedFile.members.head.asInstanceOf[TsDeclNamespace]
          assert(namespace.name.value == "MyLibrary")
          assert(namespace.members.length == 3)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - global augmentation") {
      val input = """declare global {
        |  interface Window {
        |    myCustomProperty: string;
        |  }
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val global = parsedFile.members.head.asInstanceOf[TsGlobal]
          assert(global.members.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - import and export statements") {
      val input = """import { Component } from "react";
        |export interface Props {
        |  children: React.ReactNode;
        |}
        |export default Component;""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 3)
          // Should have import, export interface, and export default
          val hasImport = parsedFile.members.exists(_.isInstanceOf[TsImport])
          val hasExport = parsedFile.members.exists(_.isInstanceOf[TsExport])
          assert(hasImport)
          assert(hasExport)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - template literal types") {
      val input = """type EventName<T extends string> = `on${Capitalize<T>}`;"""

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "EventName")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - keyof operator") {
      val input = "type Keys = keyof User;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "Keys")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - typeof operator") {
      val input = "type ConfigType = typeof config;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "ConfigType")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - infer keyword in conditional types") {
      val input = "type ReturnType<T> = T extends (...args: any[]) => infer R ? R : any;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "ReturnType")
          assert(typeAlias.tparams.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - unique symbol type") {
      val input = "declare const mySymbol: unique symbol;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val varDecl = parsedFile.members.head.asInstanceOf[TsDeclVar]
          assert(varDecl.name.value == "mySymbol")
          assert(varDecl.readOnly)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - readonly modifier on types") {
      val input = "type ReadonlyArray<T> = readonly T[];"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "ReadonlyArray")
          assert(typeAlias.tparams.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - this type") {
      val input = """interface Builder {
        |  setValue(value: string): this;
        |}""".stripMargin

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val interface = parsedFile.members.head.asInstanceOf[TsDeclInterface]
          assert(interface.name.value == "Builder")
          assert(interface.members.length == 1)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - never type") {
      val input = "type Impossible = never;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val typeAlias = parsedFile.members.head.asInstanceOf[TsDeclTypeAlias]
          assert(typeAlias.name.value == "Impossible")
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }

    test("parseString - rest parameters") {
      val input = "declare function combine(first: string, ...rest: string[]): string;"

      val result = parseString(input)
      assert(result.isRight)

      result match {
        case Right(parsedFile) =>
          assert(parsedFile.members.length == 1)
          val funcDecl = parsedFile.members.head.asInstanceOf[TsDeclFunction]
          assert(funcDecl.name.value == "combine")
          assert(funcDecl.signature.params.length == 2)
        case Left(error) =>
          throw new Exception(s"Parse failed: $error")
      }
    }
  }
}
