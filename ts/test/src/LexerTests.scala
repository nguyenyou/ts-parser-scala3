package io.github.nguyenyou.internal.ts

import io.github.nguyenyou.internal.ts.parser.*
import utest.*
import scala.util.parsing.input.CharSequenceReader

object LexerTests extends TestSuite {
  def tests = Tests {
    test("TsLexer - identifier tokens") {
      val input   = "myVariable _private $special"
      val scanner = new TsLexer.Scanner(input)

      val token1 = scanner.first
      assert(token1.isInstanceOf[TsLexer.Identifier])
      assert(token1.chars == "myVariable")

      val scanner2 = scanner.rest
      val token2   = scanner2.first
      assert(token2.isInstanceOf[TsLexer.Identifier])
      assert(token2.chars == "_private")

      val scanner3 = scanner2.rest
      val token3   = scanner3.first
      assert(token3.isInstanceOf[TsLexer.Identifier])
      assert(token3.chars == "$special")
    }

    test("TsLexer - keyword tokens") {
      val input   = "interface class function"
      val scanner = new TsLexer.Scanner(input)

      val token1 = scanner.first
      assert(token1.isInstanceOf[TsLexer.Keyword])
      assert(token1.chars == "interface")

      val scanner2 = scanner.rest
      val token2   = scanner2.first
      assert(token2.isInstanceOf[TsLexer.Keyword])
      assert(token2.chars == "class")

      val scanner3 = scanner2.rest
      val token3   = scanner3.first
      assert(token3.isInstanceOf[TsLexer.Keyword])
      assert(token3.chars == "function")
    }

    test("TsLexer - numeric literals") {
      val input   = "42 3.14 0x1A 0b1010"
      val scanner = new TsLexer.Scanner(input)

      val token1 = scanner.first
      assert(token1.isInstanceOf[TsLexer.NumericLit])
      assert(token1.chars == "42")

      val scanner2 = scanner.rest
      val token2   = scanner2.first
      assert(token2.isInstanceOf[TsLexer.NumericLit])
      assert(token2.chars == "3.14")

      val scanner3 = scanner2.rest
      val token3   = scanner3.first
      assert(token3.isInstanceOf[TsLexer.NumericLit])
      assert(token3.chars == "0x1A")

      val scanner4 = scanner3.rest
      val token4   = scanner4.first
      assert(token4.isInstanceOf[TsLexer.NumericLit])
      assert(token4.chars == "0b1010")
    }

    test("TsLexer - string literals") {
      val input   = """"hello" 'world' "simple""""
      val scanner = new TsLexer.Scanner(input)

      val token1 = scanner.first
      assert(token1.isInstanceOf[TsLexer.StringLit])
      assert(token1.chars == "hello")

      val scanner2 = scanner.rest
      val token2   = scanner2.first
      assert(token2.isInstanceOf[TsLexer.StringLit])
      assert(token2.chars == "world")

      val scanner3 = scanner2.rest
      val token3   = scanner3.first
      assert(token3.isInstanceOf[TsLexer.StringLit])
      assert(token3.chars == "simple")
    }

    test("TsLexer - comment tokens") {
      val input = """// line comment
        |/* block comment */""".stripMargin
      val scanner = new TsLexer.Scanner(input)

      val token1 = scanner.first
      assert(token1.isInstanceOf[TsLexer.CommentLineToken])
      assert(token1.chars.contains("line comment"))

      val scanner2 = scanner.rest
      val token2   = scanner2.first
      assert(token2.isInstanceOf[TsLexer.CommentBlockToken])
      assert(token2.chars.contains("block comment"))
    }

    test("TsLexer - delimiter tokens") {
      val input   = "{ } ( ) [ ] < > . ; , ? : = | & * + - ^ / %"
      val scanner = new TsLexer.Scanner(input)

      val expectedDelimiters = List(
        "{",
        "}",
        "(",
        ")",
        "[",
        "]",
        "<",
        ">",
        ".",
        ";",
        ",",
        "?",
        ":",
        "=",
        "|",
        "&",
        "*",
        "+",
        "-",
        "^",
        "/",
        "%"
      )

      var currentScanner = scanner
      for (expectedDelim <- expectedDelimiters) {
        val token = currentScanner.first
        assert(token.isInstanceOf[TsLexer.Keyword])
        assert(token.chars == expectedDelim)
        currentScanner = currentScanner.rest
      }
    }

    test("TsLexer - TypeScript specific delimiters") {
      val input   = "... => -? +? -readonly +readonly"
      val scanner = new TsLexer.Scanner(input)

      val expectedDelimiters = List("...", "=>", "-?", "+?", "-readonly", "+readonly")

      var currentScanner = scanner
      for (expectedDelim <- expectedDelimiters) {
        val token = currentScanner.first
        assert(token.isInstanceOf[TsLexer.Keyword])
        assert(token.chars == expectedDelim)
        currentScanner = currentScanner.rest
      }
    }

    test("TsLexer - directive tokens") {
      val input   = """/// <reference path="types.d.ts"/>"""
      val scanner = new TsLexer.Scanner(input)

      val token = scanner.first
      assert(token.isInstanceOf[TsLexer.DirectiveToken])
      val directive = token.asInstanceOf[TsLexer.DirectiveToken]
      assert(directive.name == "reference")
      assert(directive.key == "path")
      assert(directive.value == "types.d.ts")
    }

    test("TsLexer - shebang") {
      val input   = "#!/usr/bin/env node"
      val scanner = new TsLexer.Scanner(input)

      val token = scanner.first
      assert(token.isInstanceOf[TsLexer.Shebang])
      assert(token.chars == "#!/usr/bin/env node")
    }

    test("TsLexer - template string literals") {
      val input   = "`hello ${name} world`"
      val scanner = new TsLexer.Scanner(input)

      val token = scanner.first
      assert(token.isInstanceOf[TsLexer.StringTemplateLiteral])
      val template = token.asInstanceOf[TsLexer.StringTemplateLiteral]
      assert(template.tokens.nonEmpty)
    }

    test("TsLexer - whitespace handling") {
      val input   = "  \t\n  identifier  \t\n  "
      val scanner = new TsLexer.Scanner(input)

      // Should skip whitespace and get the identifier
      val token = scanner.first
      assert(token.isInstanceOf[TsLexer.Identifier])
      assert(token.chars == "identifier")
    }

    test("TsLexer - unicode escape sequences") {
      val input   = """"\\u0048\\u0065\\u006C\\u006C\\u006F""""
      val scanner = new TsLexer.Scanner(input)

      val token = scanner.first
      assert(token.isInstanceOf[TsLexer.StringLit])
      // The lexer should handle unicode escapes
      assert(token.chars.contains("\\u"))
    }

    test("TsLexer - hex escape sequences") {
      val input   = """"\\x48\\x65\\x6C\\x6C\\x6F""""
      val scanner = new TsLexer.Scanner(input)

      val token = scanner.first
      assert(token.isInstanceOf[TsLexer.StringLit])
      // The lexer should handle hex escapes
      assert(token.chars.contains("\\x"))
    }

    test("TsLexer - error handling for invalid tokens") {
      val input   = "@#$%^&*()_+"
      val scanner = new TsLexer.Scanner(input)

      // Should handle special characters appropriately
      // Some might be valid delimiters, others might cause errors
      val token = scanner.first
      // At minimum, should not crash
      assert(token != null)
    }
  }
}
