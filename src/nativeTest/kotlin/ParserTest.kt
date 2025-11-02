package test

import kotlin.test.*
import parser.KodeParser

class ParserTest {
    private fun testParseDoesNotCrash(source: String) {
        val parser = KodeParser()
        parser.parse(source)
    }

    @Test
    fun simpleFunctionDeclaration() {
        testParseDoesNotCrash("fun add{ a: i32, b: i32 }: i32;")
    }

    @Test
    fun functionWithBody() {
        testParseDoesNotCrash("fun add{ a: i32, b: i32 }: i32 ( a + b; );")
    }

    @Test
    fun pointerTypes() {
        testParseDoesNotCrash("i32 ptr p; i32 ptr ptr pp;")
    }

    @Test
    fun helloWorld() {
        testParseDoesNotCrash("alien fun print_string{ s: char ptr }; fun main{}: i32 ( 0; );")
    }
}
