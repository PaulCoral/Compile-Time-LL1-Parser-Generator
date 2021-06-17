package ll1compiletime

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.quoted._


class CompileTimeLL1Test extends AnyFlatSpec {
    import MyToken._
    import ParsingResult._

    val tokens = List(
        ParT(true),
            IntT(1),
            SepT,
            ParT(true),
                IntT(2),
            ParT(false),
        ParT(false)
    )

    "Sum parser" should "return 3 with rest" in {
        val parser = P1.SyntaxDefTesting.parser
        val result = parser(tokens.iterator)
        assert(
            result === ParsedSuccessfully(3)
        )
    }
}