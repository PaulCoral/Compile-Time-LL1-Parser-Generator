package ll1compiletime

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.quoted._


class TestP1 extends AnyFlatSpec {
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

    val empty = tokens.head :: tokens.last :: Nil

    "Sum parser" should "return 0" in {
        val parser = P1.SyntaxDefTesting.parser
        val result = parser(empty.iterator)
        assert(
            result === ParsedSuccessfully(0)
        )
    }

    "Sum parser" should "return 3" in {
        val parser = P1.SyntaxDefTesting.parser
        val result = parser(tokens.iterator)
        assert(
            result === ParsedSuccessfully(3)
        )
    }

    "Sum parser" should "return 3 with rest" in {
        val parser = P1.SyntaxDefTesting.parser
        val rest = List(SepT)
        val result = parser((tokens ++ rest).iterator)
        assert(
            result === ParsedSuccessfullyWithRest(3, rest)
        )
    }

    "Sum parser" should "Unexpected end" in {
        val parser = P1.SyntaxDefTesting.parser
        val result = parser((tokens.take(1)).iterator)
        assert(
            result === UnexpectedEnd(Set(getMyKind(tokens.last)))
        )
    }

    "Sum parser" should "Unexpected Token" in {
        val parser = P1.SyntaxDefTesting.parser
        val unexpected = SepT
        val result = parser((unexpected :: tokens).iterator)
        assert(
            result === UnexpectedToken(getMyKind(unexpected),Set(getMyKind(tokens.head)))
        )
    }
}

//===========================================================


class TestP2 extends AnyFlatSpec {
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

    val empty = tokens.head :: tokens.last :: Nil

    "Sum parser" should "return 0" in {
        val parser = P2.SyntaxDefTesting.parser
        val result = parser(empty.iterator)
        assert(
            result === ParsedSuccessfully(0)
        )
    }

    "Sum parser" should "return 3" in {
        val parser = P2.SyntaxDefTesting.parser
        val result = parser(tokens.iterator)
        assert(
            result === ParsedSuccessfully(3)
        )
    }

    "Sum parser" should "return 3 with rest" in {
        val parser = P2.SyntaxDefTesting.parser
        val rest = List(SepT)
        val result = parser((tokens ++ rest).iterator)
        assert(
            result === ParsedSuccessfullyWithRest(3, rest)
        )
    }

    "Sum parser" should "Unexpected end" in {
        val parser = P2.SyntaxDefTesting.parser
        val result = parser((tokens.take(1)).iterator)
        assert(
            result === UnexpectedEnd(Set(getMyKind(tokens.last)))
        )
    }

    "Sum parser" should "Unexpected Token" in {
        val parser = P2.SyntaxDefTesting.parser
        val unexpected = SepT
        val result = parser((unexpected :: tokens).iterator)
        assert(
            result === UnexpectedToken(getMyKind(unexpected),Set(getMyKind(tokens.head)))
        )
    }
}