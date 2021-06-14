import org.scalameter.api._
import org.scalameter.CurveData
import org.scalameter.picklers.Implicits._
import org.scalameter.utils.Tree

import scala.io

import json._

abstract class BenchmarkFiles extends Bench.OfflineReport {

  // Uncomment files from here.
  val files = Gen.enumeration("file")(
    "normal-100k",
    //"normal-200k",
    //"normal-300k",
    //"normal-400k",
    //"normal-500k",
    //"normal-600k",
    //"normal-700k",
    //"normal-800k",
    //"normal-900k",
    "normal-1M",
    "normal-10M",
  )
}

abstract class BenchmarkTokens extends BenchmarkFiles {
  val tokens = for {
    file <- files
  } yield JSONLexer(io.Source.fromFile("resources/" + file + ".json")).toArray
}

class CompileTimeLL1 extends BenchmarkTokens {
  performance of "Compile Time LL(1)" in {
    measure method "parsingTable and parser.apply" in {
      using(tokens) in { ts =>
        import ll1compiletime._
        val parser = parsingTable.withFunctionTable(SyntaxDef)
        assert(
          parser.apply(ts.iterator) match {
            case ParsedSuccessfully(_) => true
            case _ => false
          }
        )
      }
    }
  }
}