import sbt._
import sbt.Keys._
import sbt.inc.{Analysis, AnalysisStore}

object CachedCompilation{


  def settings: Seq[Setting[_]] = Seq(
    (previousCompile in Compile) := {
      streams.value.log.info("Using my stream!")

      val prev = (previousCompile in Compile).value
      Compiler.PreviousAnalysis(Analysis.Empty, prev.setup)
    }
  )

}