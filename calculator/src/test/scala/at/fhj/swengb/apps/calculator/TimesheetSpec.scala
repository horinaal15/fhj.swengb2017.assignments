package at.fhj.swengb.apps.calculator

import java.nio.file.Files
import java.nio.file.{Files, Paths}
import org.scalatest.WordSpecLike
import scala.collection.JavaConverters._

class TimesheetSpec extends WordSpecLike {
  "a test" should {
    "work" in {
      val input = """C:\workspace\fhj.swengb2017.assignments\calculator\timesheet-calculator.adoc"""
      println(Files.readAllLines(Paths.get(input)))

    }
  }



}


