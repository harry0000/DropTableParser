package com.harry0000.kancolle.ac

import com.harry0000.kancolle.ac.Parser.{Area, ShipMap}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import wvlet.log.{LogFormatter, LogSupport, Logger}

import scala.collection._

object DropTableParser extends LogSupport {
  Logger.scheduleLogLevelScan
  Logger.setDefaultFormatter(LogFormatter.AppLogFormatter)

  def main(args: Array[String]): Unit = {
    implicit val browser = JsoupBrowser()

    Parser.parse() match {
      case Right(drops) =>
        println(
          prettyPrint(drops)
        )
      case Left(e) =>
        error(e)
    }
  }

  def prettyPrint(drops: Seq[(Area, ShipMap)]): String = {
    val sb = new StringBuilder(8192)
    drops.foreach { case (area, shipMap) =>
      sb.append(area.label + "\n")
      shipMap.foreach { case (category, ships) =>
        sb.append(category.label + "\n")
        sb.append(ships.mkString(" ") + "\n")
      }
      sb.append("\n")
    }
    sb.toString()
  }

}
