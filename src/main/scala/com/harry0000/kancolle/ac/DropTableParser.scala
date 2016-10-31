package com.harry0000.kancolle.ac

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import wvlet.log.{LogFormatter, LogSupport, Logger}

import scala.collection._

object DropTableParser extends LogSupport {
  Logger.scheduleLogLevelScan
  Logger.setDefaultFormatter(LogFormatter.AppLogFormatter)

  def main(args: Array[String]): Unit = {
    implicit val browser = JsoupBrowser()

    DropListScraper.scrape() match {
      case Right(drops) =>
        println(
          prettyPrint(drops)
        )
      case Left(e) =>
        error(e)
    }
  }

  def prettyPrint(drops: Seq[(Area, Map[ShipCategory, Seq[String]])]): String = {
    val sb = new StringBuilder(8192)
    drops.foreach { case (area, shipMap) =>
      sb.append(s"${area.stage} ${area.areaType}\n")
      shipMap.foreach { case (category, ships) =>
        sb.append(category.name + "\n")
        sb.append(ships.mkString(" ") + "\n")
      }
      sb.append("\n")
    }
    sb.toString()
  }

}
