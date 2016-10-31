package com.harry0000.kancolle.ac

import com.harry0000.kancolle.ac.Scraper.ShipDrops
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import wvlet.log.{LogFormatter, LogSupport, Logger}

import scala.collection._

object DropTableParser extends LogSupport {
  Logger.scheduleLogLevelScan
  Logger.setDefaultFormatter(LogFormatter.AppLogFormatter)

  def main(args: Array[String]): Unit = {
    implicit val browser = JsoupBrowser()

    DropListByCardScraper.scrape() match {
      case Right(drops) =>
        println(
          prettyPrint(drops)
        )
      case Left(e) =>
        error(e)
    }
  }

  def prettyPrint(drops: Seq[ShipDrops]): String = {
    val sb = new StringBuilder(8192)
    drops.foreach { drop =>
      sb.append(s"${drop.area.stage} ${drop.area.areaType}\n")
      drop.shipMap.foreach { case (category, ships) =>
        sb.append(category.name + "\n")
        sb.append(ships.mkString(" ") + "\n")
      }
      sb.append("\n")
    }
    sb.toString()
  }

}
