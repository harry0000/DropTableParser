package com.harry0000.kancolle.ac

import com.harry0000.kancolle.ac.Scraper.{ShipDrops, ShipMap}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import wvlet.log.{LogFormatter, LogSupport, Logger}

object DropTableParser extends LogSupport {
  Logger.scheduleLogLevelScan
  Logger.setDefaultFormatter(LogFormatter.AppLogFormatter)

  def main(args: Array[String]): Unit = {
    implicit val browser = JsoupBrowser()

    (for {
      card <- DropListByCardScraper.scrape().right
      area <- DropListByAreaScraper.scrape().right
    } yield {
      println(Printer.prettyPrint(diff(card, area)))
    }).left
      .foreach(error(_))
  }

  def diff(ds1: Seq[ShipDrops], ds2: Seq[ShipDrops]): Seq[ShipDrops] = {
    val m1 = ds1.flatMap(ShipDrops.unapply).toMap
    val m2 = ds2.flatMap(ShipDrops.unapply).toMap
    val areas = (m1.keys.toSet ++ m2.keys).toSeq.sorted

    areas.foldLeft(Seq.empty[ShipDrops]) { case (z, area) =>
      (m1.get(area), m2.get(area)) match {
        case (Some(ships1), None        ) => z :+ ShipDrops(area, ships1)
        case (None,         Some(ships2)) => z :+ ShipDrops(area, ships2)
        case (Some(ships1), Some(ships2)) =>
          ShipCategory.values.flatMap { c =>
            (ships1.get(c), ships2.get(c)) match {
              case (Some(names1), None        ) => Some(c -> names1)
              case (None,         Some(names2)) => Some(c -> names2)
              case (Some(names1), Some(names2)) => Some(names1 diff names2).filter(_.nonEmpty).map(c -> _)
              case _ => None
            }
          } match {
            case Nil  => z
            case diff => z :+ ShipDrops(area, ShipMap(diff: _*))
          }
        case _ => z
      }
    }
  }

}
