package com.harry0000.kancolle.ac

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import wvlet.log.{LogFormatter, LogSupport, Logger}

import scala.collection._
import scala.collection.immutable.TreeMap

object DropTableParser extends LogSupport {
  Logger.scheduleLogLevelScan
  Logger.setDefaultFormatter(LogFormatter.AppLogFormatter)

  type ShipName = String
  type ShipType = String
  type ShipMap = Map[ShipType, Seq[ShipName]]

  case class Ship(
    number: String,
    rarity: Int,
    shipType: ShipType,
    name: ShipName
  )

  sealed trait Mark
  object Mark {
    case object Standard extends Mark
    case object Pursuit extends Mark
    case object Both extends Mark
    case object Empty extends Mark

    private lazy val both     = Config.markBoth
    private lazy val standard = Config.markStandard
    private lazy val pursuit  = Config.markPursuit

    def apply(mark: String): Mark = {
      mark match {
        case `both`     => Both
        case `standard` => Standard
        case `pursuit`  => Pursuit
        case _          => Empty
      }
    }
  }

  case class Standard(stage: String) extends Area
  case class Pursuit(stage: String) extends Area

  sealed trait Area {
    def stage: String
    def label: String = this match {
      case _: Standard => s"$stage 通常"
      case _: Pursuit  => s"$stage 追撃"
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val browser = JsoupBrowser()

    parse() match {
      case Right(drops) =>
        println(
          prettyPrint(drops)
        )
      case Left(e) =>
        error(e)
    }
  }

  def getCardOrder()(implicit browser: JsoupBrowser): Either[String, Map[ShipName, Int]] = {
    (browser.get(Config.cardPage) >?> elementList("#body table.style_table tr td"))
      .filter(_.nonEmpty)
      .map { tds =>
        tds.map(_.text.stripPrefix("\n"))
           .zipWithIndex
           .toMap
      }.toRight("Could not find card table.")
  }

  def getDropList(order: Map[ShipName, Int])(implicit browser: JsoupBrowser): Either[String, Seq[(Area, ShipMap)]] = {
    val doc = browser.get(Config.dropPage)
    for {
      table  <- (doc   >?> element("#body table.style_table")).toRight("Could not find drop table.").right
      header <- (table >?> element("thead tr")).toRight("Could not find drop table header.").right
      rows   <- (table >?> elementList("tbody tr")).filter(_.nonEmpty).toRight("Could not find drop table rows.").right
    } yield {
      val drops = mutable.LinkedHashMap.empty[Area, ShipMap]
      val areas = header
        .children
        .zipWithIndex
        .collect { case (e, idx) if e.text.matches("[0-9]-[0-9]") =>
          val stage = e.text
          drops += (Standard(stage) -> TreeMap.empty)
          drops += (Pursuit(stage)  -> TreeMap.empty)
          (idx, stage)
        }

      rows
        .flatMap { tr =>
          val tds = tr.children.toArray
          val ship = Ship(
            tds(0).text,
            tds(1).text.toInt,
            tds(2).text,
            (tds(3) >> element("a")).text
          )

          areas.flatMap { case (i, stage) =>
            Mark(tds(i).text) match {
              case Mark.Both => Seq(
                Standard(stage) -> (ship.shipType -> ship.name),
                Pursuit(stage)  -> (ship.shipType -> ship.name))
              case Mark.Standard => Seq(
                Standard(stage) -> (ship.shipType -> ship.name))
              case Mark.Pursuit => Seq(
                Pursuit(stage) -> (ship.shipType -> ship.name))
              case _ => Nil
            }
          }
        }.sortBy { case (area, (shipType, name)) =>
          order.getOrElse(name, Int.MaxValue)
        }.foreach { case (area, (shipType, name)) =>
          val ships = drops.getOrElse(area, TreeMap.empty[ShipType, Seq[ShipName]])
          drops.update(
            area,
            ships.updated(
              shipType,
              ships.getOrElse(shipType, Seq.empty) :+ name
            )
          )
        }

      drops.toSeq
    }
  }

  def parse()(implicit browser: JsoupBrowser): Either[String, Seq[(Area, ShipMap)]] = {
    for {
      order <- getCardOrder().right
      drops <- getDropList(order).right
    } yield drops
  }

  def prettyPrint(drops: Seq[(Area, ShipMap)]): String = {
    val sb = new StringBuilder(8192)
    drops.foreach { case (area, shipMap) =>
      sb.append(area.label + "\n")
      shipMap.foreach { case (shipType, ships) =>
        sb.append(shipType + "\n")
        sb.append(ships.mkString(" ") + "\n")
      }
      sb.append("\n")
    }
    sb.toString()
  }

}
