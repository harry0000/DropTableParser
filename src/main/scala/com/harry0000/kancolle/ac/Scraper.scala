package com.harry0000.kancolle.ac

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.{Document, Element}

import scala.collection.immutable.TreeMap
import scala.collection.{Map, Seq, mutable}

case class Ship(
  number: String,
  rarity: Int,
  category: ShipCategory,
  name: String
)

sealed abstract class ShipCategory(val name: String)
object ShipCategory {
  case object Destroyer       extends ShipCategory("駆逐艦")
  case object LightCruiser    extends ShipCategory("軽巡")
  case object HeavyCruiser    extends ShipCategory("重巡")
  case object SeaplaneTender  extends ShipCategory("水上機母艦")
  case object AircraftCarrier extends ShipCategory("空母")
  case object Submarine       extends ShipCategory("潜水艦")
  case object Battleship      extends ShipCategory("戦艦")

  private val order: Map[ShipCategory, Int] = Seq(
    Destroyer,
    LightCruiser,
    HeavyCruiser,
    SeaplaneTender,
    AircraftCarrier,
    Submarine,
    Battleship
  ).zipWithIndex
    .toMap

  implicit val ordering: Ordering[ShipCategory] = Ordering.by(order.getOrElse(_, Int.MaxValue))

  def apply(shipType: String): ShipCategory = shipType match {
    case "駆逐" => Destroyer
    case "軽巡" => LightCruiser
    case "重巡" => HeavyCruiser
    case "水母" => SeaplaneTender
    case "正母" => AircraftCarrier
    case "軽母" => AircraftCarrier
    case "潜水" => Submarine
    case "戦艦" => Battleship
  }

  def get(index: Int): Option[ShipCategory] = order.find(_._2 == index).map(_._1)
}

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
  def areaType: String = this match {
    case _: Standard => "通常"
    case _: Pursuit  => "追撃"
  }
}

trait Scraper {
  type ShipName = String
  type ShipMap = Map[ShipCategory, Seq[ShipName]]
  def scrape()(implicit browser: JsoupBrowser): Either[String, Seq[(Area, ShipMap)]]
}

object DropListByCardScraper extends Scraper {

  def scrape()(implicit browser: JsoupBrowser): Either[String, Seq[(Area, ShipMap)]] = {
    for {
      order <- getCardOrder(browser.get(Config.cardListPage)).right
      drops <- getDropList(browser.get(Config.dropListByCardPage), order).right
    } yield drops
  }

  protected def getCardOrder(doc: Document): Either[String, Map[ShipName, Int]] = {
    (doc >?> elementList("#body table.style_table tr td"))
      .filter(_.nonEmpty)
      .map { tds =>
        tds.map(_.text.stripPrefix("\n"))
           .zipWithIndex
           .toMap
      }.toRight("Could not find card table.")
  }

  protected def getDropList(doc: Document, order: Map[ShipName, Int]): Either[String, Seq[(Area, ShipMap)]] = {
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
            ShipCategory(tds(2).text),
            (tds(3) >> element("a")).text
          )

          areas.flatMap { case (i, stage) =>
            Mark(tds(i).text) match {
              case Mark.Both => Seq(
                Standard(stage) -> (ship.category -> ship.name),
                Pursuit(stage)  -> (ship.category -> ship.name))
              case Mark.Standard => Seq(
                Standard(stage) -> (ship.category -> ship.name))
              case Mark.Pursuit => Seq(
                Pursuit(stage) -> (ship.category -> ship.name))
              case _ => Nil
            }
          }
        }.sortBy { case (area, (category, name)) =>
          order.getOrElse(name, Int.MaxValue)
        }.foreach { case (area, (category, name)) =>
          val ships = drops.getOrElse(area, TreeMap.empty[ShipCategory, Seq[ShipName]])
          drops.update(
            area,
            ships.updated(
              category,
              ships.getOrElse(category, Seq.empty) :+ name
            )
          )
        }

      drops.toSeq
    }
  }

}

object DropListByAreaScraper extends Scraper {

  def scrape()(implicit browser: JsoupBrowser): Either[String, Seq[(Area, ShipMap)]] = {
    getDropList(browser.get(Config.dropListByAreaPage))
  }

  protected def getDropList(doc: Document): Either[String, Seq[(Area, ShipMap)]] = {
    def parse(tds: Iterable[Element]): ShipMap = {
      tds
        .zipWithIndex
        .flatMap { case (td, i) =>
          (ShipCategory.get(i), td.text) match {
            case (Some(shipType), ships) if ships.nonEmpty => Some(shipType -> ships.split("\\s").toSeq)
            case _ => None
          }
        }.toMap
    }

    for {
      tables <- (doc >?> elementList("#body table.style_table")).filter(_.nonEmpty).toRight("Could not find drop table by area.").right
    } yield {
      tables.flatMap { table =>
        val rows = (table >> elementList("tbody tr")).toArray
        val standard = rows(0) >> elementList("td")
        val pursuit  = rows(1) >> elementList("td")
        val stage = standard.head.text.take(3)
        Seq(
          Standard(stage) -> parse(standard.drop(2)),
          Pursuit(stage)  -> parse(pursuit.drop(1))
        )
      }
    }
  }

}
