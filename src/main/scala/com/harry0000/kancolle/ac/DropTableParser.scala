package com.harry0000.kancolle.ac

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

import scala.collection._
import scala.collection.immutable.TreeMap

object DropTableParser {

  type ShipName = String
  type ShipType = String
  type ShipMap = TreeMap[ShipType, Seq[ShipName]]

  case class Ship(
    number: String,
    rarity: Int,
    shipType: ShipType,
    name: ShipName
  )

  case object Both extends Mark
  case object Empty extends Mark
  case object Standard extends Mark with AreaType
  case object Pursuit extends Mark with AreaType

  sealed trait Mark
  object Mark {
    lazy val both     = Config.markBoth
    lazy val standard = Config.markStandard
    lazy val pursuit  = Config.markPursuit

    def apply(mark: String): Mark = {
      mark match {
        case `both`     => Both
        case `standard` => Standard
        case `pursuit`  => Pursuit
        case _          => Empty
      }
    }
  }

  trait AreaType

  case class Area(
    stage: String,
    areaType: AreaType
  ) {
    def label: String = areaType match {
      case Standard => s"$stage 通常"
      case Pursuit  => s"$stage 追撃"
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val browser = JsoupBrowser()

    println(
      prettyPrint(parse())
    )
  }

  def getCardOrder()(implicit browser: JsoupBrowser): Map[ShipName, Int] = {
    (browser.get(Config.cardPage) >> elementList("#body table.style_table tr td"))
      .map(_.text.stripPrefix("\n"))
      .zipWithIndex
      .toMap
  }

  def parse()(implicit browser: JsoupBrowser): Seq[(Area, ShipMap)] = {
    val drops = mutable.LinkedHashMap.empty[Area, ShipMap]
    val order = getCardOrder()
    val table = browser.get(Config.dropPage) >> element("#body table.style_table")
    val areas = (table >> element("thead tr"))
      .children
      .zipWithIndex
      .collect { case (e, idx) if e.text.matches("[0-9]-[0-9]") =>
        val stage = e.text
        drops += (Area(stage, Standard) -> TreeMap.empty)
        drops += (Area(stage, Pursuit)  -> TreeMap.empty)
        (idx, stage)
      }

    (table >> elementList("tbody tr"))
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
            case Both => Seq(
              Area(stage, Standard) -> (ship.shipType -> ship.name),
              Area(stage, Pursuit)  -> (ship.shipType -> ship.name))
            case Standard => Seq(
              Area(stage, Standard) -> (ship.shipType -> ship.name))
            case Pursuit => Seq(
              Area(stage, Pursuit) -> (ship.shipType -> ship.name))
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
