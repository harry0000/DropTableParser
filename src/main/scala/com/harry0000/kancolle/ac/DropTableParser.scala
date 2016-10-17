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
  case object Standard extends Mark
  case object Pursuit extends Mark
  case object Empty extends Mark

  sealed trait Mark
  object Mark {
    lazy val both: String     = Config.markBoth
    lazy val standard: String = Config.markStandard
    lazy val pursuit: String  = Config.markPursuit

    def apply(mark: String): Mark = {
      mark match {
        case `both`     => Both
        case `standard` => Standard
        case `pursuit`  => Pursuit
        case _          => Empty
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      prettyPrint(parse())
    )
  }

  def parse(): Seq[(String, ShipMap)] = {
    val drops = mutable.LinkedHashMap.empty[String, ShipMap]

    val table = JsoupBrowser().get(Config.page) >> element("#body table.style_table")
    val areas = (table >> element("thead tr"))
      .children
      .zipWithIndex
      .collect { case (e, idx) if e.text.matches("[0-9]-[0-9]") =>
        val area = e.text
        drops += (s"$area 通常" -> TreeMap.empty)
        drops += (s"$area 追撃" -> TreeMap.empty)
        (idx, area)
      }

    (table >> elementList("tbody tr"))
      .view
      .flatMap { tr =>
        val tds = tr.children.toArray
        val ship = Ship(
          tds(0).text,
          tds(1).text.toInt,
          tds(2).text,
          (tds(3) >> element("a")).text
        )

        areas.flatMap { case (i, area) =>
          Mark(tds(i).text) match {
            case Both => Seq(
              s"$area 通常" -> (ship.shipType -> ship.name),
              s"$area 追撃" -> (ship.shipType -> ship.name))
            case Standard => Seq(
              s"$area 通常" -> (ship.shipType -> ship.name))
            case Pursuit => Seq(
              s"$area 追撃" -> (ship.shipType -> ship.name))
            case _ => Nil
          }
        }
      }.foreach { case (area, (shipType, name)) =>
        val ships = drops.getOrElse(area, TreeMap.empty)
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

  def prettyPrint(drops: Seq[(String, ShipMap)]): String = {
    val sb = new StringBuilder(8192)
    drops.foreach { case (area, shipMap) =>
      sb.append(area + "\n")
      shipMap.foreach { case (shipType, ships) =>
        sb.append(shipType + "\n")
          sb.append(ships.mkString(" ") + "\n")
      }
      sb.append("\n")
    }
    sb.toString()
  }

}
