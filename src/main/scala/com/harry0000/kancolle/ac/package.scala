package com.harry0000.kancolle

import scala.collection.{Map, Seq, mutable}
import scala.collection.immutable.TreeMap

package object ac {
  type ShipName = String
  type ShipMap = TreeMap[ShipCategory, Seq[ShipName]]
  object ShipMap {
    def empty: ShipMap = TreeMap.empty[ShipCategory, Seq[ShipName]]
    def apply(value: (ShipCategory, Seq[ShipName]) *): ShipMap = TreeMap(value: _*)
  }

  sealed abstract class ShipCategory(val name: String)
  object ShipCategory {
    case object Destroyer       extends ShipCategory("駆逐艦")
    case object LightCruiser    extends ShipCategory("軽巡")
    case object HeavyCruiser    extends ShipCategory("重巡")
    case object SeaplaneTender  extends ShipCategory("水上機母艦")
    case object AircraftCarrier extends ShipCategory("空母")
    case object Submarine       extends ShipCategory("潜水艦")
    case object Battleship      extends ShipCategory("戦艦")

    private val order: Map[ShipCategory, Int] = mutable.LinkedHashMap.empty ++ Seq(
      Destroyer,
      LightCruiser,
      HeavyCruiser,
      SeaplaneTender,
      AircraftCarrier,
      Submarine,
      Battleship
    ).zipWithIndex

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

    def values: Seq[ShipCategory] = order.keys.toSeq
  }
}
