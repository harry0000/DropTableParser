package com.harry0000.kancolle.ac

object Printer {

  private val buffer = 8192

  def prettyPrint(drops: Seq[ShipDrops]): String = {
    val sb = new StringBuilder(buffer)
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
