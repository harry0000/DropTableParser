package com.harry0000.kancolle.ac

import com.typesafe.config.ConfigFactory

object Config {
  private lazy val config = ConfigFactory.load()

  def dropListByAreaPage: String = config.getString("wiki.drop.area.page")

  def dropListByCardPage: String = config.getString("wiki.drop.card.page")

  def cardListPage: String = config.getString("wiki.card.type.page")

  def markBoth: String = config.getString("wiki.drop.card.mark.both")

  def markStandard: String = config.getString("wiki.drop.card.mark.standard")

  def markPursuit: String = config.getString("wiki.drop.card.mark.pursuit")
}
