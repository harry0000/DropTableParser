package com.harry0000.kancolle.ac

import com.typesafe.config.ConfigFactory

object Config {
  private lazy val config = ConfigFactory.load()

  def dropsByAreaPage: String = config.getString("wiki.drop.area.page")

  def dropsByCardPage: String = config.getString("wiki.drop.card.page")

  def cardsPage: String = config.getString("wiki.card.type.page")

  def markBoth: String = config.getString("wiki.drop.card.mark.both")

  def markStandard: String = config.getString("wiki.drop.card.mark.standard")

  def markPursuit: String = config.getString("wiki.drop.card.mark.pursuit")
}
