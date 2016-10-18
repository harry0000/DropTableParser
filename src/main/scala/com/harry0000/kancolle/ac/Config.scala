package com.harry0000.kancolle.ac

import com.typesafe.config.ConfigFactory

object Config {
  private lazy val config = ConfigFactory.load()

  def dropPage: String = config.getString("wiki.drop.page")

  def cardPage: String = config.getString("wiki.card.page")

  def markBoth: String = config.getString("wiki.drop.mark.both")

  def markStandard: String = config.getString("wiki.drop.mark.standard")

  def markPursuit: String = config.getString("wiki.drop.mark.pursuit")
}
