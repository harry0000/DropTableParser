package com.harry0000.kancolle.ac

import com.typesafe.config.ConfigFactory

object Config {
  private lazy val config = ConfigFactory.load()

  def page: String = config.getString("wiki.page")

  def table: String = config.getString("wiki.table")

  def markBoth: String = config.getString("mark.both")

  def markStandard: String = config.getString("mark.standard")

  def markPursuit: String = config.getString("mark.pursuit")
}
