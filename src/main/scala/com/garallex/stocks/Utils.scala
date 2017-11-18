package com.garallex.stocks

object Utils {
  def formatLine(strings: String*): String = strings.foldLeft("")((zero, next) => zero + String.format("%0$-24s", next))
}
