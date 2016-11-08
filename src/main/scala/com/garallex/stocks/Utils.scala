package com.garallex.stocks

object Utils {
//  def format(str: String) = String.format("%0$-10s", str)

  def formatLine(strings: String*) = strings.foldLeft("")((zero, next) => zero + String.format("%0$-24s", next))

}
