package com.garallex.stocks

object Utils {
  def formatLine(strings: String*): String = strings.foldLeft("")((zero, next) => zero + String.format("%0$-24s", next))

  def exceptionToString(e: Throwable): String = e.getMessage + "\n\t" + e.getStackTrace.mkString("\n\t")
}
