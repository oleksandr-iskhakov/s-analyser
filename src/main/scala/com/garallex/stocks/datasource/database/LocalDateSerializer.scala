package com.garallex.stocks.datasource.database

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.json4s.CustomSerializer
import org.json4s.JsonAST.JString

case object LocalDateSerializer extends CustomSerializer[LocalDate](_ => ( {
  case JString(s) => LocalDate.parse(s)
}, {
  case zdt: LocalDate => JString(zdt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")))
}))
