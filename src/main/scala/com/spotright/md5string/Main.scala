package com.spotright.md5string

import scala.collection.mutable

import java.security.MessageDigest

object Main {

  val post = (BigInt(2).pow(128) - 1) / 128

  case class Datum(value: String, md5: BigInt) {
    val (_q, r) = md5 /% post
    val q = _q.intValue
    lazy val _r = (BigDecimal(r) / BigDecimal(post)).setScale(8, BigDecimal.RoundingMode.HALF_UP)

    override
    def toString: String = {
      s"Datum($value, $q, ${_r})"
    }
  }

  val string = mutable.Map.empty[Int,Datum]
  val md5 = MessageDigest.getInstance("MD5")

  val alpha = (('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')).toArray
  def rstr(len: Int): String = {
    val sb = new StringBuilder(len)
    (1 to len).foreach {
      _ =>
      sb += alpha(scala.util.Random.nextInt(alpha.length))
    }
    sb.result()
  }

  def nstr(x: Int, len: Int = 5): String = {
    val sb = new StringBuilder(len)
    var n = x
    (1 to len).foreach {
      _ =>
        val q = n / 62
        val r = n % 62
        sb += alpha(r)
        n = q
    }
    sb.reverse.result()
  }

  val M = BigInt(62).pow(5).intValue

  def main(av: Array[String]) {

    //(1 to 100000000).foreach {
    (0 until M).foreach {
      i =>
        val s = nstr(i)
        val bytes = md5.digest(s.getBytes("UTF8"))
        val bi = BigInt(1, bytes)
        val d = Datum(s, bi)

        // floor
        val mv = string.get(d.q)
        if (mv.isEmpty)
            string(d.q) = d
        else {
          if (d.r < mv.get.r)
            string(d.q) = d
        }
    }

    println("")
    println {
      string.toList.sortBy(_._1).mkString("\n")
    }
  }
}
