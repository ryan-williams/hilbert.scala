package com.runsascoded.hilbert

import java.awt.image.BufferedImage
import java.io.File

import com.runsascoded.hilbert.mix.{ Size, color }
import com.runsascoded.math.Permutation
import com.runsascoded.utils.Color
import hammerlab.iterator._
import javax.imageio.ImageIO

import scala.Array.fill

object Main {
  def main(args: Array[String]): Unit = {
    val W = 512
    val H = 512
    val img = new BufferedImage(W, H, BufferedImage.TYPE_INT_RGB)

    import Size._
    val sizes = Seq(`1`, `2`, `3`)

    val permutations =
      Seq(0, 1, 2)
        .permutations
        .toVector

    val overwrite = true

    println(s"Dimensions: ${W}x$H")
    for {
      Size(n, n2, n3, _) ← sizes
    } {
      println(s"Size $n…")

      val R = n3
      val C = n3
      val rows =
        (0 to R)
          .map { r ⇒ r → (r * H / R) }
          .sliding2
          .map {
            case ((r, r1), (_, r2)) ⇒
              (r, r1, r2 - r1)
          }
          .toVector

      val cols =
        (0 to C)
          .map { c ⇒ c → (c * W / C) }
          .sliding2
          .map {
            case ((c, c1), (_, c2)) ⇒
              (c, c1, c2 - c1)
          }
          .toVector

      permutations
        .foreach {
          case p @ Seq(a, b, c) ⇒
            implicit val permutation = Permutation(a, b, c)

            val perm = p.map("rgb"(_)).mkString
            print(s"\tpermutation $perm…")
            val out = new File(s"hilbert-$n3-$W-$perm.jpg")
            if (!overwrite && out.exists())
              println("found!")
            else {
              println("")
              for {
                (r, r1, w) ← rows
                (c, c1, h) ← cols
                Color(red, green, blue) = color(c, r, n, n2)
              } {
                val pixel = (red << 16) | (green << 8) | blue
                val arr = fill(w)(pixel)
                //println(s"($r,$c): ($red,$green,$blue) $pixel, $c1+$w $r1+$h")
                img.setRGB(
                  c1, r1,
                  w, h,
                  arr,
                  0, 0
                )
              }

              print(s"\t\tWriting to $out…")
              ImageIO.write(img, "jpg", out)
              println(" done!")
            }
        }
    }
  }
}
