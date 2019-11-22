package d3TestPage

import org.scalajs.dom.html.Div
import rcvcore._

object IdeologyColorPicker {
  private def format(r: Int, g: Int, b: Int) = f"#$r%02x$g%02x$b%02x"
  case class RGB(r: Int, g: Int, b: Int)

  def apply(ideology: Double): String = {
    val gray = RGB(0xA0, 0xA0, 0xA0)
    val partyBase = if (ideology > 0)
      RGB(255, 0, 0)
    else
      RGB(0, 0, 255)

    def blend(g: Int, p: Int): Int = {
      val i_abs = math.min(ideology.abs / 40.0, 1.0)
      (p * i_abs  + g * (1 - i_abs )).toInt
    }
    format(
      blend(gray.r, partyBase.r),
      blend(gray.g, partyBase.g),
      blend(gray.b, partyBase.b)
    )
  }
}

