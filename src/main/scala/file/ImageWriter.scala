package file

import java.awt.image.BufferedImage
import java.io.File

import common.Common.Color
import common.Utils
import javax.imageio.ImageIO

import scala.collection.mutable

object ImageWriter {
  val DEFAULT_FORMAT = "jpg"

  def createBufferedImage(imageBuffer: mutable.Buffer[mutable.Buffer[Color]]): BufferedImage = {
    val rows = imageBuffer.length
    val cols = imageBuffer.map(_.length).min

    val width = cols
    val height = rows
    val outImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for(y <- 0 until height){
      for(x <- 0 until width){
        outImage.setRGB(x, y, Utils.colorToHex(imageBuffer(y)(x)))
      }
    }

    outImage
  }

  def writeImageToFile(imageBuffer: mutable.Buffer[mutable.Buffer[Color]], path: String): Boolean = {
    ImageIO.write(createBufferedImage(imageBuffer), DEFAULT_FORMAT, new File(path))
  }
}
