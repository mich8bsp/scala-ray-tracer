package graphics

import common.Common.Color

import scala.collection.mutable

class PostProcessor {


  def process(image: mutable.Buffer[mutable.Buffer[Color]]): mutable.Buffer[mutable.Buffer[Color]] = {
    correctGamma(image)
  }

  private def correctGamma(image: mutable.Buffer[mutable.Buffer[Color]]): mutable.Buffer[mutable.Buffer[Color]] = {
    for(i <- image.indices){
      for(j <- image(i).indices){
        image(i)(j) = image(i)(j).sqrt
      }
    }
    image
  }
}
