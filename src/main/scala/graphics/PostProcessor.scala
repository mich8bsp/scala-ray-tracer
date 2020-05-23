package graphics

import common.Common.Color

class PostProcessor {


  def process(image: Array[Array[Color]]): Array[Array[Color]] = {
    correctGamma(image)
  }

  private def correctGamma(image: Array[Array[Color]]): Array[Array[Color]] = {
    //TODO: fix shit performance
    image.map(_.map(c => c.sqrt))
  }
}
