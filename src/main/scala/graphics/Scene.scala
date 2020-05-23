package graphics

import common.HittableObject

import scala.collection.mutable

class Scene {
  val sceneObjects: mutable.Buffer[HittableObject] = mutable.Buffer()
  var background: Option[Background] = None

  def setBackground(background: Background): Scene = {
    this.background = Some(background)
    this
  }

  def addToScene(obj: HittableObject): Scene = {
    sceneObjects.append(obj)
    this
  }

}
