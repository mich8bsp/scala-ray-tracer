package graphics

import common.HittableObject

import scala.collection.mutable

class Scene {
  val sceneObjects: mutable.Buffer[HittableObject] = mutable.Buffer()

  def addToScene(obj: HittableObject): Scene = {
    sceneObjects.append(obj)
    this
  }

}
