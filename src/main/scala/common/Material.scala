package common

trait Material {
}

case class DiffuseMaterial(diffuseRate: Double = 0.5) extends Material