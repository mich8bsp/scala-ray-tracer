package common

import file.ImageWriter
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class RayTracerTest extends AnyFlatSpec with Matchers {

  "color util" should "convert to hex" in {
    var colorHex = Utils.colorToHex((200, 100, 0))
    colorHex shouldBe 0xc86400

    colorHex = Utils.colorToHex((128, 30, 255))
    colorHex shouldBe 0x801eff
  }

  "image writer" should "create image and write to file" in {
    val input = (
      (0 to 50).map(_  =>
        ((0 to 50).map(_ => (255,0,0)) ++ (0 to 50).map(_ => (0,255,0)) ++ (0 to 50).map(_ => (0, 0, 255))).toArray
      ) ++
    (0 to 50).map(_  =>
      ((0 to 50).map(_ => (0,0,0)) ++ (0 to 50).map(_ => (127,127,127)) ++ (0 to 50).map(_ => (255, 255,255))).toArray
      )
      ).toArray
    ImageWriter.writeImageToFile(input, s"test_image.${ImageWriter.DEFAULT_FORMAT}")
  }

  "vec3" should "support all vector functions" in {
    val v = Vec3(10, 20, 30)
    v + 5 shouldBe Vec3(15, 25, 35)
    v + Vec3(0,1,2) shouldBe Vec3(10, 21, 32)
    v - 5 shouldBe Vec3(5, 15, 25)
    v - Vec3(0,1,2) shouldBe Vec3(10, 19, 28)
    -v shouldBe Vec3(-10, -20, -30)
    v * 5 shouldBe Vec3(50, 100, 150)
    v / 5 shouldBe Vec3(2, 4, 6)
    v * Vec3(2,4,5) shouldBe 250
    v x Vec3(2,4,5) shouldBe Vec3(-20, 10, 0)
    v.lengthSquared shouldBe 1400
    v.length shouldBe math.sqrt(1400)
    val unit = Vec3(0,3,4).unit
    unit.x shouldBe 0
    unit.y shouldBe 0.6 +- 0.0001
    unit.z shouldBe 0.8 +- 0.0001
  }
}
