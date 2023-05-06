package chess

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

import cats.syntax.all.*
import Arbitraries.given
import org.scalacheck.Prop.propBoolean
import scala.util.Random
import cats.kernel.Monoid

class NodeTest extends ScalaCheckSuite:

  given HasId[Int, Int] with
    def getId(a: Int): Int = a

  test("mainline size <= node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size <= node.size

  test("mainline is a sub set of node"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.forall(x => node.exists(_ == x))

  test("mainline.size + variations.size == node.size"):
    forAll: (node: Node[Int]) =>
      node.mainline.size + variationsCount(node) == node.size

  test("find ids with mainline as path retuns last mainline node"):
    forAll: (node: Node[Int]) =>
      node.find(node.mainlineValues).get == node.lastMainlineNode

  test("use mainline as path for findPath"):
    forAll: (node: Tree[Int]) =>
      node.findPath(node.mainlineValues) == node.mainline.some

  test("use subset of mainline as path for findPath"):
    forAll: (node: Node[Int]) =>
      node.mainlineValues.size >= 2 ==> {
        val size = Random.nextInt(node.mainlineValues.size - 1) + 1
        val path = node.mainlineValues.take(size)
        node.findPath(path).isDefined == true
      }

  test("use randomPath for findPath"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      path.nonEmpty ==> {
        val found = node.findPath(path).map(_.map(_.value))
        found.isEmpty || (found.isDefined && found.get == path)
      }

  test("modifyAt with mainline == modifyLastMainlineNode"):
    forAll: (node: Node[Int], f: Int => Int) =>
      node.modifyAt(node.mainlineValues, Tree.lift(f)) == node.modifyLastMainlineNode(Node.lift(f)).some

  test("modifyAt and find are consistent"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      def f[A]: TreeMapper[A] = node =>
        node match
          case n: Node[A]      => n
          case v: Variation[A] => v
      node.modifyAt(path, f).flatMap(_.find(path)) == node.find(path)

  test("deleteAt with root value return Some(None)"):
    forAll: (node: Node[Int]) =>
      val deleted = node.deleteAt(List(node.value))
      deleted == Some(None)

  test("deleteAt with mainline == deleteLastMainlineNode"):
    forAll: (node: Node[Int]) =>
      node.size >= 2 ==> {
        val deleted = node.deleteAt(node.mainlineValues)
        deleted.isDefined && deleted.get.get.size == node.size - 1
      }

  test("deleteAt and findAt are consistent"):
    forAll: (node: Node[Int]) =>
      val path = node.randomPath
      node.deleteAt(path).isDefined == node.find(path).isDefined

  given Monoid[Long] with
    def empty                     = 0L
    def combine(x: Long, y: Long) = x + y

  extension [A](node: Node[A])

    // generate a path from the root to a random node
    def randomPath: List[A] =
      if Random.nextBoolean() then node.value :: node.child.fold(List.empty[A])(_.randomPath)
      else if node.variations.size <= 1 then Nil
      else if Random.nextBoolean() then
        val v = node.variations(Random.nextInt(node.variations.size - 1))
        v.value :: v.child.fold(List.empty[A])(_.randomPath)
      else Nil

    def variationsCount: Long =
      node.child.foldLeft(node.variations.foldMap(_.size))((acc, v) => acc + v.variationsCount)
