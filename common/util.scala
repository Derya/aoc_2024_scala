import scala.collection.immutable.ArraySeq

implicit class SeqOps[A](x: Seq[A]) {
  def zippedWithPriorElements: Seq[(A, Seq[A])] = x.zip(x.inits.toSeq.reverse)
}

class Cacher[A, B] {
  import scala.collection.mutable

  private val cache: mutable.Map[A, B] = mutable.Map.empty

  def getOrElseUpdate(key: A, f: => B): B =
    cache.getOrElseUpdate(key, f)
}

object Cacher {
  def apply[A, B] = new Cacher[A, B]()
}

implicit class Crossable[A](x: Iterable[A]) {
  def cross[B](other: Iterable[B]): Iterable[(A, B)] = x.flatMap(a => other.map(b => (a, b)))
}

class Matrix2D[A](matrix: ArraySeq[ArraySeq[A]]) {
  def map[B](f: A => B): Matrix2D[B] =
    Matrix2D(matrix.map(_.map(f)))

  def updated(coord: Coord, value: A): Matrix2D[A] =
    Matrix2D(matrix.updated(coord.x, matrix(coord.x).updated(coord.y, value)))

  def coordAt(coord: Coord): Option[A] =
    if (coord.x < 0 || coord.x >= matrix.length || coord.y < 0 || coord.y >= matrix(coord.x).length) {
      None
    } else {
      Some(matrix(coord.x)(coord.y))
    }

  lazy val indices: IndexedSeq[Coord] =
    matrix
      .indices
      .flatMap(x => matrix(x).indices.map(y => Coord(x, y)))

  lazy val indexedValues: IndexedSeq[(Coord, A)] =
    matrix
      .indices
      .flatMap(x => matrix(x).indices.map(y => Coord(x, y)))
      .map(coord => (coord, matrix(coord.x)(coord.y)))
}

object Matrix2D {

  def apply[A](matrix: ArraySeq[ArraySeq[A]]): Matrix2D[A] =
    new Matrix2D(matrix)

  def fromLines(lines: IterableOnce[String]): Matrix2D[Char] =
    new Matrix2D[Char](lines.iterator.map(_.to(ArraySeq)).to(ArraySeq))

  def fromString(text: String): Matrix2D[Char] =
    fromLines(text.linesIterator)

  import scala.collection.generic.IsIterableOnce
  import scala.collection.Factory

  def from[C, CC](data: C)(using
    outerIsIterableOnce: IsIterableOnce[C] {type A = CC},
    innerIsIterableOnce: IsIterableOnce[CC],
    outerFactory: Factory[ArraySeq[innerIsIterableOnce.A], ArraySeq[ArraySeq[innerIsIterableOnce.A]]],
    innerFactory: Factory[innerIsIterableOnce.A, ArraySeq[innerIsIterableOnce.A]]
  ): Matrix2D[innerIsIterableOnce.A] = {
    val asArraySeq: ArraySeq[ArraySeq[innerIsIterableOnce.A]] =
      outerFactory.fromSpecific(outerIsIterableOnce(data).iterator.map(cc => innerFactory.fromSpecific(innerIsIterableOnce(cc))))

    new Matrix2D[innerIsIterableOnce.A](asArraySeq)
  }
}

case class Coord(x: Int, y: Int) {
  def +(other: Coord): Coord = Coord(x + other.x, y + other.y)

  def -(other: Coord): Coord = Coord(x - other.x, y - other.y)

  def *(scalar: Int): Coord = Coord(x * scalar, y * scalar)

  def shrinkRetainSlope(): Coord = {
    val gcd = BigInt(x).gcd(BigInt(y)).toInt
    Coord(x / gcd, y / gcd)
  }

  lazy val adjacents: List[Coord] = List(
    Coord(x + 1, y),
    Coord(x - 1, y),
    Coord(x, y + 1),
    Coord(x, y - 1)
  )

  lazy val diagonals: List[Coord] = List(
    Coord(x + 1, y + 1),
    Coord(x - 1, y - 1),
    Coord(x + 1, y - 1),
    Coord(x - 1, y + 1)
  )

  lazy val adjacentsAndDiagonals = adjacents ++ diagonals
}

//implicit class IterableOnceOps[A](x: IterableOnce[A]) {
//  def sumBig[B >: A](implicit num: Numeric[B]): B =
//    if (isEmpty) {
//      num.zero
//    } else {
//      x.iterator.foldLeft(BigInt(0))((a, b) => a + num.toBigInt(b))
//    }
//}

implicit class ArrayOps[A](x: Array[A]) {
  def findWithIdx(p: A => Boolean): Option[(A, Int)] = {
    x.indices.find(i => p(x(i))).map(foundIdx => (x(foundIdx), foundIdx))
  }

  import scala.reflect.ClassTag

  def mapWithIndex[B](f: (A, Int) => B)(implicit ct: ClassTag[B]): Array[B] = {
    val len = x.length
    val ys = new Array[B](len)
    if (len > 0) {
      var i = 0
      (x: Any @unchecked) match {
        case x: Array[AnyRef] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Int] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Double] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Long] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Float] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Char] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Byte] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Short] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
        case x: Array[Boolean] => while (i < len) {
          ys(i) = f(x(i).asInstanceOf[A], i);
          i = i + 1
        }
      }
    }
    ys
  }
}
