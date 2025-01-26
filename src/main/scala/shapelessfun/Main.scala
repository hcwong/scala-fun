package shapelessfun

import shapeless._

object Main {

  case class Nested(bar: String)
  case class Foo(test: String, maybeNested: Option[Nested], nested: Nested)
  sealed trait Animal
  case class Dog(name: String, age: Int) extends Animal
  case class Penguin(name: String, species: String) extends Animal

  // Type Astronaut Guide to Shapeless
  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {
    // Summoner - equivalent to implicitly[]
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    // Constructor
    def instance[A](func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        def encode(value: A) = func(value)
      }

    implicit val hNilEncoder: CsvEncoder[HNil] = instance(hnil => Nil)

    implicit def hListEncoder[H, T <: HList](implicit
        hEncoder: CsvEncoder[H],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] = instance { case h :: t =>
      hEncoder.encode(h) ++ tEncoder.encode(t)
    }

    implicit def genericEncoder[A, R](implicit
        gen: Generic.Aux[A, R],
        enc: CsvEncoder[R]
        // gen.to(a) converts the generic to a hList.
        // Resolution is done by our hList encoders above
    ): CsvEncoder[A] = instance(a => enc.encode(gen.to(a)))

    implicit val cNilEncoder: CsvEncoder[CNil] =
      instance(cnil => sys.error("Not possible"))

    implicit def coproductEncoder[H, T <: Coproduct](implicit
        hEncoder: CsvEncoder[H],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = instance {
      // disjunction of types, hence the match statement
      case Inl(h) => hEncoder.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }
  }

  // When all params ot implicit def are implicit, compiler can use
  // it as a resolution rule to create instance from other instances
  // This is also known as implicit resolution
  implicit def pairEncoder[A, B](implicit
      aEncoder: CsvEncoder[A],
      bEncoder: CsvEncoder[B]
  ): CsvEncoder[(A, B)] =
    new CsvEncoder[(A, B)] {
      def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  def main(array: Array[String]): Unit = {
    import CsvEncoder._

    implicit val intEncoder: CsvEncoder[Int] =
      instance(num => List(num.toString))
    implicit val strEncoder: CsvEncoder[String] = instance(s => List(s))

    /** writeCsv(dogs)(
      *    genericEncoder(
      *      Generic[Dog], // it seems like shapeless implicitly resolves the Generic
      *      hListEncoder(StringEncoder,
      *        hListEncoder(intEncoder, hnilEncoder))))
      */
    println(writeCsv(List[Dog](Dog("Ranmaru", 10))))
    println(writeCsv(List[Animal](Penguin("steve", "adelie"))))
  }
}
