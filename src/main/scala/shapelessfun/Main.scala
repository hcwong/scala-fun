package shapelessfun

import shapeless._
import shapeless.labelled.FieldType

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
        // Using Lazy[] prevents divergence at compilation by deferring
        // evaluation to runtime and allowing self-referential implicits
        hEncoder: Lazy[
          CsvEncoder[H]
        ],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] = instance { case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

    implicit def genericEncoder[A, R](implicit
        gen: Lazy[Generic.Aux[A, R]],
        enc: CsvEncoder[R]
        // gen.to(a) converts the generic to a hList.
        // Resolution is done by our hList encoders above
    ): CsvEncoder[A] = instance(a => enc.encode(gen.value.to(a)))

    implicit val cNilEncoder: CsvEncoder[CNil] =
      instance(cnil => sys.error("Not possible"))

    implicit def coproductEncoder[H, T <: Coproduct](implicit
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = instance {
      // disjunction of types, hence the match statement
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }
  }

  // When all params to implicit def are implicit, compiler can use
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

  object DependentFunctions {
    // Type parameters are useful as inputs, type members are useful as outputs
    // The result type of getRepr depends on its value parameters via the type members
    // The output of this function depends on the type member of the generic passed in
    def getRepr[A](value: A)(implicit gen: Generic[A]) =
      gen.to(value)

    // Why use type members?
    /**  Else we will see this scenario getRepr2[A, R] - but the whole point is to output R, passing it as type parameter is moot
      */

    trait Second[L <: HList] {
      type Out
      def apply(l: L): Out
    }

    object Second {
      type Aux[L <: HList, O] = Second[L] { type Out = O }
      // Convenient wrapper to avoid having to do Second[L] {type Out = O} like Generic.Aux
      // If we put return type as Second[L] in apply, Out member type will be erased
      // This also prevents the compiler from using the member type for further implicit resolution
      def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
    }
    // We should extract every intermediate type out to a type parameter.
    // Many type parameters won’t be used in the result, but the compiler needs them to know which types it has to unify.
    import Second._
    implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
      new Second[A :: B :: Rest] {
        type Out = B
        def apply(value: A :: B :: Rest): B = value.tail.head
      }
  }

  object LabelledGenerics {
    // Records are HList of tagged elements
    sealed trait JsonValue
    case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
    case class JsonArray(items: List[JsonValue]) extends JsonValue
    case class JsonString(value: String) extends JsonValue
    case class JsonNumber(value: Double) extends JsonValue
    case class JsonBoolean(value: Boolean) extends JsonValue
    case object JsonNull extends JsonValue

    trait JsonEncoder[A] {
      def encode(value: A): JsonValue
    }

    object JsonEncoder {
      // Only define a summoner for JSON Encoder, this is the interface we're exporting
      def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
    }

    def instance[A](func: A => JsonValue): JsonEncoder[A] =
      new JsonEncoder[A] {
        def encode(value: A) = func(value)
      }

    implicit val stringEncoder: JsonEncoder[String] =
      instance(str => JsonString(str))

    implicit val doubleEncoder: JsonEncoder[Double] =
      instance(num => JsonNumber(num))

    implicit val intEncoder: JsonEncoder[Int] =
      instance(num => JsonNumber(num))

    implicit val booleanEncoder: JsonEncoder[Boolean] =
      instance(bool => JsonBoolean(bool))

    implicit def listEncoder[A](implicit
        enc: JsonEncoder[A]
    ): JsonEncoder[List[A]] =
      instance(list => JsonArray(list.map(enc.encode)))

    implicit def optionEncoder[A](implicit
        enc: JsonEncoder[A]
    ): JsonEncoder[Option[A]] =
      instance(opt => opt.map(enc.encode).getOrElse(JsonNull))

    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(value: A): JsonObject
    }

    def objectInstance[A](func: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        def encode(value: A): JsonObject = func(value)
      }

    implicit def hNilEncoder: JsonObjectEncoder[HNil] =
      objectInstance(hnil => JsonObject(Nil))

    implicit def hListObjectEncoder[K <: Symbol, H, T <: HList](implicit
        witness: Witness.Aux[K],
        hEncoder: Lazy[JsonEncoder[H]],
        tEncoder: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName: String = witness.value.name
      objectInstance { hlist =>
        val head = hEncoder.value.encode(hlist.head)
        // Recursive to get the tail
        // Note that when tail is exhausted, it defaults to JsonObject(Nil) which appends nothing to the list
        val tail = tEncoder.encode(hlist.tail)
        JsonObject((fieldName, head) :: tail.fields)
      }
    }

    implicit def genericObjectEncoder[A, H](implicit
        generic: LabelledGeneric.Aux[A, H],
        hEncoder: Lazy[JsonObjectEncoder[H]]
    ): JsonEncoder[A] = objectInstance { value =>
      hEncoder.value.encode(generic.to(value))
    }
  }

  object Extractor {
    trait KeyExtractor[A] {
      def extract(value: A, fieldNames: Seq[String]): Option[Any]
    }

    object KeyExtractor {
      // Summoner method
      def apply[A](implicit keyExtractor: KeyExtractor[A]) = keyExtractor

      // Constructor method
      def instance[A](func: (A, Seq[String]) => Option[Any]): KeyExtractor[A] =
        new KeyExtractor[A] {
          def extract(value: A, fieldNames: Seq[String]): Option[Any] =
            func(value, fieldNames)
        }

      implicit def coProductEncoder[H, T <: Coproduct](implicit
          headEncoder: Lazy[KeyExtractor[H]],
          tailEncoder: KeyExtractor[T]
      ): KeyExtractor[H :+: T] = instance { (value, fieldNames) =>
        value match {
          case Inl(head) => headEncoder.value.extract(head, fieldNames)
          case Inr(tail) => tailEncoder.extract(tail, fieldNames)
        }
      }

      implicit def hNilExtractor: KeyExtractor[HNil] = instance {
        (value: HNil, fieldNames: Seq[String]) => None
      }

      implicit def instantiateOptionalKeyExtractor[A](implicit
          keyExtractor: Lazy[KeyExtractor[A]]
      ): Option[Lazy[KeyExtractor[A]]] =
        Some(keyExtractor)

      implicit def instantiateOptionalGeneric[A, H](implicit
          generic: LabelledGeneric.Aux[A, H]
      ): Option[LabelledGeneric.Aux[A, H]] =
        Some(generic)

      implicit def hListExtractorForLeafValues[
          K <: Symbol,
          H,
          HRepr <: HList,
          T <: HList
      ](implicit
          witness: Witness.Aux[K],
          tailExtractor: KeyExtractor[T],
          ev: Option[H <:< HList] = None,
          // Maybe we add Option Generic and Option headExtractor here
          // and then if generic and ev is defined, treat this as a case class, using get field name
          headExtractor: Option[Lazy[KeyExtractor[HRepr]]] = None,
          generic: Option[LabelledGeneric.Aux[H, HRepr]] = None
      ): KeyExtractor[FieldType[K, H] :: T] = {
        val fieldName = witness.value.name
        instance { (hList: FieldType[K, H] :: T, fieldNames: Seq[String]) =>
          {
            fieldNames match {
              case Seq(headFieldName)
                  if headExtractor.isEmpty && generic.isEmpty && headFieldName == fieldName =>
                Some(hList.head)
              case Seq(headFieldName, rest @ _*)
                  if headExtractor.isDefined && generic.isDefined && headFieldName == fieldName =>
                headExtractor.get.value
                  .extract(generic.get.to(getFieldValue(hList.head)), rest)
              case seq if seq.isEmpty =>
                sys.error("Received empty fieldnames list")
              case Seq(headFieldName, rest @ _*)
                  if headFieldName != fieldName =>
                tailExtractor.extract(hList.tail, fieldNames)
              case _ =>
                sys.error(
                  s"Unrecognised value: ${hList.toString}, ${fieldNames}, ${headExtractor.toString} ${generic.toString}"
                )
            }
          }
        }
      }

      // We need this to unpack the value of nested case classes
      def getFieldValue[K, V](value: FieldType[K, V]): V = value

      /**      implicit def nestedHListExtractor[
        *        K <: Symbol,
        *        H,
        *        HRepr <: HList,
        *        T <: HList
        *    ](implicit
        *        witness: Witness.Aux[K],
        *        generic: LabelledGeneric.Aux[H, HRepr],
        *        headExtractor: Lazy[KeyExtractor[HRepr]],
        *        tailExtractor: KeyExtractor[T]
        *    ): KeyExtractor[FieldType[K, H] :: T] = {
        *      val fieldName = witness.value.name
        *      instance { (hList: FieldType[K, H] :: T, fieldNames: Seq[String]) =>
        *        {
        *          fieldNames match {
        *            case Seq(headFieldName, rest @ _*) if headFieldName == fieldName =>
        *              headExtractor.value.extract(generic.to(getFieldValue(hList.head)), rest)
        *            case seq if seq.isEmpty =>
        *              sys.error("Received empty fieldnames list")
        *            case _ => tailExtractor.extract(hList.tail, fieldNames)
        *          }
        *        }
        *      }
        *    }
        */

      /**      implicit def hListExtractor[K <: Symbol, H, T <: HList](implicit
        *        witness: Witness.Aux[K],
        *        headExtractor: Lazy[KeyExtractor[H]],
        *        tailExtractor: KeyExtractor[T],
        *        ev: Option[H <:< HList] = None
        *    ): KeyExtractor[FieldType[K, H] :: T] = {
        *      val fieldName = witness.value.name
        *      instance { (hList: FieldType[K, H] :: T, fieldNames: Seq[String]) =>
        *        {
        *          fieldNames match {
        *            case Seq(headFieldName)
        *                if ev.isEmpty && headFieldName == fieldName =>
        *              Some(hList.head)
        *            case Seq(headFieldName)
        *                if ev.isDefined && headFieldName == fieldName =>
        *              sys.error("Last field is still nested")
        *            case Seq(headFieldName, rest @ _*)
        *                if ev.isDefined && headFieldName == fieldName =>
        *              headExtractor.value.extract(hList.head, rest)
        *            case Seq(headFieldName, rest @ _*)
        *                if ev.isEmpty && headFieldName == fieldName =>
        *              sys.error(
        *                "Reached tail field but still have field names to iterate through"
        *              )
        *            case seq if seq.isEmpty =>
        *              sys.error("Received empty fieldnames list")
        *            case _ => tailExtractor.extract(hList.tail, fieldNames)
        *          }
        *        }
        *      }
        *    }
        */

      // Unfortunately, generics only work one layer at a time - this only handles the first level
      implicit def genericObjectExtractor[A, B](implicit
          generic: LabelledGeneric.Aux[A, B],
          extractor: Lazy[KeyExtractor[B]]
      ): KeyExtractor[A] = instance { (value: A, fieldNames: Seq[String]) =>
        extractor.value.extract(generic.to(value), fieldNames)
      }
    }
  }

  def main(array: Array[String]): Unit = {
    import CsvEncoder._
    import Extractor._
    import LabelledGenerics.JsonEncoder

    implicit val intEncoder: CsvEncoder[Int] =
      instance(num => List(num.toString))
    implicit val strEncoder: CsvEncoder[String] = instance(s => List(s))

    case class Shiba(
        name: String,
        age: Int
    )
    case class Human(
        name: String,
        dog: Shiba
    )
    val ranmaru = Shiba("Ranmaru", 10)
    val owner = Human("Owner", ranmaru)

    // import scala.reflect.runtime.universe.reify allows us to debug implicit resolution

    /** writeCsv(dogs)(
      *    genericEncoder(
      *      Generic[Dog], // it seems like shapeless implicitly resolves the Generic
      *      hListEncoder(StringEncoder,
      *        hListEncoder(intEncoder, hnilEncoder))))
      */
    println(writeCsv(List[Dog](Dog("Ranmaru", 10))))
    println(writeCsv(List[Animal](Penguin("steve", "adelie"))))
    println(JsonEncoder[Dog].encode(Dog("Ranmaru", 10)))
    // LabelledGeneric[Human].to(human).tail.head
    println(KeyExtractor[Human].extract(owner, Seq("name")))
    // KeyExtractor[Dog].extract(human, Seq("age"))
  }
}
