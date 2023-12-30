package io.adamnfish.dynamodecs

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.util.UUID
import scala.compiletime.{erasedValue, summonInline}
import scala.jdk.CollectionConverters.*


/**
 * Codec for a single field in a case class.
 * 
 * This converts the field into an AttributeValues, for us in an item db entry `Map[String, AttributeValue]`.
 */
trait AttributeCodec[A]:
  def encode(a: A): AttributeValue
  def decode(av: AttributeValue): A

object AttributeCodec:
  def apply[A](using AttributeCodec[A]): AttributeCodec[A] =
    summon[AttributeCodec[A]]

  extension [A](avc: AttributeCodec[A])
    def imap[B](f: A => B)(g: B => A): AttributeCodec[B] =
      new AttributeCodec[B]:
        def encode(b: B): AttributeValue = avc.encode(g(b))
        def decode(av: AttributeValue): B = f(avc.decode(av))

  def instance[A](encodeValue: A => AttributeValue)(decodeValue: AttributeValue => A): AttributeCodec[A] =
    new AttributeCodec[A]:
      def encode(a: A): AttributeValue = encodeValue(a)
      def decode(av: AttributeValue): A = decodeValue(av)

  given AttributeCodec[String] with
    def encode(a: String): AttributeValue = AttributeValue.builder().s(a).build()
    def decode(av: AttributeValue): String = av.s()

  given AttributeCodec[Int] with
    def encode(a: Int): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): Int = av.n().toInt

  given AttributeCodec[Long] with
    def encode(a: Long): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): Long = av.n().toLong

  given AttributeCodec[Double] with
    def encode(a: Double): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): Double = av.n().toDouble

  given AttributeCodec[Boolean] with
    def encode(a: Boolean): AttributeValue = AttributeValue.builder().bool(a).build()
    def decode(av: AttributeValue): Boolean = av.bool()

  given AttributeCodec[BigDecimal] with
    def encode(a: BigDecimal): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): BigDecimal = BigDecimal(av.n())

  given AttributeCodec[BigInt] with
    def encode(a: BigInt): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): BigInt = BigInt(av.n())

  given AttributeCodec[Short] with
    def encode(a: Short): AttributeValue = AttributeValue.builder().n(a.toString).build()
    def decode(av: AttributeValue): Short = av.n().toShort

  given AttributeCodec[UUID] with
    def encode(a: UUID): AttributeValue = AttributeValue.builder().s(a.toString).build()
    def decode(av: AttributeValue): UUID = UUID.fromString(av.s())

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Option[A]] with
    def encode(a: Option[A]): AttributeValue = a.map(codec.encode).getOrElse(AttributeValue.builder().nul(true).build())
    def decode(av: AttributeValue): Option[A] = if av.nul() then None else Some(codec.decode(av))

  given [A](using codec: AttributeCodec[A]): AttributeCodec[List[A]] with
    def encode(a: List[A]): AttributeValue = AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): List[A] = av.l().asScala.toList.map(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Seq[A]] with
    def encode(a: Seq[A]): AttributeValue = AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): Seq[A] = av.l().asScala.toSeq.map(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Iterable[A]] with
    def encode(a: Iterable[A]): AttributeValue = AttributeValue.builder().l(a.toList.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): Iterable[A] = av.l().asScala.map(codec.decode)

  given [A](using codec: AttributeCodec[A]): AttributeCodec[Set[A]] with
    def encode(a: Set[A]): AttributeValue = AttributeValue.builder().l(a.map(codec.encode).asJava).build()
    def decode(av: AttributeValue): Set[A] = av.l().asScala.toList.map(codec.decode).toSet

  given [V](using valueCodec: AttributeCodec[V]): AttributeCodec[Map[String, V]] =
    new AttributeCodec[Map[String, V]]:
      override def encode(a: Map[String, V]): AttributeValue =
        val entries =
          a.map { case (k, v) => k -> valueCodec.encode(v) }
        AttributeValue.builder().m(entries.asJava).build()

      override def decode(av: AttributeValue): Map[String, V] =
        av.m().asScala.toMap.map { case (k, v) =>
          k -> valueCodec.decode(v)
        }

  given [K, V](using keyCodec: MapKeyCodec[K], valueCodec: AttributeCodec[V]): AttributeCodec[Map[K, V]] =
    new AttributeCodec[Map[K, V]]:
      override def encode(a: Map[K, V]): AttributeValue =
        val entries =
          a.map { case (k, v) =>
            keyCodec.encodeKey(k) -> valueCodec.encode(v)
          }
        AttributeValue.builder().m(entries.asJava).build()

      override def decode(av: AttributeValue): Map[K, V] =
        av.m().asScala.toMap.map { case (k, v) =>
          keyCodec.decodeKey(k) -> valueCodec.decode(v)
        }

  inline given derived[A : io.circe.Codec]: AttributeCodec[A] =
    val jsonCodec = summon[io.circe.Codec[A]]

    new AttributeCodec[A]:
      override def encode(a: A): AttributeValue =
        val json = jsonCodec(a)
        AttributeCodec[String].encode(json.noSpaces)

      override def decode(av: AttributeValue): A =
        val rawJson = AttributeCodec[String].decode(av)
        val result = for
          json <- io.circe.parser.parse(rawJson)
          a <- json.as[A](jsonCodec)
        yield a
        // TODO: refactor error handling into the codec interface
        result.toTry.get
