package io.adamnfish.dynamodecs


trait MapKeyCodec[A]:
  def encodeKey(a: A): String
  def decodeKey(str: String): A

object MapKeyCodec:
  def apply[A: MapKeyCodec]: MapKeyCodec[A] =
    summon[MapKeyCodec[A]]

  def instance[A](encode: A => String)(decode: String => A): MapKeyCodec[A] =
    new MapKeyCodec[A] {
      def encodeKey(a: A): String = encode(a)
      def decodeKey(str: String): A = decode(str)
    }

  given MapKeyCodec[String] with
    def encodeKey(a: String): String = a
    def decodeKey(str: String): String = str

  given MapKeyCodec[Int] with
    def encodeKey(a: Int): String = a.toString
    def decodeKey(str: String): Int = str.toInt

  given MapKeyCodec[Long] with
    def encodeKey(a: Long): String = a.toString
    def decodeKey(str: String): Long = str.toLong

  given MapKeyCodec[Short] with
    def encodeKey(a: Short): String = a.toString
    def decodeKey(str: String): Short = str.toShort

  given MapKeyCodec[Char] with
    def encodeKey(a: Char): String = a.toString
    def decodeKey(str: String): Char = str.head

  given MapKeyCodec[Byte] with
    def encodeKey(a: Byte): String = a.toString
    def decodeKey(str: String): Byte = str.toByte

  given MapKeyCodec[Boolean] with
    def encodeKey(a: Boolean): String = a.toString
    def decodeKey(str: String): Boolean = str.toBoolean

  given MapKeyCodec[Float] with
    def encodeKey(a: Float): String = a.toString
    def decodeKey(str: String): Float = str.toFloat

  given MapKeyCodec[Double] with
    def encodeKey(a: Double): String = a.toString
    def decodeKey(str: String): Double = str.toDouble

  given MapKeyCodec[BigInt] with
    def encodeKey(a: BigInt): String = a.toString
    def decodeKey(str: String): BigInt = BigInt(str)
