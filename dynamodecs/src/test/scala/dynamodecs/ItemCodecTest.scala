package dynamodecs

import io.adamnfish.dynamodecs.ItemCodec
import io.adamnfish.dynamodecs.UnwrappedAttributeCodec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.circe._
import io.circe.generic.semiauto._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue


class ItemCodecTest extends AnyFreeSpec with Matchers {
  "round-trips" - {
    "a simple codec works" in {
      case class Foo(a: String, b: Boolean, c: Int) derives ItemCodec

      val foo = Foo("hello", true, 42)
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

      decodedFoo shouldEqual foo
    }

    "a codec with a collection works" in {
      case class Foo(a: String, ns: List[Int]) derives ItemCodec

      val foo = Foo("hello", List(42, 7))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

      decodedFoo shouldEqual foo
    }

    "a datastructure with an optional value" - {
      "works when it is present" in {
        case class Foo(a: String, maybeB: Option[Int]) derives ItemCodec

        val foo = Foo("hello", Some(42))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

        decodedFoo shouldEqual foo
      }

      "works when it is absent" in {
        case class Foo(a: String, maybeB: Option[Int]) derives ItemCodec

        val foo = Foo("hello", None)
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

        decodedFoo shouldEqual foo
      }
    }

    "a datastructure with a Map" - {
      "works with String keys" in {
        case class Foo(a: String, map: Map[String, Int]) derives ItemCodec

        val foo = Foo("hello", Map("a" -> 1, "b" -> 2))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

        decodedFoo shouldEqual foo
      }

      "works with non-string keys that have a MapKeyCodec instance" in {
        case class Foo(a: String, map: Map[Int, Int]) derives ItemCodec

        val foo = Foo("hello", Map(1 -> 1, 2 -> 2))
        val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
        val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

        decodedFoo shouldEqual foo
      }
    }

    "JSON serialises a case class field" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given Codec[Bar] = deriveCodec

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

      decodedFoo shouldEqual foo
    }

    "JSON serialises a multiply nested case class field" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String, baz: Baz)
      case class Baz(c: Boolean)
      given Codec[Baz] = deriveCodec
      given Codec[Bar] = deriveCodec

      val foo = Foo(42, Bar("hello", Baz(true)))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

      decodedFoo shouldEqual foo
    }

    "a datastructure containing wrapped fields" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given UnwrappedAttributeCodec[Bar] = UnwrappedAttributeCodec.deriveUnwrapper

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)
      val decodedFoo = summon[ItemCodec[Foo]].decode(encodedFoo)

      decodedFoo shouldEqual foo
    }
  }

  "value class fields" - {
    "are unwrapped by providing an UnwrappedAttributeCodec instance" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given UnwrappedAttributeCodec[Bar] = UnwrappedAttributeCodec.deriveUnwrapper

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)

      encodedFoo.get("bar") shouldEqual Some(AttributeValue.fromS("hello"))
    }

    "are normally wrapped with a provided JSON codec" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given Codec[Bar] = deriveCodec

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)

      encodedFoo.get("bar") shouldEqual Some(AttributeValue.fromS("""{"b":"hello"}"""))
    }

    "are unwrapped if both a JSON Codec and Unwrapper instance are present" in {
      case class Foo(a: Int, bar: Bar) derives ItemCodec
      case class Bar(b: String)
      given Codec[Bar] = deriveCodec
      given UnwrappedAttributeCodec[Bar] = UnwrappedAttributeCodec.deriveUnwrapper

      val foo = Foo(42, Bar("hello"))
      val encodedFoo = summon[ItemCodec[Foo]].encode(foo)

      encodedFoo.get("bar") shouldEqual Some(AttributeValue.fromS("hello"))
    }
  }

  "Can manually summon a codec" in {
    case class Bar(a: Int)
    val bar = Bar(42)
    val barItemCodec = summon[ItemCodec[Bar]]
    barItemCodec.decode(barItemCodec.encode(bar)) shouldEqual bar
  }

  "Can manually create a codec" ignore {
    // TODO: Once error handling is in place to save having to rewrite this
  }

  "deriving an ItemCodec does not compile if there is no JSON codec available for the nested type" in {
    case class Foo(a: Int, bar: Bar)
    case class Bar(b: String)

    "summon[ItemCodec[Foo]]" shouldNot compile
  }

  "syntax" - {
    "asDbItem works the same way as manually encoding" in {
      import io.adamnfish.dynamodecs.ItemCodec.asDbItem
      case class Foo(s: String, n: Int)
      val foo = Foo("hello", 42)
      foo.asDbItem shouldEqual summon[ItemCodec[Foo]].encode(foo)
    }

    "fromDbItem works the same way as manually decoding" in {
      import io.adamnfish.dynamodecs.ItemCodec.fromDbItem
      case class Foo(s: String, n: Int)
      val foo = Foo("hello", 42)
      val encodedItem = summon[ItemCodec[Foo]].encode(foo)
      encodedItem.fromDbItem[Foo] shouldEqual summon[ItemCodec[Foo]].decode(encodedItem)
    }
  }
}
