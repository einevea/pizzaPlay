import java.util.Currency

import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import models._

/**
 * Created by einevea on 25/01/15.
 */
@RunWith(classOf[JUnitRunner])
class ItemSpec extends Specification{

  "Price" should {
    "sum only zero or same currencies" in {
      val eur = Currency.getInstance("EUR")
      val usd = Currency.getInstance("USD")

      val euros = Price(6, eur)
      val zeuros = Price(0, eur)
      val dollars = Price(6, usd)
      val zdollar = Price(0, usd)


      "you can always sum zeros" in {
        {Price.zero + zeuros + zdollar}.value mustEqual (BigDecimal(0))

        {zeuros + zdollar + Price.zero}.value mustEqual (BigDecimal(0))

        {zeuros + Price.zero + zdollar}.value mustEqual (BigDecimal(0))

        {zeuros + zdollar}.value mustEqual (BigDecimal(0))
      }

      "different currencies throw an error" in{
          euros + dollars must throwA(new IllegalArgumentException("currency mismatch"))
      }

      "same currencies add the values" in {
        euros + euros mustEqual(Price(12, eur))
      }
    }
  }

  "Item" should {
    "have a total price equal to the sum of the properties" in {

        "for none properties" in {
          val item = Item(0, "item", "this is an item", List())
          item.getPrice().value mustEqual (BigDecimal(0))
        }

        "for multiple properties" in {
          val euros = Currency.getInstance("EUR")
          val property = Property(0, "test", Price(0, euros))
          val property1 = Property(0, "test1", Price(1, euros))
          val property2 = Property(0, "test1", Price(2, euros))

          val properties = List(property, property1, property2)

          val item = Item(0, "item", "this is an item", properties)

          item.getPrice() mustEqual (Price(3, euros))
        }
    }
  }

}
