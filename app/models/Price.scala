package models

import java.util.Currency


/**
 * Created by einevea on 27/01/15.
 */
trait Price {
  def value:BigDecimal
  def currency:Option[Currency]

  def +(elem: Price):Price = {
    if(value.equals(BigDecimal(0))){
      elem
    }else if (elem.value.equals(BigDecimal(0))) {
      this
    }else if(currency.equals(elem.currency)){
      Price((elem.value + value), currency)
    }else {
      throw new IllegalArgumentException("currency mismatch")
    }
  }
}

case class Value(value:BigDecimal, currency:Option[Currency]) extends Price

case class Zero() extends Price{
  val value = BigDecimal(0)
  val currency = None
}

private object Value {

}

private object Zero{

}

object Price {
  def apply(value:BigDecimal, currency:Option[Currency]) = {Value(value, currency)}
  def apply(value:BigDecimal, currency:Currency) = {Value(value, Some(currency))}
  def zero: Price = Zero()
}
