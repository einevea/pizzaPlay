package models

import scala.annotation.tailrec

/**
 * Created by einevea on 27/01/15.
 */
abstract class Item(id:Long, name:String, description:String) {
  def getPrice():Price
}

case class Simple(id:Long, name:String, description:String,  price:Price) extends Item(id, name, description){
  override def getPrice(): Price = price
}

case class Complex(id:Long, name:String, description:String, properties:List[Property]) extends Item(id, name, description){
  lazy val price:Price = calcPrice()

  private def calcPrice():Price = {
    sum(for (property <- properties) yield property.price)
  }

  def sum(xs: List[Price]): Price = {
    @tailrec
    def inner(xs: List[Price], accum: Price): Price = {
      xs match {
        case x :: tail => inner(tail, accum + x)
        case Nil => accum
      }
    }
    inner(xs, Price.zero)
  }

  override def getPrice(): Price = price
}

object Item{
  def apply(id:Long, name:String, description:String,  price:Price) = {new Simple(id, name, description, price)}
  def apply(id:Long, name:String, description:String,  properties:List[Property]) = {new Complex(id, name, description, properties)}
}