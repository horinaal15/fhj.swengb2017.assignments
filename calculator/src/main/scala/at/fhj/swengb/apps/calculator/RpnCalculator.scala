package at.fhj.swengb.apps.calculator

import java.util.NoSuchElementException

import scala.util.Try

/**
  * Companion object for our reverse polish notation calculator.
  */
object RpnCalculator {

  /**
    * Returns empty RpnCalculator if string is empty, otherwise pushes all operations
    * on the stack of the empty RpnCalculator.
    *
    * @param s a string representing a calculation, for example '1 2 +'
    * @return
    */
  def apply(s: String): Try[RpnCalculator] = if (s.isEmpty) Try(RpnCalculator()) else {
    val stack: List[Op] = s.split(' ').map(e => Op(e)).toList
    stack.foldLeft(Try(RpnCalculator()))((acc, elem) => acc.get.push(elem))
  }
}

/**
  * Reverse Polish Notation Calculator.
  *
  * @param stack a datastructure holding all operations
  */
case class RpnCalculator(stack: List[Op] = Nil) {

  /**
    * By pushing Op on the stack, the Op is potentially executed. If it is a Val, it the op instance is just put on the
    * stack, if not then the stack is examined and the correct operation is performed.
    *
    * @param op
    * @return
    */
  def push(op: Op): Try[RpnCalculator] = {

    op match {
      case a: Val => Try(RpnCalculator(stack :+ a))
      case b: BinOp =>
        try {
          def getVal (rcal: RpnCalculator): Val = {
            var myVal = rcal.peek()
            myVal match {
              case a: Val   => a
              case _: BinOp => throw new NoSuchElementException
            }
          }
          var first = getVal(this)
          var rCalc = pop()._2

          var second = getVal(rCalc)
          rCalc = rCalc.pop()._2

          var result: Val = b.eval(first, second)

          rCalc.push(result)
        } catch {
          case e: Exception => Try[RpnCalculator](throw e)
        }
    }
  }

  /**
    * Pushes val's on the stack.
    *
    * If op is not a val, pop two numbers from the stack and apply the operation.
    *
    * @param op
    * @return
    */
  def push(op: Seq[Op]): Try[RpnCalculator] =
    op.foldLeft(Try(RpnCalculator()))((acc, elem) => acc.get.push(elem))

  /**
    * Returns an tuple of Op and a RevPolCal instance with the remainder of the stack.
    *
    * @return
    */
  def pop(): (Op, RpnCalculator) = (stack.head, RpnCalculator(stack.tail))

  /**
    * If stack is nonempty, returns the top of the stack. If it is empty, this function throws a NoSuchElementException.
    *
    * @return
    */
  def peek(): Op = {
    if (stack.isEmpty)
      throw new NoSuchElementException
    else
      stack.head
  }

  /**
    * returns the size of the stack.
    *
    * @return
    */
  def size: Int = stack.size
}