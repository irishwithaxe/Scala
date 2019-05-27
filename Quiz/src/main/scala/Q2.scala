object Q2 {
  /*
    Q2. Starting from the previous exercise, use a trait to modify the behavior of 'put' so it reverses each string before adding it to the queue. Include tests.
   */

  trait StringQueue {
    def put(str: String): Boolean
    def get: Option[String]
  }

  trait ReverseString extends ArrayStringQueue {
    abstract override def put(str: String): Boolean = {
      val rStr = str.reverse
      super.put(rStr)
    }
  }

  final case class Pointer(arrayLength: Int, private val _v: Int = 0) {
    val value: Int = _v
    def next: Pointer =
      if (value == arrayLength - 1) Pointer(arrayLength)
      else Pointer(arrayLength, value + 1)

    def valueBefore(stepsBack: Int): Int = {
      val diff = value - stepsBack
      if (diff < 0) arrayLength + diff else diff
    }
  }

  class ArrayStringQueue(private var _queueSize: Int = 42)
    extends StringQueue {

    private var _array = new Array[String](_length = _queueSize)
    private var _nextFreePlace = Pointer(_queueSize)
    private var _dataLength = 0

    def isFull: Boolean = _array.length == _dataLength
    def isEmpty: Boolean = _dataLength == 0

    override def put(str: String): Boolean = {
      if (isFull) false
      else {
        _array(_nextFreePlace.value) = str
        _nextFreePlace = _nextFreePlace.next
        _dataLength += 1

        true
      }
    }

    override def get: Option[String] = {
      if (isEmpty) None
      else {
        val dataPosition = _nextFreePlace.valueBefore(_dataLength)
        val returnValue = _array(dataPosition)

        _array(dataPosition) = null
        _dataLength -= 1

        Some(returnValue)
      }
    }
  }

  case class ReverseIntQueue(arrayLength:Int = 42) extends ArrayStringQueue(arrayLength) with ReverseString
}
