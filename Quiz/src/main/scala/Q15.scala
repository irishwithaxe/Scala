object Q15 {
  /*
Q15. Add a method "median" to a Seq of integers so that s.median is the median of s for s of type Seq[Int]
How can you add the same method for a sequence of doubles with minimal code duplication?
   */

  trait SequenceOperations[T] {
    def sortFunc(v1: T, v2: T): Boolean
    def median(v1: T, v2: T): T
  }

  implicit object IntSeqOp extends SequenceOperations[Int] {
    override def sortFunc(v1: Int, v2: Int): Boolean = v1 < v2
    override def median(v1: Int, v2: Int): Int = (v1 + v2) / 2
  }

  implicit class SequenceExtension[T](seq: Seq[T])(
      implicit op: SequenceOperations[T]) {

    def median: Option[T] = {
      if (seq.isEmpty) None else Some(medianForNonEmptySeq)
    }

    private def medianForNonEmptySeq: T = {
      val sorted = seq.sortWith(op.sortFunc)
      val middle = sorted.length / 2

      if (sorted.length % 2 == 1) {
        sorted(middle)
      } else {
        op.median(sorted(middle), sorted(middle - 1))
      }
    }
  }
}
