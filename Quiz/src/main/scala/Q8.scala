object Q8 {
  /*
Q8. Given the following code:
```
trait IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
}
class EmptySet extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
}
class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
        if (x < elem) left contains x
        else if (x > elem) right contains x
        else true
    def incl(x: Int): IntSet =
        if (x < elem) new NonEmptySet(elem, left incl x, right)
        else if (x > elem) new NonEmptySet(elem, left, right incl x)
        else this
}
Write methods union and intersection to form the union and intersection between two sets. Add a method

def excl(x: Int)
to return the given set without the element x. To accomplish this, it is useful to also implement a test method

def isEmpty: Boolean
for sets.
```
   */

  trait IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def isEmpty: Boolean
    def union(intSet: IntSet): IntSet
    def intersection(intSet: IntSet): IntSet
    def excl(x: Int): IntSet
    def foldLeft[T](startValue: T)(f: (T, Int) => T): T
    def min: Option[Int]
  }

  class EmptySet extends IntSet {
    override def contains(x: Int): Boolean = false
    override def incl(x: Int): IntSet =
      new NonEmptySet(x, new EmptySet, new EmptySet)
    override def isEmpty: Boolean = true
    override def excl(x: Int): IntSet = new EmptySet
    override def union(intSet: IntSet): IntSet = intSet
    override def intersection(intSet: IntSet): IntSet = new EmptySet
    override def foldLeft[T](startValue: T)(f: (T, Int) => T): T = startValue
    override def min: Option[Int] = None
  }

  class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmptySet(elem, left incl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right incl x)
      else this

    override def isEmpty: Boolean = false

    override def excl(x: Int): IntSet = {
      if (x < elem) new NonEmptySet(elem, left excl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right excl x)
      else if (left.isEmpty) right
      else if (right.isEmpty) left
      else {
        val minElem = right.min.get // right branch is not empty, there should be value, unless we have a race
        new NonEmptySet(minElem, left, right excl minElem)
      }
    }

    def min: Option[Int] =
      this.foldLeft(Some(elem))((min, value) => Some(Math.min(min.get, value)))

    override def union(intSet: IntSet): IntSet = {
      this.foldLeft(intSet)((set, value) => set.incl(value))
    }

    override def intersection(intSet: IntSet): IntSet = {
      this.foldLeft[IntSet](new EmptySet())((set, value) =>
        if (intSet contains value) set incl value else set)
    }

    override def foldLeft[T](startValue: T)(f: (T, Int) => T): T = {
      val leftResult = left.foldLeft(startValue)(f)
      val center = f(leftResult, elem)
      val rightResult = right.foldLeft(center)(f)
      rightResult
    }
  }
}
