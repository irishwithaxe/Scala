object Q10 {
  /*Q10. Consider a function which squares all elements of a list and returns a list with the results. Complete the following two equivalent definitions of squareList.
```
def squareList(xs: List[Int]): List[Int] = xs match {
       case List() => ??
       case y :: ys => ??
 }
 def squareMapList(xs: List[Int]): List[Int] =
       xs map ??

   */

  def squareList(xs: List[Int]): List[Int] = xs match {
    case List()  => List()
    case y :: ys => y * y :: squareList(ys)
  }
  def squareMapList(xs: List[Int]): List[Int] =
    xs map { x =>
      x * x
    }

}
