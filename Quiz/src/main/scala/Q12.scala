object Q12 {
  /*
Q12. Given a list:List[Int] and map:Map[Int, Double], multiply all the numbers in the list with their corresponding value in the map, and drop if don't exists
for example: list = [1,2,3,4], map = {1 -> 3, 3-> 5} ==> res = [3.0, 15.0]
   */

  def MultCorrespondings(list: List[Int],
                         map: Map[Int, Double]): List[Double] = {
    list.foldLeft(List[Double]())((list, number) => {
      val mapVal = map get number
      if (mapVal.isEmpty) list else list :+ (mapVal.get * number)
    })
  }
}
