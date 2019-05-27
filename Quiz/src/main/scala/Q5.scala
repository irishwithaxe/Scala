object Q5 {
  /*
Q5. Write a clause that measures the run time of a block of code and prints it (also needs to return the original output of the block):
```
timeit {
    ...
    ...
}
```
   */

  def timeit[T](codeblock: => T): T = {
    val started = System.nanoTime
    println("measure started")
    val v = codeblock
    val ended = System.nanoTime
    println("measure ended")

    val diff = ended - started
    println("code block was running for: " + diff + " nano sec")

    v
  }

}
