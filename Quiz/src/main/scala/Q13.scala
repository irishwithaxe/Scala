object Q13 {
  /*
Q13. Write a retry method that converts a method to a retry-able method.
the syntax should look like:
```
retry { ... }
```
However, you also need some way to specify how many times to retry and support sleep between retries (hint: implicit arguments)

   */

  final case class RetryConfig(retryCnt: Int, sleepMilliSeconds: Int = 0)

  object RetryThreeTimes {
    implicit val retryConfig: RetryConfig = RetryConfig(3)
  }

  implicit val retryConfig: RetryConfig = RetryConfig(2)

  def retry(codeblock: => Unit)(implicit config: RetryConfig): Unit = {
    (0 until config.retryCnt).foreach { _ =>
      if (config.sleepMilliSeconds != 0)
        Thread.sleep(config.sleepMilliSeconds)

      codeblock
    }
  }
}
