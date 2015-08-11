package euler.util

object Prime {
  // thanks to https://gist.github.com/landon9720/2990101
  def factors(n:Int): List[Int] = {
    def divides(d:Int, n:Int) = (n % d) == 0
    def ld(n:Int):Int =  ldf(2, n)
    def ldf(k:Int, n:Int):Int = {
      if (divides(k, n)) k
      else if ((k*k) > n) n
      else ldf((k+1), n)
    }
    n match {
      case 1 => Nil
      case _ => val p = ld(n); (p :: factors(n / p)).distinct
    }
  }

  def isPrime(i: Long) = {
    if (i == 1) false
    else if (i == 2) true
    else if ((i & 1) == 0) false // efficient div by 2
    else prime(i)
  }
  

  private def primes: Stream[Long] = 2 #:: prime3

  // performance: avoid redundant divide by two, so this starts at 3
  private val prime3: Stream[Long] = {
    def next(i: Long): Stream[Long] =
      if (prime(i))
        i #:: next(i + 2)
      else
        next(i + 2) // tail
    3 #:: next(5)
  }

  // assumes not even, check evenness before calling - perf note: must pass partially applied >= method
  private def prime(i: Long) = {
    val root = math.sqrt(i)
    prime3 takeWhile (root >= _) forall { i % _ != 0 }
  }
}