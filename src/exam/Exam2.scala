package exam

object Exam2 {
  // Task 1
  def from(a: Int, b: Int, c: Int) : Stream[Int] = a #:: from(b, c, a + b + c)

  val tribonacci = from(1, 1, 2)
  val tribonacciOdd = tribonacci.filter(_ % 2 == 1)
  val sumTribonacciOdd100 = tribonacci.take(100).sum

  case class Movie( year: Int,
                    length: Int,
                    title: String,
                    subject: String,
                    actor: String,
                    actress: String,
                    director: String,
                    popularity: Int,
                    awards: Boolean)

  def main(args: Array[String]): Unit = {
    // Task 1 tests
    println("tribonacci - first 20 members: ")
    tribonacci.take(20).foreach(e => print(e + " "))
    println
    println("tribonacciOdd - first 20 members: ")
    tribonacciOdd.take(20).foreach(e => print(e + " "))
    println
    println("sumTribonacciOdd100: " + sumTribonacciOdd100)

    // Task2
    val data = (0 to 10000)

    import scala.io.Source
    val fileContents = Source.fromFile("movies.txt").getLines.toList

    val movies = fileContents.map(x => {
      val data = x.split(";");
      Movie(data(0).toInt, data(1).toInt, data(2), data(3), data(4), data(5), data(6), data(7).toInt, data(8).toBoolean)
    })

    // 2А
    val movies80s = movies.filter(x => x.year >= 1980 && x.year < 1990)
    val sumDuration80s = movies80s.map(_.length).sum
    val avgDuration80s = sumDuration80s / movies80s.length

    // 2B
    val exists = movies.exists { x => x.length < 52 && x.year > 1960 }

    // 2C
    val actresses = movies.filter { _.title.split(" ").length == 3 }.map(_.actress)

    // 2D
    val noAwards = movies.filter(_.awards == false).sortBy(_.popularity)

    // 3
    val (decade, noComedies) = movies
      .filter(_.subject.toLowerCase() == "comedy")
      .groupBy(x => (x.year - 1900) / 10).toList
      .maxBy(x => x._2.length)
  }
}
