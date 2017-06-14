package exam

import scala.io.Source

// Task 1
case class Wolf(age: Int, cubs: List[Wolf]) {
  def this(age: Int) = this(age, Nil)
}

case class Pack(leader: Wolf) {
  def count = {
    def iterate(wolfs: List[Wolf], res: Int): Int = wolfs match {
      case Nil => res
      case w :: ws => iterate(ws ::: w.cubs, res + 1)
    }
    iterate(leader.cubs, 1)
  }

  def oldest = {
    def iterate(wolfs: List[Wolf], res: Int): Int = wolfs match {
      case Nil => res
      case w :: ws => iterate(ws ::: w.cubs, if (w.age > res) w.age else res)
    }
    iterate(leader.cubs, leader.age)
  }
}

case class Country( alpha2: String,
                    fips: String,
                    name: String,
                    capital: String,
                    area: Double,
                    population: Long,
                    continent: String)

object Exam3 {
  def main(args: Array[String]): Unit = {
    // Task 1 tests
    val myPack = Pack(Wolf(7, List(
                                Wolf(9, List(
                                          new Wolf(1),
                                          new Wolf(2),
                                          new Wolf(3))),
                                new Wolf(2),
                                Wolf(4, List(
                                          new Wolf(11),
                                          new Wolf(6))))))
    println("There are " + myPack.count + " in this pack")
    println("The oldest wolf in this pack is " + myPack.oldest + " years old")

    // Task 2
    def lineToCountry(data: Array[String]) = Country(data(0), data(1), data(2), data(3), data(4).toDouble, data(5).toInt, data(6))

    def groupByPopulation(c: Country) = if (c.population > 5000000) "Above 5M" else "Below 5M"

    def sumCountryPopulation(countries: List[Country]) = countries.map(_.population).sum

    def sumCountryArea(countries: List[Country]) = countries.map(_.area).sum

    val fileContent = Source.fromFile("countries.txt").getLines
    val countries = fileContent.map(x => lineToCountry(x.split(","))).toList

    // 3.1 Извежда столиците на всички държава с еднаква алфа и фипс кодове
    val capitalsSameCode = countries.filter(c => c.alpha2 == c.fips).map(_.capital)
    println(capitalsSameCode)

    // 3.2. Населението на държавата в Европа, чиято столица има най-дълго име (ако са повече от една – една от тях)
    val popOfLongestCapitalNameInEU = countries.filter(_.continent == "EU").maxBy(_.capital.length).population
    println(popOfLongestCapitalNameInEU)

    // 3.3. Колко човека живеят в общо в държави с население под 5 000 000 и колко в останалите? (с една операция)
    val popBelowAndAbove5M = countries.groupBy(groupByPopulation(_)) // Като резултат се получават 2 елемента от Tuple (String, List[Countries])
        .map(x => (x._1, sumCountryPopulation(x._2))) // Запазваме първия елемент от Tuple, a във втория връщаме като сума на населението на държавите
    println(popBelowAndAbove5M)

    // 3. 4. Средната гъстота на населението на всеки континент
    val avgPopDensityPerContinent = countries.groupBy(_.continent).map(x => (x._1, (sumCountryPopulation(x._2) / sumCountryArea(x._2))))
    println(avgPopDensityPerContinent)
  }
}
