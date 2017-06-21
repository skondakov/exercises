package ex1

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = if (data.isEmpty) 0 else length(data.tail) + 1
  
  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int) = if (cond) onTrue else onFalse

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)( 
  def balance(chars: List[Char], open: Int = 0): Boolean = {
    
    if (chars.isEmpty) return open == 0
      
    var newOpen = open;
    if (chars.head == ')') {
      if (open <= 0)
        return false
      else 
        newOpen = open - 1
    } else if (chars.head == '(') {
       newOpen = open + 1;
    }

    return balance(chars.tail, newOpen)
  }

  def map(chars: List[Char], f: Char => Any): List[Any] = for (char <- chars) yield f(char)
  
  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) = if (char.toInt >= 97 && char.toInt <= 122) (char -32).toChar else char
    for (char <- chars) yield upperCase(char)
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Any => Boolean): Boolean = {
      for (item <- data) if (f(item)) return true
      return false
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: Int => Boolean): List[Int] = for (item <- data if (f(item))) yield item


  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Int => Boolean) = filter(data, f).length == data.length

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = ???
}
