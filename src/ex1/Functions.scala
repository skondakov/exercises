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

  def map(chars: List[Char], f: Char => Unit): Unit = {
      f(chars.head)
      if (!chars.tail.isEmpty) map(chars.tail, f)
  }

  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) = ???
  
    ???
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Any) = ???

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: Any) = ???

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Any) = ???

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = ???
}
