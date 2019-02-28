import io.StdIn

object Zadanie3 extends App {

  /**
    * Zakładamy, że A=0, B=1, ... Z=24 - każdej literze przyporządkowujemy kod
    * Gdy szyfrujemy literę 'a' względem litery 'b' z klucza chcemy aby para (a, b) dała nam 'c' - zaszyfrowaną literę
    * Wtedy (a,b) = c = (a+b)%25
    * Na przykład (T, N) = G, czyli (19, 13)= 32%25 = 6 = G
    */
  val max = ('Z'-'A') + 1
  def code(a: Int, b:Int) : Int = return (a+b)%max

  println("Podaj słowo do zaszyfrowania:")
  var value = StdIn.readLine()

  println("Podaj klucz:")
  var key = StdIn.readLine()

  value = value.toUpperCase()
  key   = key.toUpperCase()

  //Delete white spaces
  var i = 0
  while(i < value.length) {
    if(value.charAt(i) == ' ') {
      value = value.substring(0, i) + value.substring(i+1)
      i -= 1
    }
    i += 1
  }

  //extend key
  i = 0
  while(value.length - key.length != 0) {
    key = key + key.charAt(i)
    i += 1
    if(i == key.length) {
      i = 0
    }
  }

  var coded = ""

  //code letters
  i = 0
  while(i < value.length) {
    val a = value.charAt(i) - 65
    val b = key.charAt(i) - 65
    coded += (code(a, b)).toChar
    i += 1
  }

  println("Słowo po zaszyfrowaniu: " +coded)

}
