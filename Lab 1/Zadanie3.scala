import io.StdIn

object Zadanie3 extends App {

  println("Podaj dwie liczby: ")

  var a = StdIn.readInt()
  var b = StdIn.readInt()

  var nwd = "NWD(" +a+ ", " +b+ ") = "

  var remainder = 1;

  do {
    remainder = a%b;
    a = b
    b = remainder
  } while(remainder != 0)

  print(nwd + a)

}
