import io.StdIn

object Zadanie2 extends App {

  var positive = 0
  var negative = 0

  var next = 0.0

  do {
    print("Podaj liczbę rzeczywistą:")
    next = StdIn.readDouble();
    if (next > 0.0) {
      positive += 1;
    } else if(next < 0.0) {
      negative += 1;
    }
  } while (next != 0.0)

  println("Wpisałeś " +positive+ " liczb dodatnich i " +negative+ " liczb ujemnych")

}
