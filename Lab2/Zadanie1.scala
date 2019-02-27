import io.StdIn

object Zadanie1 extends App {

  print("Podaj liczbę n:")

  var n = StdIn.readInt()

  var text = new Array[String](n)

  var i = 0

  while(i < n) {

    println("Podaj kolejny napis:")
    text(i) = StdIn.readLine()
    i += 1

  }

  var k = n

  do {
    i = 0
    while(i < k-1) {
      if(text(i).length > text(i+1).length) {
        //swap
        val temp = text(i)
        text(i) = text(i+1)
        text(i+1) = temp
      } else if(text(i).length == text(i+1).length) {
        if(text(i) > text(i+1)) {
          //swap lex
          val temp = text(i)
          text(i) = text(i+1)
          text(i+1) = temp
        }
      }
      i += 1
    }
    k -= 1
  } while(k > 1)

  i = 0
  while(i < n) {

    println(text(i))
    i += 1

  }

}

