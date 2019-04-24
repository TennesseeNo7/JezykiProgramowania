package src.exercises.mastermind

class Console {

  private val in = io.StdIn

  def initialize(firstRun: Boolean): Unit = {
    if(firstRun) println("Welcome to mastermind!")
    println("Type:")
    println("\t-> 'start' to start the game")
    println("\t-> 'help' to show help")
    println("\t-> 'exit' to exit the game")
    println()
    print(">")
  }

  def run(): Unit = in.readLine() match {
    case "start" => (new Game).start(); initialize(false); run()
    case "help" => help(); print(">"); run()
    case "exit" => stop()
  }

  def stop(): Unit = println("Goodbye!")

  def help(): Unit = {
    val filename = "\\src\\exercises\\mastermind\\help.txt"
    for (line <- scala.io.Source.fromFile(filename).getLines) {
      println(line)
    }
  }

}
