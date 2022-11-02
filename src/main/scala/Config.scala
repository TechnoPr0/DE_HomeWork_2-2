object Config {
  val name = "Hello, "
  def main(args: Array[String]): Unit = {
    val name = "Oleg"
    greeting(name)
  }



  def greeting(name: String) {
    import Config.{ name => prefix}
    println(prefix + name)
  }
}



