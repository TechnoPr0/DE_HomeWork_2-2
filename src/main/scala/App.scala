import scala.io.StdIn
import scala.math.BigDecimal.RoundingMode

object App {

  def main(args: Array[String]): Unit = {
    val salaryStaff = List(100000, 150000, 200000, 80000, 120000, 75000)
    var newSalaryStaff = List[Int]()
    var floatSalaryStaff = List[Float]()

    //a
    def trainString() {
      val string = "Hello, Scala!"
      println(string.reverse)
      println(string.toLowerCase())
      println(string.replace("!", ""))
      println(string + " and goodbye python!")
    }
    //trainString()

    //b
    def salary():Float={
      //Ввод данных о зарплате.
      val salaryOfYear = StdIn.readLine("Введите годовую зарплату сотрудника: \n").toFloat
      //Процент пишется целым числом т.е. 13% нужно писать 13, а не 0.13
      val bonusSalary = StdIn.readLine("Введите процент премии от зарплаты: \n").toFloat
      val compensationFood = StdIn.readLine("Введите сумму компенсациии питания: \n").toFloat

      // Считаем месячную зарплату за вычетом налогов
      val salaryOfMonth = ((((salaryOfYear - (salaryOfYear * 0.13)) - ((bonusSalary / 100) * salaryOfYear)) - compensationFood) / 12)
      //Округляем число до 2х знаков после запятой в большую сторону.
      val roundSalary = BigDecimal(salaryOfMonth) setScale(2, RoundingMode.HALF_UP)
      val finalSalary = roundSalary.toString().toFloat
      println(s"Ежемесячный доход сотрудника: $finalSalary руб.")
      finalSalary
    }

    //c
    def meanSalaryStaff(salaryStaff : List[Int]){
      var response = ""
      val meanSalary: Float = salaryStaff.sum/salaryStaff.length
      val employeeSalary = salary()
      val deviationOfSalary = (((employeeSalary * 100) / meanSalary)-100).toInt
      if (deviationOfSalary > 0) {
        response = "+"+deviationOfSalary.toString
      }
      else if (deviationOfSalary == 0){
        response = "0"
      }
      else if (deviationOfSalary < 0){
        response = deviationOfSalary.toString
      }
      println (s"Отклонение от средней зарплаты: $response%")
      response
    }
    //meanSalaryStaff(salaryStaff)

    //d
    def correctSalary(): Unit ={

      val employeeSalary = salary()
      println("Введите сумму премии/штрафа (при штрафе добавьте знак'-' перед числом:")
      val correct = StdIn.readInt()
      val sumSalary = employeeSalary + correct
      println(sumSalary)
      newSalaryStaff = salaryStaff :+ sumSalary.toInt
      val max = newSalaryStaff.max
      val min = newSalaryStaff.min
      println(s"Самая высокая зарплата: $max")
      println(s"Самая низкая зарплата: $min")

    }
    //correctSalary()
    //println(newSalaryStaff)

    //e
    def newWorkers(): Unit ={

      val salaryList = List(350000, 90000)
      newSalaryStaff = salaryStaff:::salaryList
      println(newSalaryStaff.sorted)
    }
    //newWorkers()

    //f
    def newEmployee(): Unit ={
      var i = 0
      for (salary <- salaryStaff){
        if (i == 1){
          newSalaryStaff = newSalaryStaff :+ 130000
        }
        else{
          newSalaryStaff = newSalaryStaff :+ salary
        }
        i += 1
      }
      println(newSalaryStaff)
    }
    //newEmployee()

    //g
    def salaryOfMiddle(salaryStaff : List[Int]){
      for (worker <- salaryStaff){
        if ((worker >= 100000) && (worker <= 150000)){
          println(s"Зарплата middle разработчика: $worker")
        }
      }
    }
    //salaryOfMiddle(salaryStaff)

    //h
    //Первым параметром указывается список, вторым параметром индексация зарплаты в %
    def salaryIndexing(salaryStaff : List[Int], indexing : Float){
      for (salary <- salaryStaff) {
        val floatSalary = (salary+ (salary * (indexing / 100)))
        newSalaryStaff = newSalaryStaff :+ floatSalary.toInt
      }
      println(newSalaryStaff)
    }
    //salaryIndexing(salaryStaff, 7)
  }
}
