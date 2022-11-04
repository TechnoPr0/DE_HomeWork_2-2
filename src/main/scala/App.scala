import scala.annotation.tailrec
import scala.io.StdIn
import scala.math.BigDecimal.RoundingMode

object App {

  def main(args: Array[String]): Unit = {

    val salaryStaff = List(100000, 150000, 200000, 80000, 120000, 75000)

    var juniorSalaryStaff = List[Int]()
    var middleSalaryStaff = List[Int]()
    var seniorSalaryStaff = List[Int]()

    var floatSalaryStaff = List[Float]()

    //a
    //Напишите программу, которая:
    //i.выводит фразу «Hello, Scala!» справа налево
    //ii.     переводит всю фразу в нижний регистр
    //iii.     удаляет символ!
    //iv.     добавляет в конец фразы «and goodbye python!»
    def trainString() {
      val string = "Hello, Scala!"
      println(string.reverse)
      println(string.toLowerCase())
      println(string.replace("!", ""))
      println(string + " and goodbye python!")
    }
    //trainString()


    //b
    // Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
    // На вход вашей программе подается значение годового дохода до вычета налогов,
    // размер премии – в процентах от годового дохода и компенсация питания.

    //n.
    // *Опробуйте завернуть программу из пункта 3.b в функцию и входные значения переделать в параметры функции.

    def salary(salary: Float = 0, bonus: Float = 0, compensation: Float = 0): Float = {
      var salaryOfYear = salary
      var bonusSalary = bonus
      var compensationFood = compensation

      // Если в функцию не передали значения - просим пользователя их ввести:
      if ((salaryOfYear == 0) && (bonusSalary == 0) && (compensationFood == 0)) {
        //Ввод данных о зарплате.
        salaryOfYear = StdIn.readLine("Введите годовую зарплату сотрудника: \n").toFloat
        //Процент пишется целым числом т.е. 13% нужно писать 13, а не 0.13
        bonusSalary = StdIn.readLine("Введите процент премии от зарплаты: \n").toFloat
        compensationFood = StdIn.readLine("Введите сумму компенсациии питания: \n").toFloat
      }
      // Считаем месячную зарплату за вычетом налогов
      val salaryOfMonth = ((((salaryOfYear - (salaryOfYear * 0.13)) - ((bonusSalary / 100) * salaryOfYear)) - compensationFood) / 12)
      //Округляем число до 2х знаков после запятой в большую сторону.
      val roundSalary = BigDecimal(salaryOfMonth) setScale(2, RoundingMode.HALF_UP)
      val finalSalary = roundSalary.toString().toFloat
      println(s"Ежемесячный доход сотрудника: $finalSalary руб.")
      finalSalary
    }

    //salary(1500000, 10, 50000)
    //c
    // Напишите программу, которая рассчитывает для
    // каждого сотрудника отклонение(в процентах) от среднего значения оклада на уровень всего отдела.
    // В итоговом значении должно учитываться в большую или меньшую сторону отклоняется размер оклада.
    // На вход вышей программе подаются все значения, аналогичные предыдущей программе,
    // а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.
    def meanSalaryStaff(salaryStaff: List[Int]) {
      var response = ""
      val meanSalary: Float = salaryStaff.sum / salaryStaff.length
      val employeeSalary = salary()
      val deviationOfSalary = (((employeeSalary * 100) / meanSalary) - 100).toInt
      if (deviationOfSalary > 0) {
        response = "+" + deviationOfSalary.toString
      }
      else if (deviationOfSalary == 0) {
        response = "0"
      }
      else if (deviationOfSalary < 0) {
        response = deviationOfSalary.toString
      }
      println(s"Отклонение от средней зарплаты: $response%")
      response
    }
    //meanSalaryStaff(salaryStaff)

    //d
    // Попробуйте рассчитать новую зарплату сотрудника,
    // добавив(или отняв, если сотрудник плохо себя вел) необходимую сумму с учетом результатов прошлого задания.
    // Добавьте его зарплату в список и вычислите значение самой высокой зарплаты и самой низкой.
    def correctSalary(): List[Int] = {
      var nSalaryStaff = List[Int]()
      val employeeSalary = salary()
      println("Введите сумму премии/штрафа (при штрафе добавьте знак'-' перед числом:")
      val correct = StdIn.readInt()
      val sumSalary = employeeSalary + correct
      println(sumSalary)
      nSalaryStaff = salaryStaff :+ sumSalary.toInt
      val max = nSalaryStaff.max
      val min = nSalaryStaff.min
      println(s"Самая высокая зарплата: $max")
      println(s"Самая низкая зарплата: $min")
      nSalaryStaff
    }


    var newSalaryStaff = List[Int]()
    //newSalaryStaff = correctSalary()

    //e
    // Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
    // Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему.
    def newWorkers(): Unit = {

      val salaryList = List(350000, 90000)
      newSalaryStaff = salaryStaff ::: salaryList
      println(newSalaryStaff.sorted)
    }
    //newWorkers()

    //f
    // Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч.
    // Вычислите самостоятельно номер сотрудника в списке так,
    // чтобы сортировка не нарушилась и добавьте его на это место.

    def newEmployee(): Unit = {
      var i = 0
      for (salary <- salaryStaff) {
        if (i == 1) {
          newSalaryStaff = newSalaryStaff :+ 130000
        }
        else {
          newSalaryStaff = newSalaryStaff :+ salary
        }
        i += 1
      }
      println(newSalaryStaff)
    }
    //newEmployee()

    //g
    // Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle.
    // На входе программе подается «вилка» зарплаты специалистов уровня middle.
    def salaryOfMiddle(salaryStaff: List[Int]) {
      for (worker <- salaryStaff) {
        if ((worker > 100000) && (worker <= 150000)) {
          middleSalaryStaff = middleSalaryStaff :+ worker
        }
        else if (worker <= 100000) {
          juniorSalaryStaff = juniorSalaryStaff :+ worker
        }
        else if (worker > 150000) {
          seniorSalaryStaff = seniorSalaryStaff :+ worker
        }
      }
      println(juniorSalaryStaff)
      println(middleSalaryStaff)
      println(seniorSalaryStaff)
    }
    //salaryOfMiddle(salaryStaff)

    //h
    // Однако наступил кризис и ваши сотрудники требуют повысить зарплату.
    // Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции – 7%

    //Первым параметром указывается список, вторым параметром индексация зарплаты в %
    def salaryIndexing(salaryStaff: List[Int], indexing: Float) {
      for (salary <- salaryStaff) {
        val floatSalary = (salary + (salary * (indexing / 100)))
        newSalaryStaff = newSalaryStaff :+ floatSalary.toInt
      }
      println(newSalaryStaff)
    }
    //salaryIndexing(salaryStaff, 7)

    //i.*
    // Ваши сотрудники остались недовольны и просят индексацию на уровень рынка.
    // Попробуйте повторить ту же операцию, как и в предыдущем задании,
    // но теперь вам нужно проиндексировать зарплаты на процент отклонения от среднего по рынку
    // с учетом уровня специалиста.
    // На вход вашей программе подается 3 значения – среднее значение зарплаты на рынке
    // для каждого уровня специалистов(junior, middle и senior)

    def averageSalaries(levelSalaryStaff: List[Int], level: String): List[Int] = {
      println(s"Введите среднюю зарплату $level специалиста: ")
      val staffSalary = StdIn.readFloat()
      var percentList = List[Float]() //список для хранения отклонения процентов от средней зп.

      for (salary <- levelSalaryStaff) {
        percentList = percentList :+ ((staffSalary - salary.toFloat) / salary.toFloat) * 100
      }
      val meanPercent = percentList.sum / percentList.length
      println(s"Средний процент отклонения зарплаты $level специалиста: $meanPercent")

      var finalList = List[Int]()
      val len = levelSalaryStaff.length
      var i = 0
      while (i < len) {
        if (percentList(i) > 0) {
          finalList = finalList :+ (((levelSalaryStaff(i) / 100) * meanPercent) + levelSalaryStaff(i)).toInt
        }
        else {
          finalList = finalList :+ levelSalaryStaff(i)
        }
        i += 1
      }
      finalList
    }



    //val newJuniorList = averageSalaries(juniorSalaryStaff, "Junior")
    //print (newJuniorList)
    //val newMiddleList = averageSalaries(middleSalaryStaff, "Middle")
    //println (newMiddleList)
    //val newSeniorList = averageSalaries(seniorSalaryStaff, "Senior")
    //println(newSeniorList)

    //k.
    // *Попробуйте деанонимизировать ваших сотрудников – составьте структуру,
    // которая позволит иметь знания о том, сколько зарабатывает каждый сотрудник(Фамилия и имя).

    val salaryMap = Map(
      "Ефимова Анастасия" -> 100000,
      "Баранов Иван" -> 150000,
      "Ершова Диана" -> 200000,
      "Новиков Матвей" -> 80000,
      "Дьяков Матвей" -> 120000,
      "Рыбакова Алиса" -> 75000,
    )

    //m.
    // *Попробуйте запутать тех, кто может случайно наткнуться на эти данные – удалите для каждого сотрудника имя,
    // переведите строку в нижний регистр,
    // удалите гласные и разверните оставшиеся символы справа налево(abc -> cb).
    var editedSalaryMap = Map[String, Int]()

    for ((i, j) <- salaryMap) {
      var name = i.subSequence(0, i.indexOf(' ')).toString.toLowerCase.reverse
      val vovelLaters = List("а", 'у', 'о', 'ы', 'и', 'э', 'я', 'ю', 'ё', 'е')
      for (letter <- vovelLaters) {
        name = name.replace(letter.toString, "")
      }
      editedSalaryMap += (name -> j)

    }
    //println(editedSalaryMap)


    //o.      *Попробуйте написать функцию, которая вычисляет значение степени двойки:
    //i.     С помощью обычной рекурсии
    //ii.     **С помощью хвостовой рекурсии
    var iter = 0

    def degreeOfTwo(num: Int): Int = {

      if (num > 1) {
        iter += 1
        degreeOfTwo(num / 2)
      }
      else iter
    }
    //println(degreeOfTwo(64))

    def degreeOfTwoTail(num: Int):Int={
      @tailrec
      def degreeOfTwoInside(num: Int, acc: Int = 0): Int = {
        if (num > 1) {
          degreeOfTwoInside(num / 2, acc + 1)
        }
        else acc
      }
      degreeOfTwoInside(num)

    }
    //println(degreeOfTwoTail(64))
  }
}