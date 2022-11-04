var i = 0
def degreeOfTwo(num: Int): Int = {
  if (num > 1) {
    i += 1
    degreeOfTwo(num / 2)
  }
  else i
}
println(degreeOfTwo(64))

