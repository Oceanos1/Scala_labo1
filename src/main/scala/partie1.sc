def op(oper: Char, a: Double, b: Double): Double = oper match {
  case '+' => a + b
  case '-' => a - b
  case '*' => a * b
  //case '^' => pow(a,b)
  //operations binaires
}

def op(oper: Char, a: Double): Double = oper match {
  case '!' => 1
  //operations unaires
}

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

def solve(a: Double, b: Double, c: Double): Any = {
  def discriminant(a: Double, b: Double, c: Double): Double = b * b - 4 * a * c

  val disc = discriminant(a,b,c)
  println(disc)
  disc match {
    case j if j > 0 => ((-b + Math.sqrt(disc)) / (2 * a), (-b - Math.sqrt(disc)) / (2 * a))
    case k if k == 0 => (-b + Math.sqrt(disc)) / (2 * a)
    case _ => ()

  }
}

solve(1, -2, 1)
solve(1,3,-4)
solve(1, 0, 1)

