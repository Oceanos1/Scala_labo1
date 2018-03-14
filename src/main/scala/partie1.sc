def op(oper:Char, a: Double, b:Double): Double = oper match{
  case '+' => a+b
  case '-' => a-b
  case '*' => a*b
  case '^' => pow(a,b)
  //operations binaires
}

def op(oper:Char, a: Double): Double = oper match{
  case '!' => fact(a)
  //operations unaires
}

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

def solve()