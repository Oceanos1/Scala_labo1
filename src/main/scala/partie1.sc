def op(oper:Char, a: Double, b:Double): Double = oper match{
  case '+' => a+b
  case '-' => a-b
  case '*' => a*b
  case '^' => pow(a,Math.floor(b).toInt)
  //operations binaires
}

def op(oper:Char, a: Double): Double = oper match{
  case '!' => fact(a)
  //operations unaires
}

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

def solve()

def pow(a: Double, b:Int): Double ={
  def loop(acc: Double, a: Double, b: Int): Double ={
    if(b == 0){
      1.0
    }else if(b == 0){
      acc
    }else{
      loop(acc * a, a, b);
    }
  }

  loop(1, a, b)
}