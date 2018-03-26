import scala.collection.mutable.Map

//operations binaires
def op(oper: Char, a: Double, b: Double): Double = oper match {
  case '+' => a + b
  case '-' => a - b
  case '*' => a * b
  case '^' => pow(a, Math.floor(b).toInt)
  case '%' => a % b
  //case '/' => a%b

}

//operations unaires
def op(oper: Char, a: Double): Double = oper match {
  case '!' => fact(a.toInt)
}

/**
  * Calcule le Plus Grand Diviseur Commun de deux nombres
  *
  * @param a le premier nombre
  * @param b le deuxième nombre
  * @return le plus grand diviseur commun des deux nombres
  */
def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

/**
  * Résoud une équation du second degrés
  *
  * @param a le coefficient a
  * @param b le coefficient b
  * @param c le coefficient c
  * @return la solution de l'équation du second degré sous la forme suivante : un tuple de deux Double si le determinant est plus grand que 0,
  *         un tuple d'un Double si le determinant vaut 0 et un tuple de deux tuple correspondant chacun a la partie reel et la partie imaginaire de la solution
  */
def solve(a: Double, b: Double, c: Double): Any = {
  def discriminant(a: Double, b: Double, c: Double): Double = b * b - 4 * a * c

  if (a == 0) {
    return new Exception("L'équation n'est pas du second degrés")
  }
  val disc = discriminant(a, b, c)
  disc match {
    case j if j > 0 => ((-b + Math.sqrt(disc)) / (2 * a), (-b - Math.sqrt(disc)) / (2 * a))
    case k if k == 0 => ((-b + Math.sqrt(disc)) / (2 * a))
    case _ => ((-b / (2 * a), Math.sqrt(-disc) / (2 * a)), (-b / (2 * a), -Math.sqrt(-disc) / (2 * a)))

  }
}

/**
  * Calcule récursivement la factorielle d'un entier
  *
  * @param a
  * @return
  */
def fact(a: Int): Int = {
  def loop(acc: Int, a: Int): Int = {
    if (a < 2) {
      acc
    } else {
      loop(acc * a, a - 1);
    }
  }

  loop(1, a)
}

/**
  * Calcule récursivement 'a' à la puissance 'b'. Ne prend que les puissances
  * entières en paramètre.
  *
  * @param a
  * @param b
  * @return
  */
def pow(a: Double, b: Int): Double = {
  def loop(acc: Double, a: Double, b: Int): Double = {
    if (b == 0) {
      1.0
    } else if (b == 1) {
      acc
    } else {
      loop(acc * a, a, b - 1);
    }
  }

  loop(a, a, b)
}

/**
  * Calcule si un nombre est premier
  *
  * @param number le nombre à tester
  * @return un Boolean valant true s'il est premier
  */
def isPrime(number: Int): Boolean = {
  def loop(n: Int, a: Int): Boolean = {
    if (a < 2) {
      true
    } else if (n % a == 0) {
      false
    } else {
      loop(n, a - 1)
    }
  }

  loop(number, Math.sqrt(number).floor.toInt)
}

/**
  * Calcule la racine carrée d'un nombre
  *
  * @param number le nombre dont on veut la racine carrée
  * @return la racine carrée du nombre passé en paramètre 
  */
def square(number: Int): Double = {
  val epsilon = 0.0001

  def loop(n: Int, x: Double): Double = {
    if (Math.abs(x * x - n) / n < epsilon) x else loop(n, (x + (n / x)) / 2)
  }

  loop(number, 1)
}

/**
  * Calcule l'algorithme d'Euclide étendu récursivement
  * Utilisation du pseudocode:
  * https://fr.wikipedia.org/wiki/Algorithme_d'Euclide_%C3%A9tendu#Exemple_introductif
  *
  * @param a
  * @param b
  * @return le tuple (r, u, v) tel que a * u + b * v = r
  */
def egcd(a: Int, b: Int): (Int, Int, Int) = {
  def loop(r: Int, u: Int, v: Int, r_prime: Int, u_prime: Int, v_prime: Int): (Int, Int, Int) = {
    if (r_prime == 0) {
      (r, u, v)
    } else {
      val q = r / r_prime
      loop(r_prime, u_prime, v_prime, r - q * r_prime, u - q * u_prime, v - q * v_prime)
    }
  }

  loop(a, 1, 0, b, 0, 1)
}

/**
  * Calcule l'inverse modulaire de l'entier a modulo b
  *
  * @param a
  * @param b
  * @return l'inverse modulaire ou None si il n'y en a pas
  */
def ModInvert(a: Int, b: Int) = {
  val (d, e, f) = egcd(a, b)
  if (d != 1) {
    None
  } else {
    e
  }
}

/**
  * Allows storing and loading of variables by name
  */
object Memory {
  //Represents the memory, maps Strings (names) to Doubles (values)
  var memory: Map[String, Double] = Map()

  def checkIsValidName(name: String): Boolean = {
    for (char <- name) {
      if (char < 'A' || char > 'z') {
        false
      }
    }
    true
  }

  /**
    * Stores a variable in memory. If the name if incorrect, does nothing. The
    * name must match the format [a-zA-Z]+
    *
    * @param name
    * @param value
    * @return
    */
  def store(name: String, value: Double) = {
    if (checkIsValidName(name)) {
      memory(name) = value
    }
  }

  /**
    * Loads a variable from memory
    *
    * @param name
    * @return the variable if it is memory, otherwise None
    */
  def load(name: String) = {
    memory.getOrElse(name, None)
  }

}

