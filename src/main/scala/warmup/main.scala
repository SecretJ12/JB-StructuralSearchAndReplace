package de.secretj12
package warmup

@main
def main(): Unit = {
  println(f(10))
  println(g(10))
  println(h(10))
}

/**
 * Calls the function twice for every number
 * Therefor runs in O(2^n^)
 */
def f(x: Int): Int = x match {
  case 0 => 1
  case x => f(x - 1) + f(x - 1)
}

/**
 * Improvement:
 * Only call the next function once
 * Leads to a linear complexity O(n)
 */
def g(x: Int): Int = x match {
  case 0 => 1
  case x => 2 * f(x - 1)
}

/**
 * The boring version:
 * Just using the library function
 */
def h(x: Int): Int = Math.pow(2, x).toInt