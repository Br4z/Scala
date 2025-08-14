package recursion {
	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */

	/* ---------------------------------- 1.1.1 --------------------------------- */
	/**
	  * Calculates the greatest common divisor (GCD) using the Trial Factorization Algorithm (TFA).
	  *
	  * @param ln     The list of integers representing the exponents of the prime factors for the first number.
	  * @param lm     The list of integers representing the exponents of the prime factors for the second number.
	  * @param primes The list of prime numbers.
	  * @return the GCD of the two numbers.
	  */
	def GCD_TFA(ln : List[Int], lm : List[Int], primes : List[Int]) : Int = {
		if (primes.isEmpty)
			1
		else {
			val min_exponent = Math.min(ln.head, lm.head)
			Math.pow(primes.head, min_exponent).toInt * GCD_TFA(ln.tail, lm.tail, primes.tail)
		}
	}

	/* ---------------------------------- 1.1.2 --------------------------------- */
	/**
	  * Calculates the greatest common divisor (GCD) of two integers using the Bezout's identity algorithm.
	  *
	  * @param n The first integer.
	  * @param m The second integer.
	  * @return A tuple containing the GCD and the Bezout coefficients (x, y) such that d = gcd(n, m) =
	  *          n * x + m * y.
	  */
	def GCD_Bezout(n : Int, m : Int) : (Int, Int, Int) = {
		if (m == 0)
			(n, 1, 0)
		else {
			val (d, x1, y1) = GCD_Bezout(m, n % m)

			(d, y1, x1 - n / m * y1)
		}
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Calculates the nth Fibonacci number using tree recursion.
	  *
	  * @param n The position of the Fibonacci number to calculate.
	  * @return the nth Fibonacci number.
	  */
	def tree_recursion_fibonacci(n : Int) : Int = {
		if (n == 0)
			0
		else if (n == 1)
			1
		else
			tree_recursion_fibonacci(n - 1) + tree_recursion_fibonacci(n - 2)
	}


	/**
	  * Calculates the Nth Fibonacci number using an iterative approach.
	  *
	  * @param N The position of the Fibonacci number to calculate.
	  * @return the Nth Fibonacci number.
	  */
	def iterative_fibonacci(N : Int) = {
		def aux(n : Int, a : Int, b : Int) : Int = {
			if(n == 0)
				a
			else
				aux(n - 1, b, a + b)
		}
		aux(N, 0, 1)
	}
}
