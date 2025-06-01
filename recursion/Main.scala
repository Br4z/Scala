import recursion.*


object Main extends App {
	// Test GCD_TFA
	val primes = List(2, 3, 5, 7, 11)
	val n_1 = List(2, 1, 0, 1, 0) // represents 2^2 x 3 x 7 = 84
	val n_2 = List(1, 2, 0, 0, 0) // represents 2 x 3^2 = 18

	println("Testing GCD_TFA:")
	val tfa = GCD_TFA(n_1, n_2, primes)
	println(s"GCD using TFA: $tfa") // 6 (2 x 3)

	// Test GCD_Bezout
	val n = 84
	val m = 18
	println("\nTesting GCD_Bezout:")
	val (gcd, x, y) = GCD_Bezout(n, m)
	println(s"GCD of $n and $m is: $gcd")
	println(s"Bezout coefficients: x = $x, y = $y")
	println(s"Verification: ${n} * ${x} + ${m} * ${y} = ${n * x + m * y}")

	// Test Fibonacci implementations
	val fib_n= 10
	println("\nTesting Fibonacci implementations:")
	println(s"Tree recursion fibonacci($fib_n) = ${tree_recursion_fibonacci(fib_n)}")
	println(s"Iterative fibonacci($fib_n) = ${iterative_fibonacci(fib_n)}")
}
