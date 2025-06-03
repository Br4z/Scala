import symbolic_derivatives._


object Main extends App {
	// Create a simple expression: f(x) = x^2 + ln(x)
	val x = Atom('x')
	val expression = Addition(
		Exponentiation(x, Number(2)),
		NaturalLogarithm(x)
	)

	println("Original expression: " + show(expression)) // ((x ^ 2.0) + (ln(x)))

	// Take the derivative
	val derivative = derivate(expression, x)
	println("Derivative: " + show(derivative))

	// Clean up the derivative expression
	val cleaned_derivative = clean(derivative)
	println("Cleaned derivative: " + show(cleaned_derivative))

	// Evaluate the original expression at x = 2
	val value = evaluate(expression, x, 2.0)
	println(s"f(2) = $value") // 4.69314...

	// Find the root of f(x) using Newton's method, starting at x = 1.0
	val root = Newton_method(expression, x, 1.0, good_approximation)
	println(s"Root found at x ≈ $root") // 0.65
}
