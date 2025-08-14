import derivatives.{derivative,
                    derivative_addition,
                    derivative_subtraction,
                    derivative_multiplication,
                    derivative_division}


object Main extends App {
	val f: Double => Double = x => x * x // f(x) = x^2
	val g: Double => Double = x => Math.sin(x) // g(x) = sin(x)

	// Test points
	val x = Math.PI / 4 // π / 4 ≈ 0.785
	var result = 0.0

	// 1.1: Basic derivative
	println("1.1 Basic derivatives:")
	println(s"f'(${x}) = ${derivative(f)(x)}") // 2x = 1.57
	println(s"g'(${x}) = ${derivative(g)(x)}") // cos(x) = 0.707
	println()

	// 1.2.1: Addition of derivatives
	println("1.2.1 Addition of derivatives:")
	result = derivative_addition(f, g)(x)
	println(s"(f + g)'(${x}) = ${result}") // 2.27790...
	println()

	// 1.2.2: Subtraction of derivatives
	println("1.2.2 Subtraction of derivatives:")
	result = derivative_subtraction(f, g)(x)
	println(s"(f - g)'(${x}) = ${result}") // 0.86368...
	println()

	// 1.2.3: Product rule
	println("1.2.3 Product rule:")
	result = derivative_multiplication(f, g)(x)
	println(s"(f * g)'(${x}) = ${result}") // 1.54689...
	println()

	// 1.2.4: Quotient rule
	println("1.2.4 Quotient rule:")
	result = derivative_division(f, g)(x)
	println(s"(f / g)'(${x}) = ${result}") // 1.34908...
}
