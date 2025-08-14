package object derivatives {
	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Creates a generic numerical derivative function using the five-point stencil
	  * approximation. Given any real-to-real function "f", this returns a new function
	  * that approximates f′(x) at each point x.
	  *
	  * @param f The original function to differentiate (Double => Double).
	  * @return a new function (Double => Double) that, when given x, returns the approximate derivative f′(x).
	  */
	def derivative(f: Double => Double): Double => Double = {
		(x: Double) => {
			val h = 0.1
			(f(x - 2 * h) - 8 * f(x - h) + 8 * f(x + h) - f(x + 2 * h)) / (12 * h)
		}
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */

	/* ---------------------------------- 1.2.1 --------------------------------- */
	/**
	  * Constructs the derivative of the sum of two functions. Given functions "f" and "g",
	  * returns a new function that computes (f + g)′(x) = f′(x) + g′(x).
	  *
	  * @param f First function of type (Double => Double).
	  * @param g Second function of type (Double => Double).
	  * @return a function (Double => Double) that, for each x, returns (f + g)′(x).
	  */
	def derivative_addition(f: Double => Double, g: Double => Double): Double => Double = {
		(x: Double) => {
			derivative(f)(x) + derivative(g)(x)
		}
	}

	/* ---------------------------------- 1.2.2 --------------------------------- */
	/**
	  * Constructs the derivative of the difference of two functions. Given functions "f" and "g",
	  * returns a new function that computes (f − g)′(x) = f′(x) − g′(x).
	  *
	  * @param f First function of type (Double => Double).
	  * @param g Second function of type (Double => Double).
	  * @return a function (Double => Double) that, for each x, returns (f − g)′(x).
	  */
	def derivative_subtraction(f: Double => Double, g: Double => Double): Double => Double = {
		(x: Double) => {
			derivative(f)(x) - derivative(g)(x)
		}
	}

	/* ---------------------------------- 1.2.3 --------------------------------- */
	/**
	  * Constructs the derivative of the product of two functions. Given functions "f" and "g",
	  * returns a new function that computes (f * g)′(x) = f′(x) * g(x) + f(x) * g′(x).
	  *
	  * @param f function of type (Double => Double).
	  * @param g Second function of type (Double => Double).
	  * @return a function (Double => Double) that, for each x, returns (f * g)′(x).
	  */
	def derivative_multiplication(f: Double => Double, g: Double => Double): Double => Double = {
		(x: Double) => {
			derivative(f)(x) * g(x) + f(x) * derivative(g)(x)
		}
	}

	/* ---------------------------------- 1.2.4 --------------------------------- */
	/**
	  * Constructs the derivative of the quotient of two functions. Given functions "f" and "g",
	  * returns a new function that computes (f / g)′(x) = [f′(x)·g(x) − f(x)·g′(x)] / [g(x)²].
	  *
	  * @param f Numerator function of type (Double => Double).
	  * @param g Denominator function of type (Double => Double). Must not be zero at x.
	  * @return A function (Double => Double) that, for each x, returns (f / g)′(x).
	  */
	def derivative_division(f: Double => Double, g: Double => Double): Double => Double = {
		(x: Double) => {
			(derivative(f)(x) * g(x) - f(x) * derivative(g)(x)) / (g(x) * g(x))
		}
	}
}
