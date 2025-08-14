package symbolic_derivatives {
	trait Expression

	case class Number(value: Double) extends Expression
	case class Atom(value: Char) extends Expression
	case class Addition(e_1: Expression, e_2: Expression) extends Expression
	case class Subtraction(e_1: Expression, e_2: Expression) extends Expression
	case class Multiplication(e_1: Expression, e_2: Expression) extends Expression
	case class Division(e_1: Expression, e_2: Expression) extends Expression
	case class Exponentiation(e_1: Expression, e_2: Expression) extends Expression
	case class NaturalLogarithm(value: Expression) extends Expression


	/* -------------------------------------------------------------------------- */
	/*                                      1                                     */
	/* -------------------------------------------------------------------------- */

	/* ----------------------------------- 1.1 ---------------------------------- */
	/**
	  * Converts an Expression into a fully parenthesized String form. This method
	  * ensures there is no ambiguity by surrounding every binary operation in
	  * parentheses and printing the operator symbol between its operands.
	  *
	  * @param e The Expression to format as a String.
	  * @return a String representation of "e", with parentheses around each subexpression.
	  */
	def show(e: Expression): String = {
		def formatter(e_1: Expression, e_2: Expression, operator: String): String =
			"(" + show(e_1) + " " + operator + " " + show(e_2) + ")"

		e match {
			case Number(v)               => v.toString
			case Atom(v)                 => v.toString
			case Addition(l, r)          => formatter(l, r, "+")
			case Subtraction(l, r)       => formatter(l, r, "-")
			case Multiplication(l, r)    => formatter(l, r, "*")
			case Division(l, r)          => formatter(l, r, "/")
			case Exponentiation(l, r)    => formatter(l, r, "^")
			case NaturalLogarithm(inner) => "(ln(" + show(inner) + "))"
		}
	}

	/* ----------------------------------- 1.2 ---------------------------------- */
	/**
	  * Computes the symbolic derivative of an expression "f" with respect to
	  * a given variable "a" (Atom). All other Atoms are treated as constants.
	  * This uses pattern matching to apply standard differentiation rules:
	  *	 - Constant rule:  (c)' -> 0
	  *	 - Variable rule:  (a)' -> 1; (b ≠ a)' -> 0
	  *	 - Sum rule:       (f + g)' -> f' + g'
	  *	 - Difference:     (f - g)' -> f' - g'
	  *	 - Product:        (f * g)' -> f' * g + f * g'
	  *	 - Quotient:       (f / g)' -> (f' * g - f * g') / (g * g)
	  *	 - Exponentiation: (f^g)' -> f^g * ((f' * g) / f + g' * ln(f))
	  *	 - Natural log:    (ln f)' -> f' / f
	  *
	  * @param f The Expression to differentiate.
	  * @param a The Atom variable with respect to which differentiation is performed.
	  * @return a new Expression representing d(f) / d(a).
	  */
	def derivate(f: Expression, a: Atom): Expression = {
		def derivateAlias(sub: Expression): Expression = derivate(sub, a)

		f match {
			case Number(_) => Number(0) // Derivative of a constant is zero

			case atom @ Atom(_) =>
				// If the Atom matches the differentiation variable, derivative is 1; otherwise 0
				if (atom == a) Number(1) else Number(0)

			case Addition(e_1, e_2) =>
				Addition(derivateAlias(e_1), derivateAlias(e_2))

			case Subtraction(e_1, e_2) =>
				Subtraction(derivateAlias(e_1), derivateAlias(e_2))

			case Multiplication(e_1, e_2) =>
				Addition(
					Multiplication(derivateAlias(e_1), e_2),
					Multiplication(e_1, derivateAlias(e_2))
				)

			case Division(e_1, e_2) =>
				Division(
					Subtraction(
						Multiplication(derivateAlias(e_1), e_2),
						Multiplication(e_1, derivateAlias(e_2))
					),
					Multiplication(e_2, e_2)
				)

			case Exponentiation(e_1, e_2) =>
				Multiplication(
					Exponentiation(e_1, e_2),
					Addition(
						Division(Multiplication(derivateAlias(e_1), e_2), e_1),
						Multiplication(derivateAlias(e_2), NaturalLogarithm(e_1))
					)
				)

			case NaturalLogarithm(inner) =>
				Division(derivateAlias(inner), inner)
		}
	}

	/* ----------------------------------- 1.3 ---------------------------------- */
	/**
	  * Evaluates a symbolic expression "f" by substituting the Atom "a" with
	  * the numeric value "v" (Double). Assumes "f" contains at most one Atom type
	  * (the variable "a"), and other Atoms are treated as that same variable if they match.
	  * Uses standard floating‐point arithmetic:
	  *	 - Number(d) -> d
	  *	 - Atom(a) -> v
	  *	 - Addition, Subtraction, Multiplication, Division, Exponentiation: evaluate recursively
	  *	 - NaturalLogarithm: math.log(...)
	  *
	  * @param f The Expression to evaluate.
	  * @param a The Atom variable that is replaced by "v".
	  * @param v The Double value used for substitution of "a".
	  * @return a Double result of evaluating f(v).
	  */
	def evaluate(f: Expression, a: Atom, v: Double): Double = {
		def alias(expr: Expression): Double = evaluate(expr, a, v)

		f match {
			case Number(value)           => value
			case Atom(_)                 => v
			case Addition(e_1, e_2)        => alias(e_1) + alias(e_2)
			case Subtraction(e_1, e_2)     => alias(e_1) - alias(e_2)
			case Multiplication(e_1, e_2)  => alias(e_1) * alias(e_2)
			case Division(e_1, e_2)        => alias(e_1) / alias(e_2)
			case Exponentiation(e_1, e_2)  => math.pow(alias(e_1), alias(e_2))
			case NaturalLogarithm(e)     => math.log(alias(e))
		}
	}

	/* ----------------------------------- 1.4 ---------------------------------- */
	/**
	  * Simplifies an expression by eliminating unnecessary zeros and ones:
	  *	 - 0 + e -> e
	  *	 - e + 0 -> e
	  *	 - 0 - e -> -e (represented as 0 - e -> multiply(-1, e))
	  *	 - e - 0 -> e
	  *	 - 0 * e -> 0
	  *	 - 1 * e -> e
	  *	 - e * 0 -> 0
	  *	 - e * 1 -> e
	  *	 - 0 / e -> 0
	  *	 - e / 1 -> e
	  *	 - 1 ^ e -> 1
	  *	 - 0 ^ e -> 0
	  *	 - e ^ 1 -> e
	  *	 - e ^ 0 -> 1
	  *	 - ln(1) -> 0
	  *	 - Otherwise, recursively clean subexpressions.
	  *
	  * @param f The Expression to simplify ("clean").
	  * @return a simplified Expression with redundant constants removed.
	  */
	def clean(f: Expression): Expression = f match {
		case Number(v) => Number(v)
		case Atom(v) => Atom(v)

		case Addition(e_1, e_2) => clean(e_1) match {
			case Number(0) => clean(e_2)
			case _ => clean(e_2) match {
				case Number(0) => clean(e_1)
				case _ => Addition(clean(e_1), clean(e_2))
			}
		}

		case Subtraction(e_1, e_2) => clean(e_1) match {
			case Number(0) => // 0 - e_2 -> -e_2 represented as (−1) * e_2
				Multiplication(Number(-1), clean(e_2))
			case _ => clean(e_2) match {
				case Number(0) => clean(e_1)
				case _ => Subtraction(clean(e_1), clean(e_2))
			}
		}

		case Multiplication(e_1, e_2) => clean(e_1) match {
			case Number(0) => Number(0)
			case Number(1) => clean(e_2)
			case _ => clean(e_2) match {
				case Number(0) => Number(0)
				case Number(1) => clean(e_1)
				case _ => Multiplication(clean(e_1), clean(e_2))
			}
		}

		case Division(e_1, e_2) => clean(e_1) match {
			case Number(0) => Number(0)
			case _ => clean(e_2) match {
				case Number(1) => clean(e_1)
				case _ => Division(clean(e_1), clean(e_2))
			}
		}

		case Exponentiation(e_1, e_2) => clean(e_1) match {
			case Number(1) => Number(1)
			case Number(0) => Number(0)
			case _ => clean(e_2) match {
				case Number(1) => clean(e_1)
				case Number(0) => Number(1)
				case _ => Exponentiation(clean(e_1), clean(e_2))
			}
		}

		case NaturalLogarithm(inner) => clean(inner) match {
			case Number(1) => Number(0)
			case _ => NaturalLogarithm(clean(inner))
		}
	}

	/* ----------------------------------- 1.5 ---------------------------------- */
	/**
	  * Uses the Newton–Raphson method to find a root of the function "f(x) = 0",
	  * starting from an initial guess "x". Recursively iterates until "good_approximation"
	  * returns true for the current guess.
	  *
	  * Newton's iteration step:
	  *	 x_{ n + 1 } = x_n − f(x_n) / f'(x_n)
	  *
	  * This function does not perform any explicit divergence checks; it assumes
	  * that "good_approximation" will eventually return true if a root is found.
	  *
	  * @param f                  The Expression representing f(x).
	  * @param a                  The Atom (variable) with respect to which f is defined.
	  * @param x                  The current guess (Double) for the root.
	  * @param good_approximation A predicate (Expression, Atom, Double) => Boolean
	  *                           that returns true if |f(x)| is sufficiently close to zero.
	  * @return                   a Double approximate root r such that f(r) ≈ 0.
	  */
	def Newton_method(
		f: Expression,
		a: Atom,
		x: Double,
		good_approximation: (Expression, Atom, Double) => Boolean
	): Double = {
		if (good_approximation(f, a, x)) x
		else {
			val primeF = derivate(f, a)
			val newX = x - evaluate(f, a, x) / evaluate(primeF, a, x)
			Newton_method(f, a, newX, good_approximation)
		}
	}

	/**
	  * A sample "good approximation" predicate that deems the root search complete
	  * when |f(x)| < 0.001.
	  *
	  * @param f The Expression representing f(x).
	  * @param a The Atom (variable) in f.
	  * @param value	The current guess (Double) for x.
	  * @return true if |f(value)| < 0.001, false otherwise.
	  */
	def good_approximation(f: Expression, a: Atom, value: Double): Boolean =
		math.abs(evaluate(f, a, value)) < 0.001
}
