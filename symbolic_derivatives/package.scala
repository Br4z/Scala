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

	def show(e: Expression): String = {
		def formatter(e_1: Expression, e_2: Expression, operator: String) = "(" + show(e_1) + " " +
			operator + " " + show(e_2) + ")"
		e match {
			case Number(value) => value.toString
			case Atom(value) => value.toString
			case Addition(e_1, e_2) => formatter(e_1, e_2, "+")
			case Subtraction(e_1, e_2) => formatter(e_1, e_2, "-")
			case Multiplication(e_1, e_2) => formatter(e_1, e_2, "*")
			case Division(e_1, e_2) => formatter(e_1, e_2, "/")
			case Exponentiation(e_1, e_2) => formatter(e_1, e_2, "^")
			case NaturalLogarithm(e) => "(ln(" + show(e) + "))"
		}
	}

	/* ----------------------------------- 1.2 ---------------------------------- */

	def derivate(f: Expression, a: Atom): Expression = {
		def derivate_alias(f: Expression): Expression = derivate(f, a)
		f match {
			case Number(value) => Number(0)
			case Atom(value) =>
				if (Atom(value) == a)
					Number(1)
				else
					Number(0)
			case Addition(e_1, e_2) =>
				Addition(derivate_alias(e_1), derivate_alias(e_2))
			case Subtraction(e_1, e_2) =>
				Subtraction(derivate_alias(e_1), derivate_alias(e_2))
			case Multiplication(e_1, e_2) =>
				Addition(Multiplication(derivate_alias(e_1), e_2), Multiplication(e_1, derivate_alias(e_2)))
			case Division(e_1, e_2) =>
				Division(Subtraction(Multiplication(derivate_alias(e_1), e_2),
					Multiplication(e_1, derivate_alias(e_2))), Multiplication(e_2, e_2))
			case Exponentiation(e_1, e_2) =>
				Multiplication(Exponentiation(e_1, e_2), Addition(Division(Multiplication(derivate_alias(e_1), e_2), e_1),
					Multiplication(derivate_alias(e_2), NaturalLogarithm(e_1))))
			case NaturalLogarithm(e) =>
				Division(derivate_alias(e), e)
		}
	}

	/* ----------------------------------- 1.3 ---------------------------------- */

	def evaluate(f: Expression, a: Atom, v: Double): Double = {
		def evaluate_alias(e: Expression) = evaluate(e, a, v)
		f match {
			case Number(value) => value
			case Atom(value) => v
			case Addition(e_1, e_2) =>
				evaluate_alias(e_1) + evaluate_alias(e_2)
			case Subtraction(e_1, e_2) =>
				evaluate_alias(e_1) - evaluate_alias(e_2)
			case Multiplication(e_1, e_2) =>
				evaluate_alias(e_1) * evaluate_alias(e_2)
			case Division(e_1, e_2) =>
				evaluate_alias(e_1) / evaluate_alias(e_2)
			case Exponentiation(e_1, e_2) =>
				math.pow(evaluate_alias(e_1), evaluate_alias(e_2))
			case NaturalLogarithm(e) =>
				math.log(evaluate_alias(e))
		}
	}

	/* ----------------------------------- 1.4 ---------------------------------- */

	def clean(f: Expression): Expression = f match {
		case Number(value) => Number(value)
		case Atom(value) => Atom(value)
		case Addition(e_1, e_2) => clean(e_1) match {
			case Number(0) => clean(e_2)
			case _ => clean(e_2) match {
				case Number(0) => clean(e_1)
				case _ => Addition(clean(e_1), clean(e_2))
			}
		}
		case Subtraction(e_1, e_2) => clean(e_1) match {
			case Number(0) => clean(Multiplication(Number(-1), e_2))
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
		case NaturalLogarithm(e) => clean(e) match {
			case Number(1) => Number(0)
			case _ => NaturalLogarithm(clean(e))
		}
	}

	/* ----------------------------------- 1.5 ---------------------------------- */

	def Newton_method(f: Expression, a: Atom, x: Double,
					good_approximation: (Expression, Atom, Double) => Boolean): Double = {
		if (good_approximation(f, a, x))
			x
		else {
			val prime_f = derivate(f, a)
			val new_x = x - evaluate(f, a, x) / evaluate(prime_f, a, x)
			Newton_method(f, a, new_x, good_approximation)
		}
	}

	def good_approximation(f: Expression, a: Atom, value: Double): Boolean = evaluate(f, a, value) < 0.001
}
