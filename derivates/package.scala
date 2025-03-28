package derivates {


	def derivada(f: Double => Double): Double => Double = {
		def f_prime(x_0: Double): Double = {
			val h = 0.1

			(f(x_0 - 2 * h) - 8 * f(x_0 - h) + 8 * f(x_0 + h) - f(x_0 + 2 * h)) / (12 * h)
		}

		f_prime
	}
}