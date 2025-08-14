package adder_with_logic_gates {
	type Chip = List[Int] => List[Int]

	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */

	/* ---------------------------------- 1.1.1 --------------------------------- */
	/**
	  * Creates a Chip from a unary logic function.
	  * This higher-order function wraps a function that operates on a single integer (bit)
	  * into a standard Chip that operates on a List[Int].
	  *
	  * @param function The unary function `Int => Int` to be converted into a Chip.
	  * It should take one bit and return one bit.
	  * @return a `Chip` that takes a list with one bit and returns a list with the result.
	  */
	def create_unary_chip(function: Int => Int): Chip = {
		(list: List[Int]) => {
			List(function(list.head))
		}
	}

	/* ---------------------------------- 1.1.2 --------------------------------- */
	/**
	  * Creates a Chip from a binary logic function.
	  * This higher-order function wraps a function that operates on two integers (bits)
	  * into a standard Chip that operates on a List[Int].
	  *
	  * @param function The binary function `(Int, Int) => Int` to be converted into a Chip.
	  * It should take two bits and return one bit.
	  * @return a `Chip` that takes a list with two bits and returns a list with the result.
	  */
	def create_binary_chip(function: (Int, Int) => Int): Chip = {
		(list: List[Int]) => {
			List(function(list.head, list.tail.head))
		}
	}

	/* -------------------------------------------------------------------------- */

	val not_chip = create_unary_chip((x: Int) => 1 - x)
	val and_chip = create_binary_chip((x: Int, y: Int) => x * y)
	val or_chip = create_binary_chip((x: Int, y: Int) => x + y - x * y)

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Constructs a half-adder Chip.
	  * A half-adder simulates the addition of two single bits, producing a sum and a carry.
	  * It is built by combining the basic `and_chip`, `or_chip`, and `not_chip`.
	  *
	  * Input: A `List[Int]` containing two bits `[a, b]`.
	  * Output: A `List[Int]` containing two bits `[carry, sum]`.
	  *
	  * @return a `Chip` representing the half-adder circuit.
	  */
	def half_adder: Chip = {
		(list: List[Int]) => {
			val a = List(list.head)
			val b = list.tail

			val a_or_b = or_chip(a ++ b)
			val c = and_chip(a ++ b)
			val not_c = not_chip(c)

			val s = and_chip(a_or_b ++ not_c)

			c ++ s
		}
	}

	val ha = half_adder

	/* -------------------------------------------------------------------------- */
	/*                                     1.3                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Constructs a full-adder Chip.
	  * A full-adder simulates the addition of two bits and an incoming carry bit,
	  * producing a sum and a new carry-out bit. It is constructed using two
	  * `half_adder` chips and one `or_chip`.
	  *
	  * Input: A `List[Int]` containing three bits `[a, b, carry_in]`.
	  * Output: A `List[Int]` containing two bits `[carry_out, sum]`.
	  *
	  * @return a `Chip` representing the full-adder circuit.
	  */
	def full_adder: Chip = {
		(list: List[Int]) => {
			val a = List(list.head)
			val b_1 = List(list.tail.head)
			val cin_1 = list.tail.tail

			val ha_b_cin = ha(b_1 ++ cin_1)
			val b_2 = ha_b_cin.tail
			val cin_2 = List(ha_b_cin.head)

			val ha_a_b_2 = ha(a ++ b_2)
			val sum = ha_a_b_2.tail

			val b_3 = List(ha_a_b_2.head)

			val cout = or_chip(b_3 ++ cin_2)

			cout ++ sum
		}
	}

	val fa = full_adder

	/* -------------------------------------------------------------------------- */
	/*                                     1.4                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Constructs an n-bit adder Chip.
	  * This higher-order function creates a Chip capable of adding two n-bit binary numbers.
	  * It works by chaining `n` full-adders, passing the carry from one to the next.
	  * The implementation uses a tail-recursive helper function to process bits from
	  * right to left (least significant to most significant).
	  *
	  * @param n The number of bits for each of the two numbers to be added (n >= 1).
	  * @return a `Chip` that takes a list of `2n` bits (n for the first number, n for the second)
	  * and returns a list of `n + 1` bits (the final carry and the n-bit sum).
	  */
	def adder(n: Int): Chip = {
		def aux(number_1_bits: List[Int], number_2_bits: List[Int], actual_digit: Int, carry: Int,
				result: List[Int]): List[Int] = {
			if (actual_digit == 0)
				carry :: result
			else {
				val sum_result = fa(List(number_1_bits(actual_digit - 1), number_2_bits(actual_digit - 1), carry))
				val new_carry = sum_result.head
				val sum = sum_result.tail.head
				aux(number_1_bits, number_2_bits, actual_digit - 1, new_carry, sum :: result)
			}
		}

		(list: List[Int]) => {
			val number_1 = list.take(n)
			val number_2 = list.drop(n)
			aux(number_1, number_2, n, 0, Nil)
		}
	}
}
