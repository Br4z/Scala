package adder_with_logic_gates {
	type Chip = List[Int] => List[Int]

	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */

	/* ---------------------------------- 1.1.1 --------------------------------- */

	def create_unary_chip(function: Int => Int): Chip = {
		(list: List[Int]) => {
			List(function(list.head))
		}
	}

	/* ---------------------------------- 1.1.2 --------------------------------- */

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

	def hald_adder: Chip = {
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

	val ha = hald_adder

	/* -------------------------------------------------------------------------- */
	/*                                     1.3                                    */
	/* -------------------------------------------------------------------------- */

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
