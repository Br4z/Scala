import adder_with_logic_gates.{not_chip, and_chip, or_chip, half_adder, full_adder, adder}


object Main extends App {
	def print_bits(label: String, bits: List[Int]): Unit =
		println(s"$label: ${bits.mkString}")

	print_bits("NOT 1", not_chip(List(1)))
	print_bits("AND 1,0", and_chip(List(1, 0)))
	print_bits("OR 1,0", or_chip(List(1, 0)))

	// Test half_adder
	print_bits("Half Adder (1,1)", half_adder(List(1, 1))) // carry, sum

	// Test full_adder
	print_bits("Full Adder (1,1,1)", full_adder(List(1, 1, 1))) // carry_out, sum

	// Test 4-bit adder
	val n = 4
	val a = List(1, 0, 1, 1) // 1011 = 11
	val b = List(1, 1, 0, 1) // 1101 = 13
	val sum = adder(n)(a ++ b) // should be 24 = 11000
	print_bits("4-bit Adder (1011 + 1101)", sum) // carry + sum bits
}
