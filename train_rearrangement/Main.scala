import train_rearrangement._


object Main extends App {
	// Initial setup
	val t_1: Train = List("W1", "W2", "W3", "W4", "W5", "W6")
	val t_2: Train = List("W5", "W1", "W3", "W4", "W2", "W6")

	println("Original train: " + t_1)
	println("Target train: " + t_2)
	println("\nTesting individual train operations")

	// Test remove and extract operations
	println("\nRemove 2 wagons from left: " + remove_from_left(t_1, 2))
	println("Extract 2 wagons from left: " + extract_from_left(t_1, 2))
	println("Remove 2 wagons from right: " + remove_from_right(t_1, 2))
	println("Extract 2 wagons from right: " + extract_from_right(t_1, 2))

	// Test insert operations
	val wagons_to_insert = List("X1", "X2")
	println("\nInsert wagons at left: " + insert_into_left(wagons_to_insert, t_1))
	println("Insert wagons at right: " + insert_into_right(wagons_to_insert, t_1))

	println("\nTesting state operations")

	// Test single movements
	val initial_state: State = (t_1, Nil, Nil)
	println("\nInitial state: " + initial_state)

	val state_after_one = apply_movement(initial_state, One(2))
	println("After moving 2 wagons to track One: " + state_after_one)

	val state_after_two = apply_movement(state_after_one, Two(2))
	println("After moving 2 wagons to track Two: " + state_after_two)

	println("\nTesting ordering operations")

	// Test order_element
	val (new_principal, single_maneuver) = order_element(t_1, "W5")
	println("\nAfter ordering single element (W5):")
	println("New principal: " + new_principal)
	println("Maneuver used: " + single_maneuver)

	// Test complete rearrangement
	println("\nComplete train rearrangement")
	val optimized_maneuver = define_maneuver(t_1, t_2)
	println("\nComplete maneuver: " + optimized_maneuver)

	val states = apply_maneuver(initial_state, optimized_maneuver)
	println("\nAll intermediate states:")
	states.zipWithIndex.foreach { case (state, index) =>
		println(s"Step ${index + 1}: $state")
	}

	// Verify final state
	val final_state = states.last
	println("\nFinal state achieved: " + final_state)
	println("Target train achieved: " + (final_state._1 == t_2))
}
