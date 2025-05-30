import train_rearrangement.*


object Main extends App {
	val t_1: Train = List("W1", "W2", "W3", "W4", "W5", "W6")
	val t_2: Train = List("W5", "W1", "W3", "W4", "W2", "W6")

	val original_state = (t_1, Nil, Nil)
	val maneuver = define_optimized_maneuver(t_1, t_2)

	val states = apply_maneuver(original_state, maneuver)

	println(states)
}
