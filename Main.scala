import train_rearrangement.*




object Main extends App {
	val t_1: Train = List("W5", "W4", "W3", "W2", "W1")
	val t_2: Train = List("W5", "W4", "W3", "W2", "W1")

	val original_state = (t_1, Nil, Nil)
	val manuver = definy_maneuver(t_1, t_2)

	val states = apply_maneuver(original_state, manuver)

	println(states)
}
