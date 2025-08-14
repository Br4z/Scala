import flight_crew_assignments.{Preferences,
                                match_pilot,
                                match_pilots_and_copilots,
                                get_possible_matchings,
                                get_valid_matchings,
                                get_weighted_matchings,
                                best_matching}


object Main extends App {
	val pilot_preferences: Preferences = Vector(
		Vector(2, 3, 1, 1),
		Vector(1, 1, 4, 3),
		Vector(1, 2, 3, 4),
		Vector(2, 3, 2, 1)
	)
	val copilot_preferences: Preferences = Vector(
		Vector(4, 1, 3, 2),
		Vector(4, 2, 3, 1),
		Vector(1, 1, 1, 4),
		Vector(3, 2, 3, 3)
	)

	println("--- Testing match_pilot ---")
	println(s"Pilot 2, n=4: ${match_pilot(2, 4)}")
	println(s"Pilot 3, n=4: ${match_pilot(3, 4)}")
	println("-" * 40)

	println("\n--- Testing match_pilots_and_copilots ---")
	println(s"n=2: ${match_pilots_and_copilots(2)}")
	println("-" * 40)

	println("\n--- Testing get_possible_matchings ---")
	println(s"n=2: ${get_possible_matchings(2)}")
	println("-" * 40)

	println("\n--- Testing get_valid_matchings (valid pairings) ---")
	println(s"n=2: ${get_valid_matchings(2)}")
	println(s"n=3: ${get_valid_matchings(3)}")
	println("-" * 40)

	println("\n--- Testing get_weighted_matchings ---")
	val weighted_2 = get_weighted_matchings(2, pilot_preferences, copilot_preferences)
	println(s"n=2: $weighted_2")

	val weighted_3 = get_weighted_matchings(3, pilot_preferences, copilot_preferences)
	println(s"n=3: $weighted_3")
	println("-" * 40)


	println("\n--- Testing best_matching ---")
	val best_2 = best_matching(2, pilot_preferences, copilot_preferences)
	println(s"Best for n=2: $best_2")

	val best_3 = best_matching(3, pilot_preferences, copilot_preferences)
	println(s"Best for n=3: $best_3")

	val best_4 = best_matching(4, pilot_preferences, copilot_preferences)
	println(s"Best for n=4: $best_4")
	println("-" * 40)
}
