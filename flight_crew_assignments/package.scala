package flight_crew_assignments {
	type Match = (Int, Int)
	type Matching = List[Match]
	type Preferences = Vector[Vector[Int]]

	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */

	def match_pilot(pilot: Int, number_copilots: Int): Matching = {
		for {
			i <- (1 to number_copilots).toList
		} yield (pilot, i)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */

	def math_pilots_and_copilots(pilots_and_copilots_number: Int): List[Matching] = {
		for {
			i <- (1 to pilots_and_copilots_number).toList
		} yield (match_pilot(i, pilots_and_copilots_number))
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.3                                    */
	/* -------------------------------------------------------------------------- */

	def possible_matchings(pilots_and_copilots_number: Int): List[Matching] = {
		def generate_combinations(combinations: List[Matching], rest: List[Matching]): List[Matching] = {
			if (rest.isEmpty)
				combinations
			else {
				val new_combination = for {
					combination <- combinations
					another_match <- rest.head
				} yield (combination :+ another_match)

				generate_combinations(new_combination, rest.tail)
			}
		}

		val matches = math_pilots_and_copilots(pilots_and_copilots_number)

		val initial_combination = for {
			match_ <- matches.head
		} yield List(match_)

		generate_combinations(initial_combination, matches.tail)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.4                                    */
	/* -------------------------------------------------------------------------- */

	def filter_valid_matchings(pilots_and_copilots_number: Int): List[Matching] = {
		def is_valid(matching: Matching): Boolean = {
			(for {
				match_ <- matching
			} yield match_._2).toSet.size == pilots_and_copilots_number
		}

		val possible_matchings_ = possible_matchings(pilots_and_copilots_number)

		for {
			possible_matching <- possible_matchings_
			if (is_valid(possible_matching))
		} yield possible_matching
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.5                                    */
	/* -------------------------------------------------------------------------- */

	def weighted_matchings(pilots_and_copilots_number: Int, pilot_preferences: Preferences,
							copilot_preferences: Preferences): List[(Matching, Int)] = {
		val matchings = filter_valid_matchings(pilots_and_copilots_number)

		for {
			matching <- matchings
			weight = (for {
					match_ <- matching
					pilot = match_._1 - 1
					copilot = match_._2 - 1
					weight = pilot_preferences(pilot)(copilot) + copilot_preferences(copilot)(pilot)
			} yield weight).sum
		} yield (matching, weight)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.6                                    */
	/* -------------------------------------------------------------------------- */

	def best_matching(pilots_and_copilots_number: Int, pilot_preferences: Preferences,
							copilot_preferences: Preferences): (Matching, Int) = {
		val weighted_matchings_ = weighted_matchings(pilots_and_copilots_number, pilot_preferences,
														copilot_preferences)

		def aux(best: (Matching, Int), rest: List[(Matching, Int)]): (Matching, Int) = {
			if (rest.isEmpty)
				best
			else {
				val matching = rest.head
				if (best._2 < matching._2)
					aux(matching, rest.tail)
				else
					aux(best, rest.tail)
			}
		}
		aux(weighted_matchings_.head, weighted_matchings_.tail)
	}
}