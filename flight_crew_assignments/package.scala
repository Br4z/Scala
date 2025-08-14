package flight_crew_assignments {
	type Match = (Int, Int)
	type Matching = List[Match]
	type Preferences = Vector[Vector[Int]]

	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Generates a list of all possible pairings for a single pilot.
	  *
	  * @param i The pilot's number (1-based index).
	  * @param n The total number of copilots.
	  * @return a list of tuples, where each tuple is a possible match for pilot `i`.
	  */
	def match_pilot(i: Int, n: Int): Matching = {
		for (j <- (1 to n).toList) yield (i, j)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Generates a list of all possible pairings for each pilot.
	  *
	  * @param n The total number of pilots and copilots.
	  * @return a list of lists, where each inner list contains all possible
	  * pairings for a single pilot.
	  */
	def match_pilots_and_copilots(n: Int): List[Matching] = {
		for (i <- (1 to n).toList) yield match_pilot(i, n)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.3                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Generates all possible combinations of pairings, without considering validity.
	  * This function creates the Cartesian product of all individual pilot pairings.
	  * The result will include invalid matchings where a copilot is assigned to multiple pilots.
	  *
	  * @param n The total number of pilots and copilots.
	  * @return a list of all theoretically possible matchings.
	  */
	def get_possible_matchings(n: Int): List[Matching] = {
		val all_pilots_options = match_pilots_and_copilots(n)

		/**
		  * A recursive helper to build the combinations.
		  * @param remainingPilots A list of lists, where each inner list is the set of
		  * options for a pilot who has not yet been assigned.
		  * @return a list of all generated combinations.
		  */
		def combine(remaining_pilots: List[Matching]): List[Matching] = {
			remaining_pilots match {
				// Base case: no pilots to assign
				case Nil => List(Nil)
				case current_pilots :: other_pilots =>
					for {
						rest_combination <- combine(other_pilots)
						current_match <- current_pilots
					} yield current_match :: rest_combination
			}
		}

		combine(all_pilots_options)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.4                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Filters the list of all possible matchings to find only the valid ones.
	  * A matching is valid if and only if every copilot is assigned to exactly one pilot.
	  *
	  * @param n The total number of pilots and copilots.
	  * @return a list of valid `Matching`s.
	  */
	def get_valid_matchings(n: Int): List[Matching] = {
		val possible_matchings = get_possible_matchings(n)

			/**
			  * Checks if a given matching is valid.
			  * It does this by extracting all copilot numbers, converting them to a Set to
			  * remove duplicates, and checking if the size of the set is equal to n.
			  *
			  * @param m The matching to validate.
			  * @return `true` if the matching is valid, `false` otherwise.
			  */
			def is_valid(m: Matching): Boolean = {
				val assigned_copilots = for (pair <- m) yield pair._2
				assigned_copilots.toSet.size == n
			}

			for (p <- possible_matchings if is_valid(p)) yield p
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.5                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Calculates the total preference weight for every valid matching.
	  * The weight for a single pair (pilot i, copilot j) is P(i,j) * N(j,i).
	  *
	  * @param n The total number of pilots and copilots.
	  * @param pilot_preferences The preference matrix for pilots.
	  * @param copilot_preferences The preference matrix for copilots.
	  * @return a list of tuples, where each tuple contains a valid matching and its total weight.
	  */
	def get_weighted_matchings(n: Int, pilot_preferences: Preferences,
							copilot_preferences: Preferences): List[(Matching, Int)] = {
		val valid_matchings = get_valid_matchings(n)


		for (matching <- valid_matchings) yield {
			val total_weight = (for {
				(pilot, copilot) <- matching
				// Adjust for 0-based indexing of the vectors
				p_i = pilot - 1
				c_j = copilot - 1
			} yield pilot_preferences(p_i)(c_j) * copilot_preferences(c_j)(p_i)).sum // Correctly multiply preferences

			(matching, total_weight)
		}
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.6                                    */
	/* -------------------------------------------------------------------------- */
	/**
	  * Finds the single best matching with the highest total weight.
	  *
	  * @param n The total number of pilots and copilots.
	  * @param pilot_preferences The preference matrix for pilots.
	  * @param copilot_preferences The preference matrix for copilots.
	  * @return a single tuple containing the best matching and its weight.
	  */
	def best_matching(n: Int, pilot_preferences: Preferences,
							copilot_preferences: Preferences): (Matching, Int) = {
		val weighted_matchings = get_weighted_matchings(n, pilot_preferences, copilot_preferences)
		weighted_matchings.maxBy(_._2)
	}
}
