package network_polarization_simulator {
	/* -------------------------------------------------------------------------- */
	/*                                     2.1                                    */
	/* -------------------------------------------------------------------------- */

	/* ---------------------------------- 2.1.1 --------------------------------- */

	type DistributionValues = Vector[Double]

	/*
		Pi_k.lenght = k, 0 <= Pi_k(i) <= 1, 0 <= i <= k - 1
		Pi_k.sum == 1
	*/
	type Frecuency = Vector[Double]

	/*
		(Pi, dv), Pi.lenght = k and dv.lenght = k
	*/
	type Distribution = (Frecuency, DistributionValues)


	type PolarizationValue = Distribution => Double


	def min_p(f: Double => Double, min: Double, max: Double,
				precision: Double): Double = {
		val left = min
		val right = max
		if (max - min < precision)
			(max + min) / 2
		else {
			val first_third = left + (right - left) / 3
			val last_third = right - (right - left) / 3

			if (f(first_third) > f(last_third))
				min_p(f, first_third, max, precision)
			else
				min_p(f, min, last_third, precision)
		}
	}

	def rho_CMT_generator(alpha: Double, beta: Double): PolarizationValue = {
		(distribution: Distribution) => {
			val (frequencies, values) = distribution // (Pi, dv)
			val lenght = values.length // or distribution._2.length

			def rho_aux(p: Double): Double = {
				(for {
					i <- 0 until lenght
				} yield math.pow(frequencies(i), alpha) * math.pow(
						math.abs(values(i) - p), beta)).sum
			}

			val p = min_p(rho_aux, 0.0, 1.0, 0.1)
			rho_aux(p)
		}
	}

	def normalize(rho_cmt: PolarizationValue): PolarizationValue = {
		distribution => {
			val max_polarization_distribution: Distribution = (Vector(0.5, 0.0, 0.0, 0.0, 0.5),
																Vector(0.0, 0.25, 0.5, 0.75, 1.0))
			val max_polarization = rho_cmt(max_polarization_distribution)

			val polarization_value = rho_cmt(distribution)
			if (max_polarization != 0)
				polarization_value / max_polarization
			else
				0.0
		}
	}

	/* ---------------------------------- 2.1.1 --------------------------------- */


	/*
		If b : SpecificBelief
		Agent number = b.length
		0 <= b(i) <= 1
		b(i) is the belief of the agent i in the preposition p
	*/
	type SpecificBelief = Vector[Double]

	/*
		If gb : GenericBelief, gb(n) = b
	*/
	type GenericBelief = Int => SpecificBelief

	/*
		If rho: AgentsPoolMeasure, sb: SpecificBelief and d: DistributionValues
		rho(sb, d) is the polarization of the agents
	*/
	type AgentsPoolMeasure = (SpecificBelief, DistributionValues) => Double


	def uniform_belief(agents: Int): SpecificBelief = {
		Vector.tabulate(agents)((i: Int) =>
									(i + 1).toDouble / agents.toDouble)
	}

	/*
		Hall of agents has a belief decreasing from 0.25 , and
		half has a belief increasing from 0.75 , all by the given step
	*/
	def midly_belief(agents: Int): SpecificBelief = {
		val middle = agents / 2

		Vector.tabulate(agents)((i: Int) =>
				if (i < middle)
					math.max(0.25 - 0.01 * (middle - i - 1), 0)
				else
					math.min(0.75 - 0.01 * (middle - i), 1)
			)
	}

	def extreme_belief(agents: Int): SpecificBelief = {
		val middle = agents / 2

		Vector.tabulate(agents)((i: Int) =>
				if (i < middle)
					0.0
				else
					1.0
			)
	}

	def triple_belief(agents: Int): SpecificBelief = {
		val first_third = agents / 3
		val second_third = first_third * 2

		Vector.tabulate(agents)((i: Int) =>
				if (i <= first_third)
					0.0
				else if (i <= second_third)
					1.0
				else
					0.5
			)
	}

	def consensus_belief(b: Double, agents: Int): SpecificBelief = {
		Vector.tabulate(agents)((i: Int) => b)
	}


	// def rho(alpha: Double, beta: Double): AgentsPoolMeasure = {
	// 	val rho_CMT = rho_CMT_generator(alpha, beta)

	// 	(specific_belief: SpecificBelief, distribution_values: DistributionValues) => {
	// 		val belief_number = specific_belief.length
	// 		val k = distribution_values.length

	// 		val intervals = for (i <- 0 until k) yield {
	// 			if (i == 0)
	// 				(distribution_values(i), (distribution_values(i) + distribution_values(i + 1)) / 2)
	// 			else if (i == k - 1)
	// 				((distribution_values(i - 1) + distribution_values(i)) / 2, distribution_values(i))
	// 			else
	// 				((distribution_values(i - 1) + distribution_values(i)) / 2, (distribution_values(i) + distribution_values(i + 1)) / 2)
	// 		}

	// 		val frequencies = for {
	// 			(low, high) <- intervals
	// 			count = (for (belief <- specific_belief
	// 								if low < belief && belief < high)
	// 									yield 1.0).sum
	// 		} yield count / belief_number
	// 	}
	// }
}
