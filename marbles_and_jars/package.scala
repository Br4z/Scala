package marbles_and_jars {
	type Frasco = (Int, Int)
	type Distr = List[Frasco]

	def canicasPosiblesFrasco(f : Int, c : Int) : List[Frasco] = {
		for {
			actualMarble <- (0 to c).toList // 0 until c retorna un vector, pero necesitamos que finalmente retorne una lista de
			// parejas (int, int)
		} yield (f, actualMarble)
	}

	def canicasPorFrasco(n : Int, c : Int) : List[Distr] = {
		for {
			actualBottle <- (1 to n).toList // Hago la conversion por la mismas razón que la de arriba
		} yield canicasPosiblesFrasco(actualBottle, c)
	}

	def mezclarLCanicas(lc : List[Distr]) : List[Distr] = {
		def partialCombination(combinations : List[Distr], restDistribution : List[Distr]) : List[Distr] = {
			if(restDistribution.isEmpty) combinations
			else {
				val actualCombination = for {
					combination <- combinations
					anotherBottle <- restDistribution.head
				} yield (combination :+ anotherBottle)
				partialCombination(actualCombination, restDistribution.tail)
			}
		}
		val initialCombination = for(firstBottle <- lc.head) yield List(firstBottle) // Necesario para terminar con List[List[(Int, Int)]], es decir,
		// listas de frascos o en ultima instancia una distribución
		partialCombination(initialCombination, lc.tail)
	}


	def distribucion(m : Int, n : Int, c : Int) : List[Distr] = {
		val allDistributions = mezclarLCanicas(canicasPorFrasco(n, c))

		def isSolution(distribution : Distr) : Boolean = {
			(for {
				bottle <- distribution
			} yield bottle._2).sum == m
		}

		for {
			distribution <- allDistributions

			if isSolution(distribution)
		} yield distribution
	}

	def agrupacion(m : Int) : List[List[Int]] = {
		val possibleSolutions = distribucion(m, m, m)

		def clearSolution(solution : Distr) : Distr = {
			for {
				bottle <- solution

				if bottle._2 != 0
			} yield bottle
		}

		val cleanSolutions = for {
			solution <- possibleSolutions

		} yield clearSolution(solution)

		def extractNumbers(solution : Distr) : List[Int] = {
			for {
				bottle <- solution
			} yield bottle._2
		}

		val cleanLists = (for {
			solution1 <- cleanSolutions
			solution2 <- cleanSolutions
		} yield extractNumbers(solution1)).distinct

		val possibleLists = for {
			list <- cleanLists

			if list.length == (list.toSet).size // Me aseguro que tengan elementos diferentes
		} yield list

		def filter(solution : List[Int], filteredSolutions : List[List[Int]], possibleLists : List[List[Int]]) : List[List[Int]] = {
			if(possibleLists.isEmpty) filteredSolutions
			else {
				if(!filteredSolutions.exists(x => {
				(x.toSet).equals(solution.toSet)})) filter(possibleLists.head, filteredSolutions :+ solution, possibleLists.tail)
				else filter(possibleLists.head, filteredSolutions, possibleLists.tail)
			}
		}

		filter(possibleLists.head, List(), possibleLists.tail)
	}
}