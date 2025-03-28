package object train_rearrangement {
	type Wagon = Any
	type Train = List[Wagon]
	type State = (Train, Train, Train)

	trait Movement
	case class One(n: Int) extends Movement
	case class Two(n: Int) extends Movement

	type Maneuver = List[Movement]

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */

	/* ---------------------------------- 1.2.1 --------------------------------- */

	/*
	def remove_from_left(train: Train, elements_to_remove: Int): Train = {
		if (elements_to_remove == 0)
			train
		else
			remove_from_left(train.tail, elements_to_remove - 1)
	}

	def keep_until(train: Train, n: Int): Train = {
		if (n == 0)
			Nil
		else
			train.head :: keep_until(train.tail, n - 1)
	}

	def extract_from_left(train: Train, elements_to_extract: Int): Train = {
		if (elements_to_extract == 0)
			Nil
		else
			train.head :: extract_from_left(train.tail, elements_to_extract - 1)
	}

	def keep_after(train: Train, n: Int): Train = {
		if (n == 0)
			train
		else
			keep_after(train.tail, n - 1)
	}
	*/

	/**
	 * Removes a specified number of elements from the left (front) of a train.
	 *
	 * @param train The train to remove elements from.
	 * @param elements_to_remove The number of elements to remove.
	 * @return a new train with the specified elements removed from the left
	 */
	def remove_from_left(train: Train, elements_to_remove: Int): Train = (elements_to_remove, train) match {
		case (0, _) => train
		case (n, _ :: tail) => remove_from_left(tail, n - 1)
	}

	/**
	 * Keeps the first n elements of a train.
	 *
	 * @param train The original train.
	 * @param n The number of elements to keep.
	 * @return a new train containing only the first n elements.
	 */
	def keep_until(train: Train, n: Int): Train = (n, train) match {
		case (0, _) => Nil
		case (n, head :: tail) => head :: keep_until(tail, n - 1)
	}

	/**
	 * Extracts a specified number of elements from the left (front) of a train.
	 *
	 * @param train The train to extract elements from.
	 * @param elements_to_extract The number of elements to extract.
	 * @return a new train containing only the extracted elements.
	 */
	def extract_from_left(train: Train, elements_to_extract: Int): Train = (elements_to_extract, train) match {
		case (0, _) => Nil
		case (n, head :: tail) => head :: extract_from_left(tail, n - 1)
	}

	/**
	 * Returns the elements of a train after skipping the first n elements.
	 *
	 * @param train The original train.
	 * @param n The number of elements to skip.
	 * @return a new train without the first n elements.
	 */
	def keep_after(train: Train, n: Int): Train = (n, train) match {
		case (0, _)  => train
		case (n, _ :: tail) => keep_after(tail, n - 1)
	}

	def insert_into_left(wagons: List[Wagon], train: Train): Train = {
		wagons ++ train
	}

	def insert_into_right(wagons: List[Wagon], train: Train): Train = {
		train ++ wagons
	}

	def remove_from_right(train: Train, elements_to_remove: Int): Train = {
		val n = train.length - elements_to_remove
		keep_until(train, n)
	}

	def extract_from_right(train: Train, elements_to_extract: Int) = {
		val n = train.length - elements_to_extract
		keep_after(train, n)
	}

	def apply_movement(e: State, m: Movement): State = {
		val (principal: Train, one: Train, two: Train) = e
		val principal_length = principal.length
		val one_length = one.length
		val two_length = two.length

		m match {
			case One(n) => {
				if (n > principal_length) {
					(Nil, principal ++ one, two)
				} else if (n > 0) {
					val removed_elements = extract_from_right(principal, n)
					val new_principal = remove_from_right(principal, n)
					val new_one = insert_into_left(removed_elements, one)

					(new_principal, new_one, two)
				} else if (n == 0) {
					(principal, one, two)
				} else if (n < -1 * one_length) {
					(principal ++ one, Nil, two)
				} else {
					val removed_elements = extract_from_left(one, n * -1)
					val new_principal = insert_into_right(removed_elements, principal)
					val new_one = remove_from_left(one, n * -1)

					(new_principal, new_one, two)
				}
			}
			case Two(n) => {
				if (n > principal_length) {
					(Nil, one, principal ++ two)
				} else if (n > 0) {
					val removed_elements = extract_from_right(principal, n)
					val new_principal = remove_from_right(principal, n)
					val new_two = insert_into_left(removed_elements, two)

					(new_principal, one, new_two)
				} else if (n == 0) {
					(principal, one, two)
				} else if (n < -1 * two_length) {
					(principal ++ two, one, Nil)
				} else {
					val removed_elements = extract_from_left(two, -1 * n)
					val new_principal = insert_into_right(removed_elements, principal)
					val new_two = remove_from_left(two, -1 * n)

					(new_principal, one, new_two)
				}
			}
		}
	}

	/* ---------------------------------- 1.2.2 --------------------------------- */

	/*
	def apply_maneuver(e: State, movements: Maneuver): List[State] = {
		if (movements.isEmpty)
			Nil
		else {
			val new_state = apply_movement(e, movements.head)
			new_state :: apply_maneuver(new_state, movements.tail)
		}
	}
	*/

	def apply_maneuver(e: State, movements: Maneuver): List[State] = movements match {
		case Nil => Nil
		case head :: tail => {
			val new_state = apply_movement(e, head)
			new_state :: apply_maneuver(new_state, tail)
		}
	}

	/* ---------------------------------- 1.2.3 --------------------------------- */

	def search_wagon_index(train: Train, wagon: Wagon): Int =  (train, wagon) match {
		case (wagon :: tail, _) => 0
		case (head :: tail, _) => 1 + search_wagon_index(train.tail, wagon)
	}

	def order_element(principal: Train, wagon: Wagon): (Train, Maneuver) = {
		val n = principal.length
		val element_index = search_wagon_index(principal, wagon)
		val maneuver = List(One(n - element_index - 1), Two(1), One(-1 * (n - element_index - 1)))
		val new_principal = principal.take(element_index) ++ principal.drop(element_index + 1)

		(new_principal, maneuver)
	}

	def order_all_elements(current: Train, goal: Train, n: Int): Maneuver = {
		if (n == 0)
			Nil
		else {
			val (new_principal, maneuver) = order_element(current, goal(n - 1))
			maneuver ++ order_all_elements(new_principal, goal, n - 1)
		}
	}

	def definy_maneuver(t_1: Train, t_2: Train): Maneuver = {
		val n = t_1.length // = t_2.length

		order_all_elements(t_1, t_2, n) ++ List(Two(-1 * n))
	}
}
