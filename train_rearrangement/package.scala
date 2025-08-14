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
	/**
	  * Removes a specified number of elements from the left (front) of a train.
	  *
	  * @param train The train to remove elements from.
	  * @param n The number of elements to remove.
	  * @return a new train with the specified elements removed from the left
	  */
	def remove_from_left(train: Train, n: Int): Train = {
		train.drop(n);
	}

	/**
	  * Extracts a specified number of elements from the left (front) of a train.
	  *
	  * @param train The train to extract elements from.
	  * @param n The number of elements to extract.
	  * @return a new train containing only the extracted elements.
	  */
	def extract_from_left(train: Train, n: Int): Train = {
		train.take(n)
	}

	/**
	  * Inserts wagons at the left (front) of a train.
	  *
	  * @param wagons The wagons to insert.
	  * @param train The train to insert into.
	  * @return a new train with the wagons inserted at the front.
	  */
	def insert_into_left(wagons: List[Wagon], train: Train): Train = {
		wagons ++ train
	}

	/**
	  * Inserts wagons at the right (end) of a train.
	  *
	  * @param wagons The wagons to insert.
	  * @param train The train to insert into.
	  * @return a new train with the wagons inserted at the end
	  */
	def insert_into_right(wagons: List[Wagon], train: Train): Train = {
		train ++ wagons
	}

	/**
	  * Removes a specified number of elements from the right (end) of a train.
	  *
	  * @param train The train to remove elements from.
	  * @param elements_to_remove The number of elements to remove.
	  * @return a new train with the specified elements removed from the right.
	  */
	def remove_from_right(train: Train, n: Int): Train = {
		train.dropRight(n)
	}

	/**
	  * Extracts a specified number of elements from the right (end) of a train.
	  *
	  * @param train The train to extract elements from.
	  * @param elements_to_extract The number of elements to extract.
	  * @return a new train containing only the extracted elements.
	  */
	def extract_from_right(train: Train, n: Int) = {
		train.takeRight(n)
	}

	/**
	  * Applies a single movement to the current state, moving wagons between the principal train
	  * and the auxiliary tracks according to the movement specification.
	  *
	  * Positive values of n move wagons from the principal train to an auxiliary track.
	  * Negative values of n move wagons from an auxiliary track to the principal train.
	  * Zero values leave the state unchanged.
	  *
	  * @param e The current state of trains (principal, one, two).
	  * @param m The movement to apply.
	  * @return the new state after applying the movement.
	  */
	def apply_movement(e: State, m: Movement): State = {
		val (principal: Train, one: Train, two: Train) = e
		val principal_length = principal.length
		val one_length = one.length
		val two_length = two.length

		m match {
			case One(n) => {
				if (n > principal_length)
					(Nil, principal ++ one, two)
				else if (n > 0) {
					val removed_elements = extract_from_right(principal, n)
					val new_principal = remove_from_right(principal, n)
					val new_one = insert_into_left(removed_elements, one)

					(new_principal, new_one, two)
				} else if (n == 0)
					(principal, one, two)
				else if (n < -one_length)
					(principal ++ one, Nil, two)
				else {
					val removed_elements = extract_from_left(one, -n)
					val new_principal = insert_into_right(removed_elements, principal)
					val new_one = remove_from_left(one, -n)

					(new_principal, new_one, two)
				}
			}
			case Two(n) => {
				if (n > principal_length)
					(Nil, one, principal ++ two)
				else if (n > 0) {
					val removed_elements = extract_from_right(principal, n)
					val new_principal = remove_from_right(principal, n)
					val new_two = insert_into_left(removed_elements, two)

					(new_principal, one, new_two)
				} else if (n == 0)
					(principal, one, two)
				else if (n < -two_length)
					(principal ++ two, one, Nil)
				else {
					val removed_elements = extract_from_left(two, -n)
					val new_principal = insert_into_right(removed_elements, principal)
					val new_two = remove_from_left(two, -n)

					(new_principal, one, new_two)
				}
			}
		}
	}

	/* ---------------------------------- 1.2.2 --------------------------------- */
	/**
	  * Applies a sequence of movements (a maneuver) to an initial state and returns
	  * a list of all intermediate states.
	  *
	  * @param e The initial state of trains.
	  * @param movements The sequence of movements to apply.
	  * @return a list of states representing the result of each movement in the sequence.
	  */
	def apply_maneuver(e: State, movements: Maneuver): List[State] = {
		movements.scanLeft(e)(apply_movement).tail
	}

	/* ---------------------------------- 1.2.3 --------------------------------- */
	/**
	  * Creates a maneuver to move a specific wagon to an auxiliary track and returns
	  * the resulting principal train and the maneuver to accomplish this.
	  *
	  * @param principal The principal train.
	  * @param wagon The wagon to order.
	  * @return a tuple containing the new principal train and the maneuver performed.
	  */
	def order_element(principal: Train, wagon: Wagon): (Train, Maneuver) = {
		val n = principal.length
		val element_index = principal.indexOf(wagon) // Finds the index of a specific wagon in a train
		val elements_to_move = n - element_index - 1;
		val maneuver = List(One(elements_to_move), Two(1), One(-elements_to_move))
		// Simulate the maneuver execution (the original principal without the displaced element, that is now in
		// Two)
		val new_principal = extract_from_left(principal, element_index) ++ remove_from_left(principal, element_index + 1)

		(new_principal, maneuver)
	}

	/**
	  * Creates a maneuver to order all elements in a train according to a goal arrangement.
	  *
	  * @param current The current train arrangement.
	  * @param goal The target train arrangement.
	  * @param n The number of wagons to process.
	  * @return a maneuver that will transform the current train into the goal arrangement.
	  */
	def order_all_elements(current: Train, goal: Train, n: Int): Maneuver = {
		if (n == 0)
			Nil
		else {
			val (new_principal, maneuver) = order_element(current, goal(n - 1))
			maneuver ++ order_all_elements(new_principal, goal, n - 1)
		}
	}

	/**
	  * Defines a complete maneuver to transform one train arrangement into another.
	  * This function orchestrates the entire rearrangement process.
	  *
	  * @param original The original train arrangement.
	  * @param target The target train arrangement.
	  * @return a maneuver that will transform "original" into "target".
	  */
	def define_maneuver(original: Train, target: Train): Maneuver = {
		val n = original.length // = t_2.length

		order_all_elements(original, target, n) ++ List(Two(-n))
	}
}
