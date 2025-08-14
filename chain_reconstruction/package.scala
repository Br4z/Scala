import Oracle.{alphabet, Oracle}
import SuffixTree.{Tree, contains, suffix_tree}


package chain_reconstruction {
	/* -------------------------------------------------------------------------- */
	/*                                      3                                     */
	/* -------------------------------------------------------------------------- */

	/* ----------------------------------- 3.1 ---------------------------------- */
	/**
	  * Exhaustively enumerates all |Σ|^n candidate chains and returns the
	  * first one accepted by the oracle.
	  *
	  * @param n Target length |S|.
	  * @param o Oracle that recognizes valid subsequences of S.
	  * @return the unique chain S.
	  * @throws RuntimeException if the oracle rejects every candidate of length n.
	  */
	def brute_force(n: Int, o: Oracle): Seq[Char] = {
		def generate_all_chains(n: Int): List[Seq[Char]] = {
			if (n == 0)
				List(Seq.empty)
			else {
				for {
					prefix <- generate_all_chains(n - 1)
					char <- alphabet
				} yield prefix :+ char
			}
		}

		generate_all_chains(n).find(o).getOrElse(
			throw new RuntimeException(s"Error: no valid string of length $n found by the oracle")
		)
	}

	/* ----------------------------------- 3.2 ---------------------------------- */
	/**
	  * Grows chains incrementally: at step k filters the |Σ| extensions
	  * of every surviving prefix of length k – 1, retaining only those accepted
	  * by the oracle.
	  *
	  * @param n Length of the unknown chain.
	  * @param o Membership oracle.
	  * @return the reconstructed chain.
	  * @throws RuntimeException if the oracle never accepts a length‑n chain.
	  */
	def less_brute_force(n: Int, o: Oracle): Seq[Char] = {
		var SC_k_minus_1: List[Seq[Char]] = List(Seq.empty[Char]) // SC_0 = {ε}

		for (_ <- 1 to n) {
			val SC_k = for {
				prefix <- SC_k_minus_1
				char <- alphabet

				new_prefix = prefix :+ char

				if o(new_prefix)
			} yield new_prefix

			SC_k_minus_1 = SC_k
		}

		if (SC_k_minus_1.isEmpty)
			throw new RuntimeException(s"Error: no valid string of length $n found by the oracle")

		SC_k_minus_1.head
	}

	/* ----------------------------------- 3.3 ---------------------------------- */
	/**
	  * Reconstructs S when n is a power of 2 by doubling the length of valid
	  * chains at each iteration (k -> 2k).
	  *
	  * @param n Length of the target chain (power of 2).
	  * @param o Oracle.
	  * @return the reconstructed chain.
	  * @throws RuntimeException if the oracle rejects every proposal of length n.
	  */
	def binary_force(n: Int, o: Oracle): Seq[Char] = {
		var SC_k_minus_1: List[Seq[Char]] = (for {
			char <- alphabet
			seq_char = Seq(char)
			if o(seq_char)
		} yield seq_char).toList // SC_1

		var k: Int = 2

		while (k <= n) {
			val SC_k = for {
				prefix <- SC_k_minus_1
				suffix <- SC_k_minus_1
				new_chain = prefix ++ suffix

				if o(new_chain)
			} yield new_chain

			if (SC_k.isEmpty)
				throw new RuntimeException(s"Error: no valid string of length $n found by the oracle")

			SC_k_minus_1 = SC_k

			k = k * 2
		}
		SC_k_minus_1.head
	}

	/* ----------------------------------- 3.4 ---------------------------------- */
	/**
	  * Checks whether every contiguous subchain of length k of seq
	  * already belongs to the survivor set SC.
	  *
	  * @param SC  Survivor set of length k chains.
	  * @param seq The proposal to validate.
	  * @param k   subchain length.
	  */
	def contain_valid_subchains(SC: List[Seq[Char]], seq: Seq[Char], k: Int): Boolean = {
		val seq_length = seq.length

		if (seq_length < k)
			false
		else {
			(0 to seq_length - k).forall { i =>
				val subchain = seq.slice(i, i + k)
				SC.contains(subchain)
			}
		}
	}

	/**
	  * Same doubling strategy as binary_force, but prunes proposals whose
	  * internal subchains were already discarded in previous rounds.
	  *
	  * @param n length of the chain.
	  * @param o oracle.
	  * @return the reconstructed chain.
	  * @throws RuntimeException if no valid chain of length n exists.
	  */
	def better_binary_force(n: Int, o: Oracle): Seq[Char] = {
		/**
		  * Generates length‑k candidates formed by concatenating survivors of length k / 2
		  * and keeps only those whose all length‑(k / 2) subchains are themselves survivors.
		  */
		def generate_proposals(SC: List[Seq[Char]], k: Int): List[Seq[Char]] = {
			for {
				p <- SC
				s <- SC

				c = p ++ s
				if (contain_valid_subchains(SC, c, k / 2))
			} yield c
		}

		var SC_k_minus_1: List[Seq[Char]] = (for {
			char <- alphabet
			seq_char = Seq(char)
			if o(seq_char)
		} yield seq_char).toList // SC_1

		var k: Int = 2

		while (k <= n) {
			val proposals = generate_proposals(SC_k_minus_1, k)

			val SC_k = for {
				proposal <- proposals

				if o(proposal)
			} yield proposal

			if (SC_k.isEmpty)
				throw new RuntimeException(s"Error: no valid string of length $n found by the oracle")

			SC_k_minus_1 = SC_k

			k = k * 2
		}

		SC_k_minus_1.head
	}

	/* ----------------------------------- 3.5 ---------------------------------- */
	/**
	  * Improved variant of better_binary_force that stores each survivor set SC_k
	  * in a suffix tree so that membership checks for k/2‑length subchains run in
	  * O(k) instead of O(|SC_k|).
	  *
	  * @param n Length of the target chain (must be a power of two).
	  * @param o Oracle that recognizes subsequences of S.
	  * @return the reconstructed DNA chain S.
	  */
	def ultimate_binary_force(n: Int, o: Oracle): Seq[Char] = {
		// Fast check using suffix tree of the previous round
		def contain_valid_subchains(tree: Tree, seq: Seq[Char], k: Int): Boolean = {
			val seq_length = seq.length

			if (seq_length < k / 2)
				false
			else {
				(0 to seq_length - k / 2).forall { i =>
					contains(seq.slice(i, i + k / 2), tree)
				}
			}
		}

		var SC_k_minus_1: List[Seq[Char]] = (for {
			char <- alphabet
			seq_char = Seq(char)
			if o(seq_char)
		} yield seq_char).toList // SC_1

		var k: Int = 2

		while (k <= n) {
			var tree: Tree = suffix_tree(SC_k_minus_1)
			val proposals = for {
				p <- SC_k_minus_1
				s <- SC_k_minus_1

				c = p ++ s
				if (contain_valid_subchains(tree, c, k))
			} yield c

			val SC_k = for {
				proposal <- proposals

				if o(proposal)
			} yield proposal

			if (SC_k.isEmpty)
				throw new RuntimeException(s"Error: no valid string of length $n found by the oracle")

			SC_k_minus_1 = SC_k

			k = k * 2
		}

		SC_k_minus_1.head
	}
}
