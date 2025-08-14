import chain_reconstruction.{brute_force,
                             less_brute_force,
                             binary_force,
                             better_binary_force,
                             ultimate_binary_force}
import Oracle.Oracle
import SuffixTree.{Node, Leaf, add, suffix_tree}


object Main extends App {
	///*
	// Test different oracles
	val oracle_1: Oracle = (seq: Seq[Char]) => Seq('a', 'c', 'g', 'g').containsSlice(seq)
	val oracle_2: Oracle = (seq: Seq[Char]) => Seq('t', 'a', 'g', 'a').containsSlice(seq)
	val oracle_3: Oracle = (seq: Seq[Char]) => Seq('g', 'c', 'a', 't').containsSlice(seq)

	// Test parameters
	val n = 4

	println("Testing all chain reconstruction methods:")
	println("-" * 50)

	println("Oracle 1 (searching for \"acgg\"):")
	println(s"Brute force: ${brute_force(n, oracle_1)}")
	println(s"Less brute force: ${less_brute_force(n, oracle_1)}")
	println(s"Binary force: ${binary_force(n, oracle_1)}")
	println(s"Better binary force: ${better_binary_force(n, oracle_1)}")
	println(s"Ultimate binary force: ${ultimate_binary_force(n, oracle_1)}")
	println("-" * 50)

	println("Oracle 2 (searching for \"taga\"):")
	println(s"Brute force: ${brute_force(n, oracle_2)}")
	println(s"Less brute force: ${less_brute_force(n, oracle_2)}")
	println(s"Binary force: ${binary_force(n, oracle_2)}")
	println(s"Better binary force: ${better_binary_force(n, oracle_2)}")
	println(s"Ultimate binary force: ${ultimate_binary_force(n, oracle_2)}")
	println("-" * 50)

	println("Oracle 3 (searching for \"gcat\"):")
	println(s"Brute force: ${brute_force(n, oracle_3)}")
	println(s"Less brute force: ${less_brute_force(n, oracle_3)}")
	println(s"Binary force: ${binary_force(n, oracle_3)}")
	println(s"Better binary force: ${better_binary_force(n, oracle_3)}")
	println(s"Ultimate binary force: ${ultimate_binary_force(n, oracle_3)}")
	println("-" * 50)
	//*/

	/* -------------------------------------------------------------------------- */
	///*
	// Example DNA chains
	val chains = Seq(
		Seq('a', 'c', 'g', 'g'),
		Seq('t', 'a', 'g', 'a'),
		Seq('g', 'c', 'a', 't')
	)

	// Build suffix tree
	val tree = suffix_tree(chains)
	println("Suffix tree built from chains: " + chains.map(_.mkString).mkString(", "))

	// Test contains
	val test_seq = Seq(
		Seq('a', 'c', 'g', 'g'),
		Seq('c', 'g', 'g'),
		Seq('g', 'g'),
		Seq('t', 'a', 'g', 'a'),
		Seq('g', 'c', 'a', 't'),
		Seq('a', 't'),
		Seq('x', 'y', 'z')
	)

	println("Testing contains method:")
	for (seq <- test_seq) {
		println(s"Contains '${seq.mkString}': ${SuffixTree.contains(seq, tree)}")
	}

	// Test add
	val new_seq = Seq('a', 'g', 'a')
	val tree_with_new = add(new_seq, tree)
	println(s"Added new sequence '${new_seq.mkString}' to tree.")
	println(s"Contains '${new_seq.mkString}': ${SuffixTree.contains(new_seq, tree_with_new)}")

	// Test heads
	println("Root heads of tree: " + SuffixTree.heads(tree))
	//*/
}
