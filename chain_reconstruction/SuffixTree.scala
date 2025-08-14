package object SuffixTree {
	abstract class Tree
	case class Node(char: Char, marked: Boolean, children: List[Tree]) extends Tree
	case class Leaf(char: Char, marked: Boolean) extends Tree

	/**
	  * Returns the edge label leading to tree t, i.e. the character stored
	  * at its root.
	  *
	  * @param t a Tree (either Node or Leaf).
	  * @return the character labelling the edge from its parent.
	  */
	def root(t: Tree): Char = {
		t match {
			case Node(c, _, _) => c
			case Leaf(c, _) => c
		}
	}

	/**
	  * Retrieves the set of next‑characters available from the current node.
	  *
	  * Essentially a helper to avoid pattern matching in callers.
	  *
	  * @param t any Tree.
	  * @return the sequence of root characters of its direct children
	  *         (or a singleton sequence containing its own label for leaves).
	  */
	def heads(t: Tree): Seq[Char] = {
		t match {
			case Node(_, _, c) => c.map(root)
			case Leaf(c, _) => Seq[Char](c)
		}
	}

	/**
	  * Checks whether the chain "s" is stored in the tree "t".
	  *
	  * @param s Candidate chain (possibly empty).
	  * @param t Tree to query.
	  * @return true iff "s" is represented in "t".
	  */
	def contains(s: Seq[Char], t: Tree): Boolean = {
		s match {
			case Seq() => t match {
				case Node(_, m, _) => m
				case Leaf(_, m) => m
			}
			case s_char +: rest => t match {
				case Node(_, _, children) =>
					val t_heads = heads(t)
					val idx = t_heads.indexOf(s_char)
					if (idx == -1)
						false
					else
						contains(rest, children(idx))
				case Leaf(_, _) => false
			}
		}
	}

	/**
	  * Inserts the chain "s" into the tree "t", returning a new tree.
	  *
	  * @param s Chain to insert (may be empty -> only marks the root node).
	  * @param t Original tree.
	  * @return a fresh tree that recognizes every chain recognized by "t" plus
	  *         "s".
	  */
	def add(s: Seq[Char], t: Tree): Tree = {
		def as_node(t: Tree): Node = t match {
			case n @ Node(_, _, _) => n
			case Leaf(char, marked) => Node(char, marked, Nil)
		}

		s match {
			case Seq() =>
				t match {
					case Node(char, _, children) => Node(char, true, children)
					case Leaf(char, _) => Leaf(char, true)
				}
			case head +: tail =>
				val Node(char, marked, children) = as_node(t)

				val (before, at_and_after) = children.span(child => root(child) != head)

				at_and_after match {
					case child +: rest =>
						// Existing branch
						val updated_child = add(tail, child)
						Node(char, marked, before ::: (updated_child :: rest))
					case Nil =>
						// No existing branch
						val new_child =
							if (tail.isEmpty)
								Leaf(head, true)
							else
								add(tail, Node(head, false, Nil))

						Node(char, marked, before :+ new_child)
					case _ => Leaf(' ', false) // Never reached, silences compiler
				}
		}
	}

	/**
	  * Builds a suffix tree that recognizes all suffixes of the input
	  * sequences.
	  *
	  * @param ss Collection of DNA chains.
	  * @return a trie recognizing every suffix of every s in ss.
	  */
	def suffix_tree(ss: Seq[Seq[Char]]): Tree = {
		def tails(s: Seq[Char]): Seq[Seq[Char]] = {
			for {
				i <- 0 until s.length
			} yield s.drop(i)
		}

		val empty: Tree = Node(' ', marked = false, Nil)
		ss.foldLeft(empty) { (tree, s) =>
			tails(s).foldLeft(tree)( (t, suffix) => add(suffix, t) )
		}
	}
}
