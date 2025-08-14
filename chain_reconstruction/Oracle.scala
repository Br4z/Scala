package object Oracle {
	val alphabet = Seq('a', 'c', 'g', 't')
	type Oracle = Seq[Char] => Boolean


	def create_oracle(delay: Int)(original: Seq[Char]): Oracle = {
		def is_subchain(seq: Seq[Char]): Boolean = {
			Thread.sleep(delay)
			original.containsSlice(seq)
		}
		is_subchain
	}
}
