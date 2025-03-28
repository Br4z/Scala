
import scala.collection.{Map, Seq}
// import scala.collection.parallel.CollectionConverters._
// import scala.collection.parallel.{ParMap, ParSeq}
import scala.util.Random

package KMeans {
	/* -------------------------------------------------------------------------- */
	/*                                     1.1                                    */
	/* -------------------------------------------------------------------------- */

	class Point(val x: Double, val y: Double) {
		private def square(v: Double): Double = v * v

		def square_distance(that: Point): Double =
		square(that.x - x) + square(that.y - y)

		private def round(v: Double): Double = (v * 100).toInt / 100 // Round to 2 decimals

		override def toString = s"(${round(x)}, ${round(y)})"
	}

	def closest_point(p: Point, means: Seq[Point]): Point = {
		assert(means.nonEmpty)
		means.map(point =>
					(point, p.square_distance(point))
				).sortWith((a, b) => (a._2 < b._2)).head._1
	}

	def classify(points: Seq[Point], means: Seq[Point]): Map[Point, Seq[Point]] = {
		points.groupBy(punto => closest_point(punto, means))
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.2                                    */
	/* -------------------------------------------------------------------------- */

	def calculate_average(old_mean: Point, points: Seq[Point]): Point = {
		if (points.isEmpty)
			old_mean
		else
			new Point(points.map(p => p.x).sum / points.length, points.map(p => p.y).sum / points.length)
	}

	def update_means(classification: Map[Point, Seq[Point]], old_means: Seq[Point]): Seq[Point] = {
		def new_mean(old_mean: Point): Point = {
			if(!classification.contains(old_mean))
				old_mean
			else {
				val points = classification(old_mean)
				calculate_average(old_mean, points)
			}
		}

		for {
			mean <- old_means
		} yield new_mean(mean)
	}

	/* -------------------------------------------------------------------------- */
	/*                                     1.3                                    */
	/* -------------------------------------------------------------------------- */

	def is_there_convergence(eta: Double, old_means: Seq[Point], new_means: Seq[Point]): Boolean = {
		def is_divergent(old_mean: Point): Boolean = {
			val index = old_means.indexOf(old_mean)

			eta < old_mean.square_distance(new_means(index))
		}

		old_means.forall(mean => !is_divergent(mean))
	}


	/* -------------------------------------------------------------------------- */
	/*                                     1.3                                    */
	/* -------------------------------------------------------------------------- */

	def KMeans_(points: Seq[Point], means: Seq[Point], eta: Double): Seq[Point] = {
		val classified = classify(points, means)
		val new_means = update_means(classified, means)

		if(is_there_convergence(eta, means, new_means))
			new_means
		else
			KMeans_(points, new_means, eta)
	}

	/* -------------------------------------------------------------------------- */
	/*                         1.5 Corriendo el algoritmo                         */
	/* -------------------------------------------------------------------------- */

	def generate_points(n: Int, min_number: Int, max_number: Int): Seq[Point] = {
		val rand = new Random

		(0 until n).map { i =>
			val x = min_number + (max_number - min_number) * rand.nextDouble()
			val y = min_number + (max_number - min_number) * rand.nextDouble()

			new Point(x, y)
		}
	}

	def initialize_means(k: Int, points: Seq[Point]): Seq[Point] = {
		val rand = new Random
		(0 until k).map(_ => points(rand.nextInt(points.length)))
	}
}
