
import scala.util.Random


package KMeans {
	/* -------------------------------------------------------------------------- */
	/*                                      1                                     */
	/* -------------------------------------------------------------------------- */

	type Classification = Map[Point, Seq[Point]]

	/* ----------------------------------- 1.1 ---------------------------------- */

	class Point(val x: Double, val y: Double) {
		private def square(v: Double): Double = v * v

		def square_distance(that: Point): Double =
		square(that.x - x) + square(that.y - y)

		private def round(v: Double): Double = (v * 100).toInt / 100 // Round to 2 decimals

		override def toString = s"(${round(x)}, ${round(y)})"
	}

	/**
	  * Finds the closest Point (mean) to a given point p among a non-empty sequence of means.
	  *
	  * @param p The point to classify.
	  * @param means A non-empty sequence of current means.
	  * @return the mean whose squared distance to p is minimal.
	  * @throws AssertionError if means is empty.
	  */
	def closest_point(p: Point, means: Seq[Point]): Point = {
		assert(means.nonEmpty)
		means.map(point =>
					(point, p.square_distance(point))
				).sortBy(_._2).head._1
	}

	/**
	  * Classifies each point in the input sequence by assigning it to the closest mean.
	  * Returns a Classification map from each mean to the sequence of points belonging to it.
	  *
	  * @param points A sequence of points to classify.
	  * @param means A sequence of current means (centroids).
	  * @return a Classification mapping each mean to its assigned points.
	  */
	def classify(points: Seq[Point], means: Seq[Point]): Classification = {
		points.groupBy(point => closest_point(point, means))
	}

	/* ----------------------------------- 1.2 ---------------------------------- */
	/**
	  * Computes the new mean (centroid) given an old mean and the points assigned to it.
	  * If no points are assigned, returns the old mean unchanged.
	  *
	  * @param old_mean The previous mean (centroid).
	  * @param points Sequence of points in the cluster corresponding to old_mean.
	  * @return a new Point representing the average of the cluster, or old_mean if the cluster is empty.
	  */
	def calculate_average(old_mean: Point, points: Seq[Point]): Point = {
		if (points.isEmpty)
			old_mean
		else
			new Point(points.map(p => p.x).sum / points.length, points.map(p => p.y).sum / points.length)
	}

	/**
	  * Updates the sequence of old means by computing new means for each cluster.
	  * If a mean does not appear as a key in the classification map, it remains unchanged.
	  *
	  * @param classification A map from old means to the sequence of points assigned to each.
	  * @param old_means The sequence of old means, in the same order as originally provided.
	  * @return a sequence of new means (Point), preserving the original ordering of old_means.
	  */
	def update_means(classification: Classification, old_means: Seq[Point]): Seq[Point] = {
		def new_mean(old_mean: Point): Point = {
			if (!classification.contains(old_mean))
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

	/* ----------------------------------- 1.3 ---------------------------------- */
	/**
	  * Determines whether the algorithm has converged by checking that no mean
	  * has moved by more than eta (in squared distance) compared to its previous location.
	  *
	  * @param eta       The convergence threshold (maximum allowed squared displacement).
	  * @param old_means Sequence of old means.
	  * @param new_means Sequence of new means, in the same order as old_means.
	  * @return true if every mean has moved by at most eta; false otherwise.
	  */
	def is_there_convergence(eta: Double, old_means: Seq[Point], new_means: Seq[Point]): Boolean = {
		def is_divergent(old_mean: Point): Boolean = {
			val index = old_means.indexOf(old_mean)

			eta < old_mean.square_distance(new_means(index))
		}

		old_means.forall(mean => !is_divergent(mean)) // !old_means.exists(mean => is_divergent(mean))
	}


	/* ----------------------------------- 1.4 ---------------------------------- */
	/**
	  * Performs one full KMeans iteration: classifies all points, updates means,
	  * and either recurses (if not converged) or returns the final means.
	  *
	  * @param points A sequence of points to cluster.
	  * @param means  The current sequence of means (centroids).
	  * @param eta    The convergence threshold: algorithm stops when each mean moves by at most eta.
	  * @return the final sequence of means once convergence is reached.
	  */
	def run(points: Seq[Point], means: Seq[Point], eta: Double): Seq[Point] = {
		val classified = classify(points, means)
		val new_means = update_means(classified, means)

		if (is_there_convergence(eta, means, new_means))
			new_means
		else
			run(points, new_means, eta)
	}

	/* ----------------------------------- 1.5 ---------------------------------- */
	/**
	  * Generates a sequence of n random points within [min_number, max_number].
	  *
	  * @param n          The number of random points to generate.
	  * @param min_number The minimum value for both x and y coordinates.
	  * @param max_number The maximum value for both x and y coordinates.
	  * @return a sequence of n Points with coordinates uniformly sampled in the given range.
	  */
	def generate_points(n: Int, min_number: Int, max_number: Int): Seq[Point] = {
		val rand = new Random

		(0 until n).map { _ =>
			val x = min_number + (max_number - min_number) * rand.nextDouble()
			val y = min_number + (max_number - min_number) * rand.nextDouble()

			new Point(x, y)
		}
	}

	/**
	  * Initializes k means by choosing k random points from the provided sequence.
	  *
	  * @param k The number of clusters (means) to initialize.
	  * @param points The sequence of points to sample from.
	  * @return a sequence of k randomly selected points from the input as initial means.
	  */
	def initialize_means(k: Int, points: Seq[Point]): Seq[Point] = {
		val rand = new Random
		(0 until k).map(_ => points(rand.nextInt(points.length)))
	}
}
