import KMeans._


object Main extends App {
	val k = 3      // Number of clusters
	val n = 100    // Number of points to generate
	val eta = 0.01 // Convergence criterion
	val min = 0    // Minimum coordinate value
	val max = 1000 // Maximum coordinate value

	// Generate random points
	val points = generate_points(n, min, max)
	println(s"Generated $n random points")

	// Initialize random means
	val initial_means = initialize_means(k, points)
	println(s"Initial means: ${initial_means.mkString(", ")}")

	// Run K-means algorithm
	val final_means = KMeans_(points, initial_means, eta)
	println(s"Final means: ${final_means.mkString(", ")}")

	// Show final classification
	val final_classification = classify(points, final_means)
	final_classification.foreach { case (mean, cluster) =>
	println(s"\nCluster centered at $mean:")
	println(s"Size: ${cluster.size}")
	println(s"Points: ${cluster.take(5).mkString(", ")}...")
	}
}
