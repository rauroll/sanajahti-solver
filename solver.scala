import scala.io.Source
import scala.collection.mutable.Set


case class Game(input: String) {

	val grid = input.split(" ").map(_.toCharArray)
	grid.foreach(x => println(x.mkString))
	println("=================")

	def contains(word: String): Option[(Int, Int)] = {
		
		for (i <- grid.indices) {
			for (j <- grid(0).indices) {
				if (grid(i)(j) == word.head) {
					if (search((i, j), word.tail)) return Some((i, j));
				}
			}
		}
		None
	}

	def search(start: (Int, Int), word: String): Boolean = {
		

		def potentialNeighbors(visited: Set[(Int, Int)], current: (Int, Int), remaining: String): Boolean = {
			if (remaining.isEmpty)
				return true
			val neighs = scala.collection.mutable.Buffer[(Int, Int)]()
			for (i <- -1 to 1) {
				for (j <- -1 to 1) {
					val dx = current._1 + i
					val dy = current._2 + j
					if ((i != 0 || j != 0) && dx >= 0 && dy >= 0 && dx < grid.size && dy < grid(0).size) {
						neighs += ((dx, dy))
					}
				}
			}

			val nextFrontier = neighs.filter(x => grid(x._1)(x._2) == remaining.head && !visited.contains((x._1, x._2)))
			nextFrontier.map(x => potentialNeighbors(visited + x, x, remaining.drop(1))).exists(_ == true)
		}

		potentialNeighbors(Set(start), start, word)

	}

	
}

object Solver extends App {

	val words = Source.fromFile("words.txt").getLines().toArray
	val input = readLine("Give the game grid as a string\n")

	val game = Game(input)

	val wordsFound = Set[(String, (Int, Int))]()

	for (word <- words) {
		val res = game.contains(word)
		if (res.isDefined)
			wordsFound += ((word, res.get))
	}

	println("Found the following words: ")
	println(wordsFound.toVector.sortBy(-_._1.size).map(x => x._1 + ", starting at " + x._2).mkString("\n"))

}







