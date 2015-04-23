package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = b match {
    case Block(b1, b2) => b1 == goal && b.isStanding
  }

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   * 
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   * 
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] =
    (Stream[(Block, List[Move])]() /: b.legalNeighbors)((acc, n) => n match {
      case (block, move) => (block, move::history) #:: acc
    })

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])], 
                       explored: Set[Block]): Stream[(Block, List[Move])] =
    neighbors.filter { case (block, _) => !explored.contains(block) }

  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   * 
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   * 
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   * 
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def pp_block(b: Block): String = b match {
    case Block(Pos(x1, y1), Pos(x2, y2)) => "[" + x1 + "," + y1 + "|" + x2 + "," + y2 + "]"
  }

  def pp_hist(h: List[Move]): String = {
    def go(h: List[Move]): String = h match {
      case Nil => ""
      case x::xs => (x match {
        case Left => "L"
        case Right => "R"
        case Up => "U"
        case Down => "D"
      }) ++ go(xs)
    }
    h.length.toString ++ go(h)
  }

  def pp(s: Stream[(Block, List[Move])]): String = {
    def go(s: Stream[(Block, List[Move])], acc: String): String = s match {
      case Stream() => acc
      case ((b, h)#::xs) => acc + pp_block(b) + ":" + pp_hist(h) + "\n" + go(xs, acc + " ")
    }
    go(s, "")
  }

  def from(initial: Stream[(Block, List[Move])], explored: Set[Block]): Stream[(Block, List[Move])] = {
    println("from < " + pp(initial))
    val (block, history) = initial.last
    val tail = initial.filter { case (_, h) => h == history }
    val newTail = tail.flatMap { case (b, h) => newNeighborsOnly(neighborsWithHistory(b, h), explored)}
    val result = from(initial #::: newTail, explored + block)
    println("from > " + pp(result))
    result
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, List())), Set(startBlock))

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = pathsFromStart.filter { case (b, h) => done(b) }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = pathsToGoal match {
    case Stream() => List()
    case (_, h)#::_ => h 
  }
}
