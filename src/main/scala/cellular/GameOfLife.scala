package pp202302.assign3.cellular

/** Conway's Game of Life is a special case of two-dimensional cellular
  * automaton.
  *
  * For each cell, if the cell is alive, and the number of alive neighbors is 2
  * or 3, then the cell is alive at the next time step. If the cell is dead, and
  * the number of alive neighbors is 3, then the cell is alive at the next time
  * step.
  *
  * Reference: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
  */
class GameOfLife(val map: CellMap) extends CellAutomata(GameOfLife.rule):
  def getMap = map

  val cellStates = Vector(CellState("0", 0), CellState("1", 1))

  def createNew(map: CellMap): GameOfLife = new GameOfLife(map)

  def setStateAt(x: Int, y: Int, state: CellState): GameOfLife = {
    val newGrids = (map.grids.updated(x,map.grids(x).updated(y,state)))
    createNew(CellMap(map.size,newGrids))
  }

  def neighborsAt(x: Int, y: Int): Iterable[CellState] = {
    
    if (x == 0) then {
      if (y == 0) then map.grids(x + 1)(y) :: map.grids(x)(y + 1) :: map.grids(x + 1)(y + 1) :: Nil
      else if (y == map.size._2 - 1) then {
        map.grids(x + 1)(y) :: map.grids(x + 1)(y - 1) :: map.grids(x)(y - 1) :: Nil
      }
      else {
        map.grids(x + 1)(y) :: map.grids(x)(y + 1) :: map.grids(x + 1)(y + 1) ::
        map.grids(x + 1)(y - 1) :: map.grids(x)(y - 1) :: Nil
      }
    }
    else if (y == 0) then {
      if (x == map.size._1 - 1) then map.grids(x - 1)(y) :: map.grids(x - 1)(y + 1) :: map.grids(x)(y + 1) :: Nil
      else map.grids(x + 1)(y) :: map.grids(x)(y + 1) :: map.grids(x + 1)(y + 1) :: map.grids(x - 1)(y) :: map.grids(x - 1)(y + 1) :: Nil
    }
    else if (y == map.size._2 -1) then {
      if (x == map.size._1 - 1) then map.grids(x - 1)(y) :: map.grids(x - 1)(y - 1) :: map.grids(x)(y - 1) :: Nil
      else map.grids(x + 1)(y) :: map.grids(x)(y - 1) :: map.grids(x + 1)(y - 1) :: map.grids(x - 1)(y - 1) :: map.grids(x - 1)(y) :: Nil
    }
    else if (x == map.size._1 -1) then {
      map.grids(x - 1)(y) :: map.grids(x - 1)(y - 1) :: map.grids(x - 1)(y + 1) :: map.grids(x)(y -1) :: map.grids(x)(y + 1) :: Nil
    }
    else {
      map.grids(x - 1)(y - 1) :: map.grids(x - 1)(y) :: map.grids(x - 1)(y + 1) ::
      map.grids(x)(y - 1) :: map.grids(x)(y + 1) ::
      map.grids(x + 1)(y - 1) :: map.grids(x + 1)(y) ::map.grids(x + 1)(y + 1) :: Nil
    }
  }

object GameOfLife:

  val cellStates = Vector(CellState("0", 0), CellState("1", 1))

  def initMap(height: Int, width: Int): CellMap =
    CellMap(
      (height, width),
      Vector.fill(height)(Vector.fill(width)(rule.defaultState)) // cellStates(0) to rule.defaultState
    )

  def apply(height: Int, width: Int) = new GameOfLife(initMap(height, width))
/*
  def sumIndex(neighbors: Iterable[CellState]): Int = {
      neighbors.zipWithIndex.map { case (cellStates, name, index) => cellState.index}.sum
    }
*/
  val rule = new CellRule:
    val cellStates = Vector(CellState("0", 0), CellState("1", 1))
    val defaultState = cellStates(0)

    def nextState(
        currState: CellState,
        neighborsStates: Iterable[CellState]
    ): CellState = {
      val neighbors = neighborsStates.map(_.name.toInt).toList
      currState.index match {
        case 0 => 
          //dead case
          if neighbors.sum == 3 then cellStates(1)
          else cellStates(0)
        
        case 1 => 
          //alive
          if neighbors.sum == 2 || neighbors.sum == 3 then cellStates(1)
          else cellStates(0)
        
        case _ => cellStates(0)
      }
    }
    
