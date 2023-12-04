package pp202302.assign3.cellular

/** Wireworld is a 2D cellular automaton with 4 states: empty, conductor,
  * electron head, and electron tail.
  *
  * For each cell, if the cell is empty, then it remains empty at the next time
  * step. If the cell is a conductor, and the number of electron heads in its
  * neighbors is 1 or 2, then the cell becomes an electron head at the next time
  * step. If the cell is an electron head, then it becomes an electron tail at
  * the next time step. If the cell is an electron tail, then it becomes a
  * conductor at the next time step.
  *
  * Reference: https://en.wikipedia.org/wiki/Wireworld
  */
class WireWorld(val map: CellMap) extends CellAutomata(WireWorld.rule):
  def getMap = map

  def createNew(map: CellMap): WireWorld = new WireWorld(map)

  def setStateAt(x: Int, y: Int, state: CellState): WireWorld = {
    val newGrids = map.grids.updated(x,map.grids(x).updated(y,state))
    createNew(CellMap(map.size,newGrids))
  }

  val cellStates = Vector(
      CellState("empty", 0),
      CellState("conductor", 1),
      CellState("electron_head", 2),
      CellState("electron_tail", 3)
    )

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
object WireWorld:
  def initMap(height: Int, width: Int): CellMap =
    CellMap(
      (height, width),
      Vector.fill(height)(Vector.fill(width)(rule.defaultState))
    )

  def apply(height: Int, width: Int) = new WireWorld(initMap(height, width))

  val rule = new CellRule:
    val cellStates = Vector(
      CellState("empty", 0),
      CellState("conductor", 1),
      CellState("electron_head", 2),
      CellState("electron_tail", 3)
    )
    val defaultState = cellStates(0)

    def nextState(
        currState: CellState,
        neighborsStates: Iterable[CellState]
    ): CellState = {
      val neighbors = neighborsStates.map(_.index).toList
      currState.index match {
        case 0 => cellStates(0) 
        case 1 => {
          if (neighbors.count(_ == 2) == 1) || (neighbors.count(_ == 2) == 2) then cellStates(2) 
          else cellStates(1)
        }
        case 2 => cellStates(3)
        case 3 => cellStates(1)
        case _ => cellStates(0)
      }
    }
