customDMGroup25 = function(roads, car, packages) {
  # The dimension of the grid
  dimension = nrow(roads[[1]]) 
  
  # GOAL
  #
  # Determine the goal node depending if the car is loaded or not loaded
  goalNode <- createNode()
  if (car$load == 0) {
    # The car is not loaded and we are moving toward closest pick-up node
    # OBS! The closest pick-up node gets recalculated every move
    
    if (is.null(car$mem$packageIndexToPickUp)) {
      packageIndex = getPackageIndexToPickUp(car, packages)
      car$mem <- list(packageIndexToPickUp=packageIndex)
      goalNode$x = packages[packageIndex, 1+0]
      goalNode$y = packages[packageIndex, 2+0]
    } else {
      packageIndex = car$mem$packageIndexToPickUp
      if (packages[packageIndex,5] == 0) {
        goalNode$x = packages[packageIndex, 1+0]
        goalNode$y = packages[packageIndex, 2+0]
      } else {
        packageIndex = getPackageIndexToPickUp(car, packages)
        car$mem <- list(packageIndexToPickUp=packageIndex)
        goalNode$x = packages[packageIndex, 1+0]
        goalNode$y = packages[packageIndex, 2+0]
      }
    }
  } else {
    # The car is loaded and the goal is the drop off node for the specific 
    packageIndex = car$load
    goalNode$x = packages[packageIndex, 1+2]
    goalNode$y = packages[packageIndex, 2+2]
  }
  
  # Set start node
  startNode <- createNode(car$x, car$y, NULL, 5, 0)
  startNode$fCost = 0 + heuristic(startNode, goalNode, roads)
  
  # A* search
  visitedNodes <- aStarSearch(startNode, goalNode, roads, dimension)
  
  # Determine next move
  car$nextMove = 5
  if (length(visitedNodes) == 0) {
    print("ERROR - length(visitedNodes) = 0")
  } else if (length(visitedNodes) == 1) {
      visitedNode <- visitedNodes[1]
      if (is.null(visitedNode$parentNode) == FALSE) {
        car$nextMove = visitedNode$nextMove
      } else {
        print("ERROR - length(visitedNodes) = 1")
      }
  } else {
    for (visitedNode in visitedNodes) {
      if (is.null(visitedNode$parentNode) == FALSE) {
        if (isSameNode(visitedNode$parentNode, startNode) == TRUE) {
          car$nextMove = visitedNode$nextMove
          break
        }
      }
    }
    if (car$nextMove == 5) {
      print("ERROR - length(visitedNodes) > 1")
    }
  }
  
  return (car)
}

# A* Search
aStarSearch <- function(startNode, goalNode, roads, dimension) {
  openList = list()
  closedList = list()
  
  # Add start node  to open list
  openList[[length(openList)+1]] <- startNode
  
  # Main loop of A* search
  while (length(openList) > 0) {
    currentNode <- getNodeWithLowestFCost(openList)
    
    # Add current node to closeList 
    closedList[[length(closedList)+1]] <- currentNode
    
    # Check if we are at the goal (i.e. currentNode = goalNode)
    if (isSameNode(currentNode, goalNode) == TRUE) {
      break
    }
    
    # Remove current node from openList
    newOpenList <- list()
    for (openNode in openList) {
      if (isSameNode(openNode, currentNode) == TRUE) {
        next
      }
      newOpenList[[length(newOpenList)+1]] <- openNode
    }
    openList <- newOpenList
    
    # Find neighbours
    neighbourList <- getNeighbourNodes(dimension, currentNode)
    for (neighbourNode in neighbourList) {
      hasVisited = FALSE
      for (closedNode in closedList) {
        if (isSameNode(neighbourNode, closedNode) == TRUE) {
          hasVisited = TRUE
          break
        }
      }
      if (hasVisited == FALSE) {
        gCost <- calcGCost(neighbourNode, roads)
        hCost <- heuristic(neighbourNode, goalNode, roads)
        fCost <- gCost + hCost
        newOpenNode <- createNode(neighbourNode$x, neighbourNode$y, currentNode, neighbourNode$nextMove, fCost)
        openList[[length(openList)+1]] <- newOpenNode
      }
    }
  }
  return(closedList)
}

# Calculate the g cost for a node given current traffic conditions
calcGCost <- function(currentNode, roads) {
  gCostSum <- 0
  while(is.null(currentNode$parentNode) == FALSE) {
    gCostSum <- gCostSum + trafficBetween(currentNode$parentNode, currentNode, roads)
    currentNode <- currentNode$parentNode
  }
  return(gCostSum)
}

# Checks if two nodes are the same (by x,y position)
isSameNode <- function(nodeA, nodeB) {
  if (nodeA$x  == nodeB$x) {
    if (nodeA$y == nodeB$y) {
      return (TRUE)
    }
  }
  return (FALSE)
}

# Returns the node with lowest f cost
getNodeWithLowestFCost = function(openList) {
  minfCost = -1
  for (candidate in openList) {
    if (minfCost == -1) {
      minfCost = candidate$fCost
      bestCandidate <- candidate
    } else {
      if (candidate$fCost < minfCost) {
        minfCost = candidate$fCost
        bestCandidate <- candidate
      }
    }
  }
  return(bestCandidate)
}

# Find neighbour nodes
getNeighbourNodes = function(dimension, currentNode) {
  neighbourNodes = list()
  
  # Right neighbour
  if (currentNode$x+1 <= dimension) {
    neighbourNodes[[length(neighbourNodes)+1]] <- createNode(currentNode$x+1, currentNode$y, currentNode, 6)
  }
  # Left neighbour
  if (currentNode$x-1 >= 1) {
    neighbourNodes[[length(neighbourNodes)+1]] <- createNode(currentNode$x-1, currentNode$y, currentNode, 4)
  }  
  # Up neighbour  
  if (currentNode$y+1 <= dimension) {
    neighbourNodes[[length(neighbourNodes)+1]] <- createNode(currentNode$x, currentNode$y+1, currentNode, 8)
  }
  # Down neighbour
  if (currentNode$y-1 >= 1) {
    neighbourNodes[[length(neighbourNodes)+1]] <- createNode(currentNode$x, currentNode$y-1, currentNode, 2)
  }
  
  return(neighbourNodes)
}

# Get next package to pick up considering which one is the closest (by Manhattan distance) to the current position of the car
getPackageIndexToPickUp <- function(car, packages) {
  packageNode = createNode()
  closestPackageIndex = -1
  minManhattanDist = -1
  
  for (index in c(1,2,3,4,5)) {
    if (packages[index,5] == 0) {
      packageNode$x = packages[index,1]
      packageNode$y = packages[index,2]
      if (minManhattanDist == -1) {
        minManhattanDist = manhattanDistance(car, packageNode)
        closestPackageIndex = index
      } else {
        if (manhattanDistance(car, packageNode) < minManhattanDist) {
          minManhattanDist = manhattanDistance(car, packageNode)
          closestPackageIndex = index
        }
      }
    }
  }
  return(closestPackageIndex)
}

# Get the traffic between two nodes
trafficBetween <- function(parentNode, neighbourNode, roads) {
  traffic <- 0
  if (parentNode$x == neighbourNode$x){
    traffic <- roads[["vroads"]][min(parentNode$y, neighbourNode$y), parentNode$x]
  }
  else if (parentNode$y == neighbourNode$y){
    traffic <- roads[["hroads"]][parentNode$y, min(parentNode$x, neighbourNode$x)]
  }
  return (traffic)
}

# Calculate the Manhattan distance between two nodes
heuristic <- function(nodeA, nodeB, roads) {
  hroads <- roads[["hroads"]]
  vroads <- roads[["vroads"]]
  
  minHRoads = hroads[which.min(hroads)]
  minVRoads = vroads[which.min(vroads)]
  if (minHRoads < minVRoads) {
    lowestTrafficFactor <- minHRoads
  } else {
    lowestTrafficFactor <- minVRoads
  }
  
  # The Manhattan distance 
  manhattanDist = manhattanDistance(nodeA, nodeB)
  
  # Dilation of the Manhattan distance 
  resultValue = manhattanDist*lowestTrafficFactor
  return (resultValue)
}

# Calculate the Manhattan distance between two nodes
manhattanDistance <- function(nodeA, nodeB) {
  dx <- abs(nodeA$x - nodeB$x)
  dy <- abs(nodeA$y - nodeB$y)
  return (dx + dy)
}

# Create and return a empty node
createNode = function(thisX=0, thisY=0, thisParentNode=NULL, thisNextMove=5, thisFCost=0) {
  node <- list(x=thisX, y=thisY, parentNode=thisParentNode, nextMove=thisNextMove, fCost=thisFCost)
  return(node)
}