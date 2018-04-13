#Group 25 - Wheres Croc Game

#Use the main function "goodMove" as the MakeMoves function

#In order to run the code, the R igraph package is needed to be installed 


#transaction matrix
#row i, value for this row is the possibility of connect to node i
getMatrixT <- function(edges=getEdges(),moveInfo){
  T = matrix(nrow=40, ncol=40)
  neighbor = list()
  for(i in 1:dim(T)[1]){
    neighbor = getOptions(i,edges)
    len=length(neighbor)
    p = 1/len
    #print(p)
    for (j in 1: dim(T)[2]){
      #print("neighbor again")
      #print(neighbor)
      if(j %in% neighbor){
        T[i,j] <- p 
      }
      else
        T[i,j] = 0
    }
  }
  return (T)
}

#initial states
initialState <- function(){
  states =list()
  for (i in 1:40)
    states[i] = 1/40
  return (states)
}

# get Emission matrix
getEmission <- function(readings, waterholeInfo){
  probs = list()
  distributions = list()
  # Get each waterhole normal distribution, each mean and standard deviation
  distributions = waterholeInfo
  #print(distributions)
  for (i in 1:40){
    
    salinityMean = distributions$salinity[[i,1]]
    salinityStd = distributions$salinity[[i,2]]
    salinityProb = dnorm(readings[1], salinityMean, salinityStd)
    
    phosphateMean = distributions$phosphate[[i,1]]
    phosphateStd = distributions$phosphate[[i,2]]
    phosphateProb = dnorm(readings[2], phosphateMean, phosphateStd)
    
    nitrogenMean = distributions$nitrogen[[i,1]]
    nitrogenStd = distributions$phosphate[[i,2]]
    nitrogenProb = dnorm(readings[3], nitrogenMean, nitrogenStd)
  
    probs[i] = salinityProb*phosphateProb*nitrogenProb
  }
  return (probs)
}

# get normalized states
# states is a list. normalization function input is a list, its output is a normalized list
normalization <- function(states){
  m = do.call(sum, states)
  len = length(states)
  for (i in 1:len){
    r = as.numeric(as.character(unlist(states[i])))
    states[i] = r/m
  }
  return (states)
}

#previous states and T are saved in moveInfo$mem
# states list *Transcation matrix*Emission list
getCurrentStates<-function(moveInfo, positions, emission){
  currentStates = list()
  
  # For the first move (i.e. initial states with the backpackers' positions taken into account)
  if(length(moveInfo$mem[[1]]) == 0){
    moveInfo$mem[[1]] = rep(list(0), 40)
    # Keeps track of the total number of waterholes where the croc can be
    counter = 0
    for (i in 1:40){
      if ((!is.na(positions[1]))&&(!is.na(positions[2]))) {
        if ((positions[1] == i)||(positions[2] == i)) {
          moveInfo$mem[[1]][[i]] = 0
          next()
        }
      }
      moveInfo$mem[[1]][[i]] = 1
      counter = counter + 1
    }
    moveInfo$mem[[1]] = lapply(moveInfo$mem[[1]], function(x) x/counter)
  }
  
  # If backpacker 1 was eaten this turn
  if ((!(is.na(positions[1])))&&(positions[1] < 0)) {
    crocPoss = -positions[1]
    for (i in 1:40) {
      moveInfo$mem[[1]][[i]] = 0
    }
    moveInfo$mem[[1]][[crocPoss]] = 1
    print("Backpacker eaten!")
  }
  # If backpacker 2 was eaten this turn
  else if ((!(is.na(positions[2])))&&(positions[2] < 0)){
    crocPoss = -positions[2]
    for (i in 1:40) {
      moveInfo$mem[[1]][[i]] = 0
    }
    moveInfo$mem[[1]][[crocPoss]] = 1
    print("Backpacker eaten!")
  }
  # If both backpackers still alive, calculate state estimation with forward algorithm
  else {
    tmp = 0
    for (i in 1:40){
      for (j in 1:40){
        tmp = tmp + (moveInfo$mem[[1]][[j]] * moveInfo$mem[[2]][j,i])
      }
      currentStates[[i]] = tmp
      currentStates[[i]] = currentStates[[i]] * emission[[i]]
    }
    currentStates = normalization(currentStates)
    moveInfo$mem[[1]] = currentStates
  }
  
  return (moveInfo$mem[[1]])
}
#get max value in state list
# which.max() function return the first max value index
# the index is the waterpole ID
getMaxValue <- function(moveInfo){
  index = which(moveInfo==max(unlist(moveInfo)))
  print("Waterhole with highest probability")
  print(index)
  print(max(unlist(moveInfo)))
  cat("\n")
  return (index)
}

getShortestPath = function(edges=getEdges(), nodeA, nodeB) {
  graph = graph_from_edgelist(edges, directed=FALSE)
  pathInfo = shortest_paths(graph, nodeA, nodeB)
  path = unlist(pathInfo$vpath)
  return (path)
}

#
goodMove <-function(moveInfo, readings, positions, edges, probs){
  path = list()
  tMatrix = matrix()
  
  if ((length(moveInfo$mem) == 0)&&(is.null(moveInfo$moves))){
    tMatrix = getMatrixT()
    moveInfo$mem[[2]] = tMatrix
  }
  
  current = getCurrentStates(moveInfo, positions, getEmission(readings, probs))
  
  currentPosition = positions[3]
  max = getMaxValue(current)
  
  print("Current player position")
  print(currentPosition)
  cat("\n")
  
  path = getShortestPath(edges, currentPosition, max)
  print("Shortest path from player possition to max node")
  print (path)
  cat("\n")
  
  # need no movement to possible croc positin
  len = length(path)
  if(len == 1){
    mv1 = 0
    mv2 = 0
  }
  
  # need one move to the possible croc position
  if(len == 2){
    mv1 = path[[2]]
    mv2 = 0
  }
  
  if(len > 2){
    mv1 = path[[2]]
    mv2 = path[[3]]
  }
  
  moveInfo$mem[[1]] = current
  moveInfo$moves=c(mv1,mv2)
  #currentPosition = mv2
  return (moveInfo)
}