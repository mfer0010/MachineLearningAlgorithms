rm(list=ls()) #Clear Environment

require(readxl) #needed to import data
Data = read_xlsx(file.choose(),col_names=FALSE) #Choose the data file you want to open
X = Data[2] #X coordinates indexed by city
Y = Data[3] #Y coordinats indexed by city
rm(Data) #remove Data (not needed)

############
#Parameters# <- adjusting these would change the performance of the algorithm
############
no_of_ants = 10 #no of ants in each iteration
max_iter = 100 #maximum number of iterations
evaporation_rate = 0.15 #evaporation rate of pheromones
alpha = 1 #alpha and beta are paramenters for calculating the prob matrix
beta = 4
q0 = 0.7 #probability of using other ant's experience
###########
#Functions#
###########
#this function returns the reciprocal of a number, unless that number is 0
probValues <- function(number) {
  if (number == 0) {
    number
  } else {
    1/number
  }
}

#function that accepts a matrix with the first column being the starting point of each ant and the other columns all being 0
#a Heuristic Matrix and the pheromone value Matrix Tau and returns tours of each ant.
setRoutes <- function(Routes, Heuristic, Tau) {
  #for every ant:
  for (i in (1:nrow(Routes))) {
    Memory = mat.or.vec(ncol(Routes),1) #Set Ant's memory to 0s 
    Score = (Tau^alpha)*(Heuristic)^beta #Calculate score matrix
    Memory[1] = Routes[i,1] #Add starting node to memory 
    #Loop until all cities have been visited:
    for (j in 2:ncol(Routes)) {
      currentCity = Memory[j-1]
      #set score to 0 for current city:
      Score[,currentCity] = 0
      ParticularScore = Score[currentCity,]
      if (runif(1)<=q0) {
        #find the city with the largest score:
        Memory[j] = which.max(ParticularScore)
      } else {
        #Choose the next node based on probability
        Probability = ParticularScore/sum(ParticularScore)
        Memory[j] = sample(1:length(Probability),1,prob=Probability)
      }
    }
    Routes[i,] = Memory
  }
  Routes
}

#function that calculates the length of each tour
fitnessFunction <- function(Routes, Distances) {
  sum = mat.or.vec(nrow(Routes),1)
  #for each ant j
  for (j in 1:nrow(Routes)) { 
    cities = Routes[j,]
    #for each city visited
    for (i in 1:(ncol(Routes)-1)) {
      #add up the distances
      sum[j] = sum[j] + Distances[cities[i],cities[i+1]]
    }
  }
  sum
}

#function that updates pheromones after an iteration
updatePheromones <- function(path, length, evaporation_rate, Tau) {
  for (i in 1:(length(path)-1)) {
    Tau[path[i],path[i+1]] = (1-evaporation_rate)*Tau[path[i],path[i+1]]+evaporation_rate*(length)^(-1);
    Tau[path[i+1],path[i]] = Tau[path[i],path[i+1]] 
  }
  Tau
}

#################
#Other Variables#
#################
no_of_cities = nrow(X)
Tau = matrix(0.00001,nrow = no_of_cities, ncol = no_of_cities) #initial pheromone matrix
starting_nodes = mat.or.vec(no_of_ants,1)
dontStop = TRUE
#generate distance between cities matrix
Distances = matrix(0,nrow = no_of_cities,ncol = no_of_cities)
Heuristic = Distances
iter = 0
#Note that Distances is a symmetric matrix since we're working out the Symetric TSP
#, thus in order to improve complexity first the lower triangular part is worked out, 
#then its transpose is added to itself.
for (i in 2:no_of_cities) {
  for (j in 1:i) {
    Distances[i,j] = sqrt((X[i,1]-X[j,1])^2+(Y[i,1]-Y[j,1])^2)
  }
}
Heuristic = apply(Distances,c(1,2),probValues) #Genarate the Heuristic Matrix (see function probValues)
Distances = Distances + t(Distances)
Heuristic = Heuristic + t(Heuristic)

bestRoute = mat.or.vec(no_of_cities,1)
bestLength = 1e27 #Arbitrarily large number

#####################
#Main Algorithm Loop#
#####################
while (dontStop) {
  #initialize each ant in a starting node
  starting_nodes = sample(1:no_of_cities,no_of_ants,replace = TRUE)
  routes = matrix(0,nrow = no_of_ants,ncol = no_of_cities) #routes will store the routes of the ants
  routes[,1] = starting_nodes
  
  #generate the routes for each ant
  routes = setRoutes(routes,Heuristic,Tau)
  
  #build a solution
  lengths = mat.or.vec(no_of_ants,1)
  lengths = fitnessFunction(routes, Distances)
  
  #save best route
  bestAntIndex = which.min(lengths)
  if (lengths[bestAntIndex]<bestLength) {
    bestLength = lengths[bestAntIndex]
    bestRoute = routes[bestAntIndex,]
    print(bestLength)
  }
  
  #update pheromone values
  Tau = updatePheromones(routes[bestAntIndex,],lengths[bestAntIndex],evaporation_rate,Tau)
  
  #Stopping Criteria:
  iter = iter+1
  if (iter%%10==0) {print(iter)}
  dontStop = iter<=max_iter
}