rm(list=ls()) #Clear Environment

require(readxl) #needed to import data
Data = read_xlsx(file.choose(),col_names=FALSE) #Choose the data file you want to open
X = Data[2] #X coordinates indexed by city
Y = Data[3] #Y coordinats indexed by city
rm(Data) #remove Data (not needed)

#Normalize points to be between 0 and 1
#This is an optimisation step to improve the speed of the function
X = X/max(X)
Y = Y/max(Y)

############
#Parameters# <- adjusting these would change the performance of the algorithm
############
no_of_ants = 10 #no of ants in each iteration
max_iter = 3000 #maximum number of iterations
evaporation_rate = 0.15 #evaporation rate of pheromones
alpha = 1 #alpha and beta are paramenters for calculating the prob matrix
beta = 4
q0 = 0.6 #probability of using other ant's experience

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
    sum[j] = sum[j] + Distances[cities[i+1],cities[1]]
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
start_time=as.numeric(Sys.time())*1000;
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

#optTour = c(1,28,6,12,9,5,26,29,3,2,20,10,4,15,18,17,14,22,11,19,25,7,23,27,8,24,16,13,21)#bays29
#optTour = c(1,8,38,31,44,18,7,28,6,37,19,27,17,43,30,36,46,33,20,47,21,32,39,48,5,42,24,10,45,35,4,26,2,29,34,41,16,22,3,23,14,25,13,11,12,15,40,9)#att48
#optTour = c(1,41,39,117,112,115,28,62,105,128,16,45,5,11,76,109,61,129,124,64,69,86,88,26,7,97,70,107,127,104,43,34,17,31,27,19,100,15,29,24,116,95,
#            79,87,12,81,103,77,94,89,110,98,68,63,48,25,113,32,36,84,119,111,123,101,82,57,9,56,65,52,75,74,99,73,92,38,106,53,120,58,49,72,91,6,102,
#            10,14,67,13,96,122,55,60,51,42,44,93,37,22,47,40,23,33,21,126,121,78,66,85,125,90,59,30,83,3,114,108,8,18,46,80,118,20,4,35,54,2,50,130,71)#ch130
optTour = c(1,69,27,101,53,28,26,12,80,68,29,24,54,55,25,4,39,67,23,56,75,41,22,74,72,73,21,40,58,13,94,95,97,87,2,57,15,43,42,14,44,38,86,16,61,85,
            91,100,98,37,92,59,93,99,96,6,89,52,18,83,60,5,84,17,45,8,46,47,36,49,64,63,90,32,10,62,11,19,48,82,7,88,31,70,30,20,66,71,65,35,34,78,
            81,9,51,33,79,3,77,76,50)#eli101
optDistance = 0
for (i in 2:no_of_cities) {
  optDistance = optDistance + Distances[optTour[i-1],optTour[i]]
}
optDistance = optDistance + Distances[optTour[i],optTour[1]]
se = (bestLength-optDistance)^2
iteration = 0

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
    se = c(se,(bestLength-optDistance)^2) #standard error for graph
    iteration = c(iteration,iter)  #iter number for graph
    end_time = as.numeric(Sys.time())*1000
    print(bestLength)
  }
  
  #update pheromone values
  Tau = updatePheromones(routes[bestAntIndex,],lengths[bestAntIndex],evaporation_rate,Tau)
  
  #Stopping Criteria:
  iter = iter+1
  if (iter%%10==0) {print(iter)}
  dontStop = iter<=max_iter
}
paste("Time Taken: ",end_time-start_time)
print(bestRoute)
plot(iteration[-1],se[-1],type="l",main="S.E. vs. Iteration Number",xlab="Iteration",ylab="S.E.")
