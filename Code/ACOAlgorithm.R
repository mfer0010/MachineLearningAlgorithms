rm(list=ls()) #Clear Environment

require(readxl) #needed to import data
Data = read_xlsx(file.choose(),col_names=FALSE) #Choose the data file you want to open
X = Data[2] #X coordinates indexed by city
Y = Data[3] #Y coordinats indexed by city
rm(Data) #remove Data (not needed)

#Function to calculate the tours of the ants for 1 cycle
get_tours <- function(starting_nodes, no_of_ants, no_of_cities, prob,tau, alpha, beta) {
  for (i in 1:no_of_ants) {
    newProb = prob
    for (j in 1:no_of_cities-1) {
      c = starting_nodes[i,j]
      newProb[i,c] = 0
      #p is the probability of choosing the next sub solution
      p = (tau[c,]^beta)*(newProb[c,]^alpha)/sum((tau[c,]^beta)*(newProb[c,]^alpha))
      rand = runif(1)
      sum = 0
      for (k in 1:no_of_cities-1) {
        sum = sum + p[k]
        if (rand <= sum) {
          starting_nodes[i,j+1]=k
        }
      }
    }
  }
  starting_nodes
}

#Function to calulate the costs of the ants' toursr
#TO Do

#Function to update pheromones
#TO DO

###############################################################
#                       Main Function                         #
###############################################################
# This function will use the ACO method to find the shortest  #
# path to solve the symmetric TSP                             #
# TO DO:                                                      #
# -> Explenation of the parts of the algorithm                #
# -> Explenation of the parameters to modify                  #
###############################################################
#parameters:
max_iter = 10
no_of_ants = 10
evaporation_rate = 0.15
alpha = 1
beta = 4
#other variables:
no_of_cities = nrow(X)
Tau = matrix(0.00001,nrow = no_of_cities, ncol = no_of_cities) #initial pheromone matrix
starting_nodes = mat.or.vec(no_of_ants,1)
#elimination = 0.97 #common ost elimination

#generate distance between cities matrix
Distances = matrix(0,nrow = no_of_cities,ncol = no_of_cities)
Prob = Distances
for (i in 1:no_of_cities) {
  for (j in 1:no_of_cities) {
    Distances[i,j] = sqrt((X[i,1]-X[j,1])^2+(Y[i,1]-Y[j,1])^2)
  }
}

#generate probability matrix
for (i in 1:no_of_cities) {
  for (j in 1:no_of_cities) {
    if (Distances[i,j]==0) {
      Prob[i,j] = 0
    } else {
      Prob[i,j] = 1/Distances[i,j]
    }
  }
}

#Main Algorithm Loop:
for (i in 1:max_iter) {
  #select starting nodes for each ant
  starting_nodes = sample(1:no_of_cities,no_of_ants,replace = TRUE)
  routes = matrix(0,nrow = no_of_ants,ncol = no_of_cities)
  for (j in 1:no_of_ants) {
    routes[j,1] = starting_nodes[j]
  }
  routes = get_tours(routes,no_of_ants,no_of_cities,Prob,Tau,alpha,beta)
  
}