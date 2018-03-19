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
      newProb[,c] = 0
      #p is the probability of choosing the next sub solution
      p = (tau[c,]^beta)*(newProb[c,]^alpha)/sum(temp)
      rand = runif(1)
      sum = 0
      for (k in 1:n) {
        sum = sum + p[k]
        if (rand <= sum) {
          starting_nodes[i,j+1]=k
        }
      }
    }
  }
  starting_nodes
}

#Function to calulate the costs of the ants' tours
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
#elimination = 0.97 #common ost elimination

#generate distance between cities matrix
Distances = matrix(0,nrow = no_of_cities,ncol = no_of_cities)
Prob = Distances
for (i in 1:no_of_cities) {
  for (j in 1:no_of_cities) {
    Distances[i,j] = sqrt((X[i]-X[j])^2+(Y[i]-Y[j])^2)
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
#TO DO