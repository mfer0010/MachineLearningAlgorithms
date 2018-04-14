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

#fitnessFunction:
#takes a population of chromosomes in Matrix form as an argument
#returns a vector of distances each element corresponding to a chromosome
#Example of use:
#a = c(1,2,3,4,5,6,7)
#b = c(2,4,3,1,5,15,12)
#c = c(10,2,3,1,5,6,11)
#pop = t(cbind(a,b,c))
#distances=fitnessFuntion(pop)
fitnessFunction <- function(pop) {
  Npop = nrow(pop)#size of population
  Ncity = ncol(pop)#no. of cities
  #append first city to the end in order to be able to calculate the distance
  tour = cbind(pop,pop[,1]) 
  #Empty vector to store distances of each chromosome
  distances = mat.or.vec(Npop,1)
  
  #loop over every tour in the population
  for (i in 1:Npop) {
    chromosome = tour[i,]
    #for every city in the chromosome
    distance = 0
    for (j in 1:Ncity){
      #fitness function as defined in Equation 1 of my Documentation
      temp = sqrt(((X[chromosome[j+1],1]-X[chromosome[j],1])^2+(Y[chromosome[j+1],1]-Y[chromosome[j],1])^2))
      distance = distance + temp
    }
    distances[i]=distance
  }
  distances
}

#mate function, mates 2 chromosomes given a pointer as
#the starting pointer and returns a list of the 2 new 
#child chromosomes
mate <- function(mate1,mate2,pointer) {
  temp = mate1
  change = TRUE
  #loop until all values are unique
  while (change) {
    #swap values
    mate1[pointer]=mate2[pointer]
    mate2[pointer]=temp[pointer]
    #returns an array of indices which match the value at mate1[pointer]:
    pointers = which(mate1==mate1[pointer],arr.ind = TRUE) 
    #check if there exists an duplicate in the chromosome
    change = FALSE
    tempPointer = pointer
    for (j in 1:length(pointers)) {
      if (pointers[j]!=pointer) {
        #if there's a duplicate, point to it
        tempPointer = pointers[j]
        change = TRUE
      }
    }
    pointer = tempPointer
  }
  return(unname(rbind(mate1,mate2)))
}

#function that mutates the chromosome
mutate <- function(chromosome) {
  indices = sample(1:length(chromosome),2,replace=FALSE) #choose 2 random indices in the chromosome
  #swap the elements
  swapTemp = chromosome[indices[1]]
  chromosome[indices[1]] = chromosome[indices[2]]
  chromosome[indices[2]] = swapTemp
  return(chromosome)
}


#Main Function:
#Initialize Genetic Algorithm Parametes:
NoOfCities = nrow(X) #Number of Cities
popSize = 10 #no of chromosomes in each population
pop = mat.or.vec(popSize,NoOfCities)
pop2 = mat.or.vec(popSize,NoOfCities)
best = mat.or.vec(1,NoOfCities) #vector to keep the best chromosome so far
keep = 6 #no of chromosomes to be chosen as parents
mutationRate = 0.4 #probability of mutation
#noMutations = ceiling((popSize-1)*mutationRate) #total number of mutations
Matings = ceiling((popSize-keep)/2) #number of matings
maxit = 5000 #maximum number of iterations

#optTour = c(1,28,6,12,9,5,26,29,3,2,20,10,4,15,18,17,14,22,11,19,25,7,23,27,8,24,16,13,21)#bays29
#optTour = c(1,8,38,31,44,18,7,28,6,37,19,27,17,43,30,36,46,33,20,47,21,32,39,48,5,42,24,10,45,35,4,26,2,29,34,41,16,22,3,23,14,25,13,11,12,15,40,9)#att48
optTour = c(1,41,39,117,112,115,28,62,105,128,16,45,5,11,76,109,61,129,124,64,69,86,88,26,7,97,70,107,127,104,43,34,17,31,27,19,100,15,29,24,116,95,
            79,87,12,81,103,77,94,89,110,98,68,63,48,25,113,32,36,84,119,111,123,101,82,57,9,56,65,52,75,74,99,73,92,38,106,53,120,58,49,72,91,6,102,
            10,14,67,13,96,122,55,60,51,42,44,93,37,22,47,40,23,33,21,126,121,78,66,85,125,90,59,30,83,3,114,108,8,18,46,80,118,20,4,35,54,2,50,130,71)#ch130
optDistance = 0
for (i in 2:NoOfCities) {
  optDistance = optDistance + sqrt((X[optTour[i-1],1]-X[optTour[i],1])^2+(Y[optTour[i-1],1]-Y[optTour[i],1])^2)
}

bestVal = 1e+22 #Arbitrary large number
se = (bestVal-optDistance)^2
iteration = 0

#chromosomes that will survive and mate:
kept = mat.or.vec(keep,NoOfCities)
#stores the probability of each chromosome to survive
prob = mat.or.vec(popSize,1)

#populate initial population with random chromosomes:
for (i in 1:popSize) {
  pop[i,] = sample(1:NoOfCities,NoOfCities) #creates a chromosome
}

#set starting time:
start_time = as.numeric(Sys.time())*1000;
#MAIN LOOP:
for (gen in 1:maxit) {
  #compute the fitness function on the population
  Lengths = fitnessFunction(pop)
  #Save the best solution 
  if (min(Lengths)<bestVal) {
    best[1,] = pop[which.min(Lengths),]
    bestVal = min(Lengths)
    se = c(se,(bestVal-optDistance)^2) #standard error for graph
    iteration = c(iteration,gen)  #iter number for graph
    end_time=as.numeric(Sys.time())*1000
    print(bestVal)
  }

  #selection
  total = sum(Lengths)
  #Probability of each chromosome to be selected
  for (i in 1:popSize) {
    prob[i] = 1- (Lengths[i]/total)
    #This gives a higher probability value to the shortest lengths
    #Thus we need to select the chromosomes with the highest probability
  }
  #selects the elements of the population to be kept based on the probability distribution
  #as defined above
  odds = sample(1:popSize,keep,replace=TRUE,prob = prob)
  #choose the chromosomes to be kept and store them in the new population 
  #keep the best and second best solutions
  pop2[1,]=pop[which.min(Lengths),]
  pop2[2,]=pop[which(Lengths==sort(Lengths,decreasing=TRUE)[popSize-1])[1],]
  #choose parents, we'll choose 3 mums and 3 dads, to have 6 kids:
  for (i in 1:keep) {
    #keep a record of the parents kept:
    kept[i,] = pop[odds[i],]
  }

  index = 3
  while (index < 9) {
    #mate1, mate2 are random integers between 1 and keep (index)
    mate1=ceiling(runif(1, min=0, max=keep-1))
    mate2=ceiling(runif(1, min=0, max=keep-1))
    pointer = ceiling(runif(1,0,NoOfCities)) #random int between 1 and NoOfCities
    children = mate(kept[mate1,],kept[mate2,],pointer) #call the mate function
    #mutate with probability and save to population
    if (runif(1)<=mutationRate) {
      pop2[index,] = mutate(children[1,])
    } else {
      pop2[index,] = children[1,]
    }
    if (runif(1)<=mutationRate) {
      pop2[index+1,] = mutate(children[2,])
    } else {
      pop2[index+1,] = children[2,]
    }
    index=index+2
  }
  #Randomly fill remaining part of population with chromosomes
  for (i in 9:10) {
    pop2[i,] = sample(1:NoOfCities,NoOfCities) #creates a chromosome
  }

  #Print iteration number
  if (gen%%100==0) {
    print(gen)
  }
  pop = pop2
}
#compute the fitness function on the population
Lengths = fitnessFunction(pop)
#Save the best solution 
if (min(Lengths)<bestVal) {
  best[1,] = pop[which.min(Lengths),]
  bestVal = min(Lengths)
  se = c(se,(bestVal-optDistance)^2) #standard error for graph
  iteration = c(iteration,gen)  #iter number for graph
  end_time=as.numeric(Sys.time())*1000
  print(bestVal)
}
paste("Time Taken: ",end_time-start_time)
print(best[1,])
plot(iteration[-1],se[-1],type="l",main="S.E. vs. Iteration Number",xlab="Iteration",ylab="S.E.")
