rm(list=ls()) #Clear Environment

require(readxl) #needed to import data
Data = read_xlsx(file.choose(),col_names=FALSE) #Choose the data file you want to open
X = Data[2] #X coordinates indexed by city
Y = Data[3] #Y coordinats indexed by city
rm(Data) #remove Data (not needed)

#Normalize points to be between 0 and 1
#This step is done so that our selection probabilities won't all have such 
#large values
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
      #fitness function as defined in Equation 1 of my Documantation
      temp = sqrt(((X[chromosome[j+1],1]-X[chromosome[j],1])^2+(Y[chromosome[j+1],1]-Y[chromosome[j],1])^2)^2)
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
best = mat.or.vec(1,NoOfCities) #vector to keep the best chromosome so far
keep = floor(popSize/2) #no of chromosomes to keep on each iteration
mutationRate = 0.2 #probability of mutation
noMutations = ceiling((popSize-1)*mutationRate) #total number of mutations
Matings = ceiling((popSize-keep)/2) #number of matings
maxit = 400 #maximum number of iterations
bestVal = 999999 #Arbitrary large number

#chromosomes that will survive and mate:
kept = mat.or.vec(keep,NoOfCities)
#stores the probability of each chromosome to survive
prob = mat.or.vec(popSize,1)

#populate initial population with random chromosomes:
for (i in 1:popSize) {
  pop[i,] = sample(1:NoOfCities,NoOfCities) #creates a chromosome
}

#MAIN LOOP:
for (gen in 1:maxit) {
  #compute the fitness function on the population
  Lengths = fitnessFunction(pop)
  #Save the best solution 
  if (min(Lengths)<bestVal) {
    best[1,] = pop[which.min(Lengths),]
    bestVal = min(Lengths)
    print(bestVal)
  }

  #selection
  total = sum(Lengths)
  #Probability of each chromosome to be selected
  for (i in 1:popSize) {
    prob[i] = 1- (Lengths[i]/total)
    #This gives a higher probability value to the shortestt lengths
    #Thus we need to select the chromosomes with the highest probability
  }
  #selects the elements of the population to be kept based on the probability distribution
  #as defined above
  odds = sample(1:popSize,keep,replace=TRUE,prob = prob)
  #choose the chromosomes to be kept and store them in the new population 
  for (i in 1:keep) {
    #keep a record of the parents kept:
    kept[i,] = pop[odds[i],]
    #add them to the new population:
    pop[i,] = kept[i,]
}

  #Thus we have popSize-keep chromosomes left to fill up the new population
  #thus no of matings = (popSize-keep)/2
  index = keep +1
  while (index < popSize) {
    #mate1, mat2 are random integers between 1 and keep (index)
    mate1=ceiling(runif(1, min=0, max=keep-1))
    mate2=ceiling(runif(1, min=0, max=keep-1))
    pointer = ceiling(runif(1,0,NoOfCities)) #random int between 1 and NoOfCities
    children = mate(kept[mate1,],kept[mate2,],pointer) #call the mate function
    #mutate with probability and save to population
    if (runif(1)<=mutationRate) {
      pop[index,] = mutate(children[1,])
    } else {
      pop[index,] = children[1,]
    }
    if (runif(1)<=mutationRate) {
      pop[index+1,] = mutate(children[2,])
    } else {
      pop[index+1,] = children[2,]
    }
    index=index+2
  }

  #Print iteration number
  if (gen%%100==0) {
    print(gen)
  }
}
#compute the fitness function on the population
Lengths = fitnessFunction(pop)
#Save the best solution 
if (min(Lengths)<bestVal) {
  best[1,] = pop[which.min(Lengths),]
  bestVal = min(Lengths)
  print(bestVal)
}