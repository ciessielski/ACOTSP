rm(list=ls())
library(dplyr)


#ADJUST ALGORITHM:
alpha <- 1 #pheromone importance (bigger value = bigger priority)
beta <- 5 #distance priority (bigger value = smaller priority)
evaporation <- 0.5
randomness_factor <- 5 #number of random iterrarions

nOfCities <- 10 #number of cities
nOfAnts <- 10 #number of ants
iterations <- 30

cities <- data.frame(cid = c(1:nOfCities),x = abs(rnorm(nOfCities)), y = abs(rnorm(nOfCities)), visited = FALSE, pheromone = 0)
routes <- data.frame(distance = 0)
for (i in 1:nOfCities) { routes[,paste0("stop_",i)] = 0 }

trips <- 0

for (i in 1:iterations)
{
  for (a in 1:nOfAnts)
  {
    trips <- trips + 1
    
    current_city_id <- NULL
    next_city_id <- NULL
    routes[trips, "distance"] <- 0

    stops <- nOfCities+1
    
    for (j in 1:stops) 
    {
      not_visited <- filter(cities, cities$visited == FALSE)
      if (is.null(current_city_id)) { current_city_id <- 1 } #start all trips from the same place
      
      not_visited$dist_from_current <- ((not_visited$x - cities$x["cid" = current_city_id])^2 + (not_visited$y - cities$x["cid" = current_city_id])^2)^(1/2)
      not_visited$rank <- not_visited$dist_from_current / beta + not_visited$pheromone * alpha

      cities$visited[current_city_id] <- TRUE
      cities$pheromone[current_city_id] <- cities$pheromone[current_city_id] + 1
      
      not_visited <- not_visited[-grep(current_city_id,not_visited$cid),]
      routes[trips,paste0("stop_",j)] <- current_city_id
      
      #randomise:
      if (i <= randomness_factor && j < nOfCities)
      {
        if(nrow(not_visited) == 1)
        {
          next_city_id <- not_visited$cid[1]
        }
        else
        {
          next_city_id <- sample(not_visited$cid, 1)
        }
      }
      else
      {
        next_city_id <- not_visited[not_visited$rank==min(not_visited$rank),]$cid
      }
      
      next_city_row <- filter(not_visited, not_visited$cid == next_city_id)
      distance_to_next <- next_city_row$dist_from_current
      
      if (j < nOfCities) 
      { 
        routes[trips, "distance"] <- routes[trips, "distance"] + distance_to_next
        cities$pheromone[current_city_id] <- max(cities$pheromone[current_city_id] - distance_to_next*evaporation, 0)
      }
      current_city_id <- next_city_id
      if (j == nOfCities) { current_city_id <- 1 }
      
    }
    cities$visited <- FALSE
  }
}

print("BEST ROAD FOUND:")
print(routes[routes$distance==min(routes$distance),])
