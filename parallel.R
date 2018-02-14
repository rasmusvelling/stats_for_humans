#####
rm(list=ls())

# Parallel ----
library(foreach)
library(doParallel)

debugLogFile <- paste0("logs/debug_testLoop.txt")
if(file.exists(debugLogFile)) {file.remove(debugLogFile)}

no_cores     <- detectCores()
clust        <- makeCluster(no_cores, outfile=debugLogFile)
registerDoParallel(clust)

dtf <- data.frame(index = 1:10000)

# Loop Par ----
t.start <- Sys.time()
results <- foreach(index = 1:nrow(dtf), 
               .combine = 'rbind') %dopar% 
               { 
                   x1 <- index
                   x2 <- index^2
                   x3 <- index^3
                   x4 <- index^4
                   
                   return(data.frame(x1,x2,x3,x4))
               }

t.end <- Sys.time()
t.end - t.start

# run time 5.6 secs

stopCluster(clust)


# Loop non par ----
t.start <- Sys.time()
results <- foreach(index = 1:nrow(dtf), 
                   .combine = 'rbind') %do% 
                   { 
                       x1 <- index
                       x2 <- index^2
                       x3 <- index^3
                       x4 <- index^4
                       
                       return(data.frame(x1,x2,x3,x4))
                   }

t.end <- Sys.time()
t.end - t.start

# run time 8.3 secs


# Loop standard ----
t.start <- Sys.time()
results <- data.frame(x1 = numeric(), x2 = numeric(), x3 = numeric(), x4 = numeric())
for(index in 1:nrow(dtf))
                   { 
                       x1 <- index
                       x2 <- index^2
                       x3 <- index^3
                       x4 <- index^4
                       
                       results[index,] <- (data.frame(x1,x2,x3,x4))
                   }

t.end <- Sys.time()
t.end - t.start

# run time 10.6 secs
