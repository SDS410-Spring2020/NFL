#notes for plan B permutation 
#-- entering in the data

x<-rnorm(100, 0,1)

y<-3*x+rnorm(100, 3, 6)

plot(x,y)

#calculate the stat on our original data
stat<-cor(x,y)
stat

#do many iterations
N<-1000
null.dist<-numeric(N)

for(i in 1:N) {
  permuted.x<- sample(x) #permute SDS vector
  
  null.dist[i] <-cor(permuted.x,y)
  
}


#optional visualization
#make histogram of dist
hist(null.dist, xlim=c(-1,1))
abline(v=stat, col="red")

#find the p-value

# if the diff is positive 
mean(null.dist>stat)*2 

# if the diff is negative 
mean(null.dist<stat)*2 


# unit of travel distance in miles --> white it on the plot 
# add title 

# look at the last score equal, final margin of victory (home score vs visitor score (true or false for win) and margin will be the difference) 

# quantitative variables: 