install.packages("scales")
install.packages("hash")
library(scales)
library(hash)
print("hello")

#Traditional calculation approach
pv <- 10000
time_horizontal <- 30
i <- 0.07
addition <- 10000
ending <- 0

for (x in 1:30){
  ending <- pv * (1+i) + addition
  ending_incurrency <-label_currency()(ending)
  #print(ending_incurrency)
  pv <- ending
  
}
#Generating one possible future values based on market history using the expected return 9% with 18% volatility
volatility <- 0.18
expectreturn <- 0.09
addition <-10000
pv <- 10000
return <- list()
fvlist <- list()
library(scales)
for (x in 1:30){
  market_return <- rnorm(1,expectreturn,volatility)
  fv <- pv*(1+market_return) + addition
  return <- append(return,round(market_return,4))
  fvv <- label_currency()(fv)
  fvlist <- append(fvlist,fvv)
  pv <- fv
  
}
asso_arr <- setNames(as.list(fvlist),return)  #tuples or associative array in R
print(asso_arr)

#Simulate porfolio ending market values
time <- 30
it <- 5
volatility <- 0.18
expectreturn <- 0.09
addition <-10000
pv <- 10000
stream = list()

#df created

df <- data.frame()
vector_list <- list()

for (x in 1:it){ #1:it
  
  vec <- c() #each vector is a column in df
  
 for (i in 1:time){
  rate <- rnorm(1,expectreturn,volatility)
  end <- round(pv*(1+rate)+addition,3)
  vec <- append(vec,end)
  #stream <- append(stream,end)
  pv<- end
 
}
  stream <- append(stream,vec)
  
  vector_name <- paste("vector", x, sep = "_")
  vector_list[[vector_name]] <- vec  # Create vector after each iteration
}

print(stream)

# Print the list of named vectors
print(vector_list)
print(vector_list[1])
vec1 <- unlist(vector_list[1])
vec2 <- unlist(vector_list[2])
vec3 <- unlist(vector_list[3])
index <- c(1:time)
df2 <- data.frame(
  index,
  vec1,
  vec2,
  vec3,
  
  stringsAsFactors = TRUE
)
print(df2)   #have not found a way to convert a list of vector into dataframe in which each vector is a column by looping




# Calculate summary statistics
numeric_vector <- as.numeric(unlist(stream))
summary_stats <- summary(numeric_vector)

# Print the summary statistics
print(summary_stats)
cat("MAX: ", max(numeric_vector))
cat("MIN: ", min(numeric_vector))
cat("MEAN: ", mean(numeric_vector))
cat("STDEV: ", sd(numeric_vector))

#plot 
install.packages("ggplot2")
library(ggplot2)

x <- df2$index
y <- df2$vec1
y2 <- df2$vec2
y3 <- df2$vec3
plot(x,y,type = "l", col = "red", pch="o", ylab="y", lty=1)
points(x, y2, col="blue", pch="*")
lines(x, y2, col="blue",lty=2)
points(x, y3, col="green", pch="v")
lines(x, y3, col="green",lty=3)