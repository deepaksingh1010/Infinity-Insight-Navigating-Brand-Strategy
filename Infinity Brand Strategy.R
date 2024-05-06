
# install.packages(c("glmnet"), dependencies = TRUE, repos = "https://cloud.r-project.org")


library(glmnet)
library(readr)
Cars_Data <- read_csv("Cars_Data.csv")
View(Cars_Data)

# read csv file and label the data as "data"
y <-  Cars_Data[[17]]
x <-  as.matrix(Cars_Data[,2:16])
cor_mat = cor(x)									# create correlation matrix



##### Principal Components Analysis #####

out1 <-  eigen(cor_mat)		# eigen decomposition of correlation matrix
va <-  out1$values			# eigenvalues
ve <-  out1$vectors			# eigenvector


##### savings the plot as a pdf file #####
plot(va, ylab = "Eigenvalues", xlab = "Component Nos")				# scree plot
dev.off()

ego <- va[va > 1]							# eigenvalues > 1
nn <- nrow(as.matrix(ego))					# number of factors to retain

out2 <- ve[,1:nn]							# eigenvectors associated with the reatined factors
out3 <- ifelse(abs(out2) < 0.3, 0, out2)		# ignore small values < 0.3

rownames(out3) <- c("attractive", "quiet", "unreliable", "poorly.built", "interesting", "sporty", "uncomfortable", "roomy", "easy.service", "prestige", "common", "economical", "successful", "avantgarde", "poor.value")

# flip the eigenvectors b/c later we will see that regression betas are negative. We flip the eignevector so that the slopes become positive for naming the four benefits

out3[,1] <- (-1)*out3[,1] 
out3[,2] <- (-1)*out3[,2] # assign names based on the rownames and signs of coefficients
z <- x %*% out3			# Component Scores; coordinates of the brands in the map

out5 <- lm(y ~ z)		# Preference Regression to estimate how benefits drive overall preferences = f(benefits)

summary(out5)

## Consider factors z1 and z2 with positive slopes on preferences
## Let z2 be the y-axis and z1 as the x-axis	
## Plot (z2, z1) to get brands in factor space of benefit 1 and benefit 2 

# coordinates of brands in (z1, z2) space

Z1 <- z[,1]
Z2 <- z[,2]
Z3 <- z[,3]
z.out <- cbind(Z1, Z2)
z.out_2 <- cbind(Z2, Z3)
z.out_3 <- cbind(Z1, Z3)

rownames(z.out) = c("infinity", "ford", "audi", "toyota", "eagle", "honda", "saab", "pontiac", "bmw", "mercury")
rownames(z.out_2) = c("infinity", "ford", "audi", "toyota", "eagle", "honda", "saab", "pontiac", "bmw", "mercury")
rownames(z.out_3) = c("infinity", "ford", "audi", "toyota", "eagle", "honda", "saab", "pontiac", "bmw", "mercury")

# Question 1. Build brand maps for car brands. The client's brand is Infinity

plot(Z1, Z2, main = "Brands in Z1 and Z2 space", xlab = "Luxury (Z1)", ylab = "Maintenance (Z2)", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z.out, labels = row.names(z.out), font = 2, cex = 0.5, pos = 1)						# labeling brands

## Z2 and Z3
plot(Z2, Z3, main = "Brands in Z2 and Z3 space", xlab = "Maintenance Z2", ylab = "Value for money  Z3", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z.out_2, labels = row.names(z.out_2), font = 2, cex = 0.5, pos = 1)						# labeling brands


## Z1 and Z3

plot(Z1, Z3, main = "Brands in Z1 and Z3 space", xlab = "Luxury Z1", ylab = "Value for money  Z3", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z.out_3, labels = row.names(z.out_3), font = 2, cex = 0.5, pos = 1)						# labeling brands


# Question 2. How many factors to retain? 
cat("Number of factors retained ",nn)
plot(va, ylab = "Eigenvalues", xlab = "Component Nos")
# Answer - We are retaining 4 eigen values after looking at the scree plot. Using this,we chose 2 factors.

# Question 3. Assign names to the retained factors

# Name of Factor Z1 - Luxury (combination of attrative, quiet, prestige, successfull)
# Name of Factor Z2 - Maintenance factor (combination of unreliable, difficult to service)
# Name of factor Z3 - Value for money (combination of economical, negative of poor value, uninteresting)
# Name of factor Z4 - Unorthodox (combination of avantgarde and uncommon) 

colnames(out3) <- c('Luxury','Maintenance','Value_for_money','Unorthodox')
out3


#Z1 & Z2
# Slopes of iso-preference and ideal vector	
b1.z1.z2 <- as.vector(coef(out5)[2])
b2.z1.z2 <- as.vector(coef(out5)[3])
slope.iso.preference.z1.z2 = - b1.z1.z2/b2.z1.z2						# Why? See slides
slope.ideal.vector.z1.z2 = b2.z1.z2/b1.z1.z2 							# Why? See slides

# Angles of iso-preference and ideal vector	
angle.iso.preference.z1.z2 <- atan(slope.iso.preference.z1.z2)*180/pi	
angle.ideal.vector.z1.z2 <- atan(slope.ideal.vector.z1.z2)*180/pi

#Z1 & Z3
# Slopes of iso-preference and ideal vector	
b1.z1.z3 <- as.vector(coef(out5)[2])
b2.z1.z3 <- as.vector(coef(out5)[4])
slope.iso.preference.z1.z3 = - b1.z1.z3/b2.z1.z3						# Why? See slides
slope.ideal.vector.z1.z3 = b2.z1.z3/b1.z1.z3 							# Why? See slides

# Angles of iso-preference and ideal vector	
angle.iso.preference.z1.z3 <- atan(slope.iso.preference.z1.z3)*180/pi	
angle.ideal.vector.z1.z3 <- atan(slope.ideal.vector.z1.z3)*180/pi

#Z2 & Z3
# Slopes of iso-preference and ideal vector	
b1.z2.z3 <- as.vector(coef(out5)[3])
b2.z2.z3 <- as.vector(coef(out5)[4])
slope.iso.preference.z2.z3 = - b1.z2.z3/b2.z2.z3						# Why? See slides
slope.ideal.vector.z2.z3 = b2.z2.z3/b1.z2.z3 							# Why? See slides

# Angles of iso-preference and ideal vector	
angle.iso.preference.z2.z3 <- atan(slope.iso.preference.z2.z3)*180/pi	
angle.ideal.vector.z2.z3 <- atan(slope.ideal.vector.z2.z3)*180/pi

## Z4 is not significant
## combination for z1 and z2
cat("For combination of Z1 and Z2")
cat("Iso preference line ",angle.iso.preference.z1.z2)
cat("Ideal vector ",angle.ideal.vector.z1.z2)

# For combination of Z2 and Z3
cat("For combination of Z2 and Z3")
cat("Iso preference line ",angle.iso.preference.z2.z3)
cat("Ideal vector ",angle.ideal.vector.z2.z3)

# For combination of Z1 and Z3
cat("For combination of Z1 and Z3")
cat("Iso preference line ",angle.iso.preference.z1.z3)
cat("Ideal vector ",angle.ideal.vector.z1.z3)

out3 <- out3[,1:3]
# Append 5 NA values to the existing y vector
y_expanded <- c(y, rep(NA, 5))

# Now, add y_expanded as a new column to out3
out3 <- cbind(out3, y = y_expanded)
colnames(out3) <- c('attractive','Quiet','Unreliable','y')

# View the updated matrix `out3` to confirm the name change
print(out3)


data_bootstrap <- function(out){
  
  bb = 1000
  nn <- nrow(out)
  ideal_vector <- matrix(0, bb, 3)	
  
  
  
  
  for(ii in 1:bb) {
    
    
    data.star <- out[sample(nn,nn, replace = TRUE),]
    
    data.star <- data.frame(data.star)
    ystar <- data.star$y
    z_1 <- data.star$attractive
    z_2 <- data.star$Quiet
    z_3 <- data.star$Unreliable
    out.star <- lm(ystar ~ z_1 + z_2 + z_3)
    
    b1 <- as.vector(coef(out.star)[2])
    b2 <- as.vector(coef(out.star)[3])
    b3 <- as.vector(coef(out.star)[4])
    
    # Ideal vector slopes in the bootstrap sample
    bootstrap.ideal.vector.Z1.Z2 <- b1 / b2
    bootstrap.ideal.vector.Z2.Z3 <- b2 / b3
    bootstrap.ideal.vector.Z1.Z3 <- b1 / b3
    
    # Calculate the angles and store in the matrix
    ideal_vector[ii, 1] <- ifelse(b1 != 0,  
                                  atan(bootstrap.ideal.vector.Z1.Z2) * 180 / pi,
                                  NA)
    ideal_vector[ii, 2] <- ifelse(b2 != 0,
                                  atan(bootstrap.ideal.vector.Z2.Z3) * 180 / pi,
                                  NA)
    ideal_vector[ii, 3] <- ifelse(b3 != 0,
                                  atan(bootstrap.ideal.vector.Z1.Z3) * 180 / pi,
                                  NA)  
    
    
  }
  
  
  confidence.interval <- apply(ideal_vector, 2, quantile, 
                               probs=c(0.025, 0.975), na.rm=TRUE)
  print(confidence.interval)
  
  
  
  
  
}

data_bootstrap(out3)






  




