library(tidyverse)

#### MPG Regression ####


#Load the MechaCar_mpg dataset.
ds <- 'MechaCar_mpg.csv'
mecha_data <- read.csv(ds)

#Check the first few rows of the dataset.
head(mecha_data)

#Do a quick qualitative check for a normal distribution.
ggplot(mecha_data, aes(x=mpg)) + geom_density()
ggplot(mecha_data, aes(x=vehicle.length)) + geom_density()
ggplot(mecha_data, aes(x=vehicle.weight)) + geom_density()
ggplot(mecha_data, aes(x=spoiler.angle)) + geom_density()
ggplot(mecha_data, aes(x=ground.clearance)) + geom_density()
ggplot(mecha_data, aes(x=AWD)) + geom_density() 

# Perform Multiple Linear Regression Analysis
# Use vehicle.length, vehicle.weight, spoiler.angle, ground.clearance, and AWD
MLR <- lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data = mecha_data)
 
#Obtain our statistical metrics using the summary() function.
summary(MLR)
 
# Perform  2nd Multiple Linear Regression Analysis
MLR1 <- lm(mpg ~ vehicle.length + ground.clearance , data = mecha_data)

#Obtain our statistical metrics using the summary() function.
summary(MLR1)

#### Suspension Coil Summary ####

#Load the suspension coil test data.
coil_ds <- 'Suspension_Coil.csv'
s_coil <- read.csv(coil_ds)

#Do a quick qualitative check for a normal distribution.
ggplot(s_coil, aes(x=PSI)) + geom_density()

#Check the first few rows of the dataset.
head(s_coil)

#Calculate, mean, median, standard deviation and variance
summary_coil <- s_coil%>% summarise(Mean=mean(PSI),Median=median(PSI),SD=sd(PSI),Variance=var(PSI))

#Print the summary_coil table.
print(summary_coil)


#Calculate, mean, median, standard deviation and variance by Lot type.
summary_byLot <- s_coil%>% group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI),Median=median(PSI),SD=sd(PSI),Variance=var(PSI))

#Print the summary_byLot table.
print(summary_byLot)

#### T-Test #####

#Perform a two tailed t test with mu=1500.
t_result<- t.test(s_coil$PSI,mu=1500)


#Print t.test result.
print(t_result)
