library(dplyr)
library(xml2)
library(rvest)
library(selectr)
library(ggplot2)

# Batsman's data

link1 <- read_html("http://stats.espncricinfo.com/ci/engine/records/batting/most_runs_career.html?id=117;type=trophy")
tab1 <- link1 %>%
html_nodes(xpath='//*[@id="ciHomeContentlhs"]/div[3]/div/table[1]') %>%
html_table()
write.csv(tab1, "data1.csv")
tab1 <-data.frame(tab1)
even_r <- seq(2,100,2)
tab1 <- tab1 [-even_r,]
write.csv(tab1,"data.csv")

#Cleaning
x1 <- read.csv("data.csv")
x1$X <- NULL
write.csv(x1,"data.csv")

# Batsman with highest runs
x1[which.max(x1$Run),]

# Batsman with most innings played
x1[which.max(x1$Inns),]

# Batsman with most number of 50's
x1[which.max(x1$X50),]

# Batsman with most number of 100's
x1[which.max(x1$X100),]

# Batsman with best strike rate
x1[which.max(x1$SR),]

# Batsman with best average
x1[which.max(x1$Ave),]

# Batsman with worst average
x1[which.min(x1$Ave),]

# Mean of runs scored
mean(x1$Runs)

# Variance of runs scored
var(x1$Runs)

# Standard deviation of runs scored
sd(x1$Runs)

# Mean of Average
mean(x1$Ave)

# Variance of Average
var(x1$Ave)

# Standard deviation of Average
sd(x1$Ave)

# Mean of Strike Rate
mean(x1$SR)

# Variance of Strike Rate
var(x1$SR)

# Standard deviation of Strike Rate
sd(x1$SR)

# Mean of fours
mean(x1$X4s)

# Variance of fours
var(x1$X4s)

# Standard deviation of X4s
sd(x1$X4s)
# ----------------------------------------------------------------------------------------

# Plotting of graph 1

# Converting Player names to data frame
player_name <- data.frame(x1$Player)

# Replicating top 5 player names 3 times
player_name <- c(as.character(rep(player_name[1,] , 3)) ,as.character(rep(player_name[2,] , 3)), as.character(rep(player_name[3,] , 3)), as.character(rep(player_name[4,] , 3)), as.character(rep(player_name[5,] , 3)))

# Replicating their 3 attributes (5 players)
parameters <- rep(c("Runs(Total runs/100)" , "Average" , "Strike Rate") , 5)

# Their values for these attributes
# Converting attributes of players to data frame
runs_scored <- data.frame(x1$Runs*0.01) # Multiplying by 0.01 to bring to the same range as avg
# So 5000 runs is equivalent to 5000/100=50 makes it easier to show plot
average_player <- data.frame(x1$Ave)
strike_rate <- data.frame(x1$SR)
# Combining values
# The below statements will combine a player's runs, average and strike rate and repeat for all
i <- seq(1:5)
values <-NULL
for (x in i){values <- c(values, runs_scored[x,], average_player[x,], strike_rate[x,])}

# Combining the above data
final_data <- data.frame(player_name,parameters,values)

# Plotting
ggplot(final_data, aes(fill=parameters, y=values, x=player_name)) + geom_bar(position="dodge", stat="identity")

# ----------------------------------------------------------------------------------------

# Plotting of graph 2
# No.of sixes per innings vs strike rate for the batsmen
# With no. of sixes, strike rate should increase

i <- seq(1:50)
test <- matrix(,50,2)
for(x in i){test[x,1]<-(x1$X6s[x]/x1$Inns[x])}
for (x in i){test[x,2] <- x1$SR[x]}
test <- data.frame(test)
colnames(test)<- c("Sixes_per_innings", "SR")
ggplot(test, aes(x=Sixes_per_innings,y=SR)) + geom_point(color="darkorange") + ggtitle("6's per innings vs Strike Rate")

# ----------------------------------------------------------------------------------------

# Plotting of graph 3
# Average of top 10 batsman(top in terms of runs)
test<-NULL
test1<-NULL
test <- c(as.character(x1$Player[1:10]))
test1 <- c(as.numeric(x1$Ave[1:10]))
barplot(height=test1,names.arg=test,xlab="",ylab="Average",col=rgb(0.8,0.2,0.7),ylim=c(0,50),cex.names=0.6,las=2,main="Average of top 10 batsman",border="black")
mtext("Batsman", side=1, line=4)

# ----------------------------------------------------------------------------------------

# Plotting of graph 4
# Pie chart for number of sixes
test <-c(as.numeric(x1$X6s))
labels <- c(as.character(x1$Player))
pie(test, labels, main = "No. of sixes pie chart", col = rainbow(length(test)), radius=1.4, cex=0.6)

# ----------------------------------------------------------------------------------------

# Bowler's data

link2 <- read_html("https://stats.espncricinfo.com/ci/engine/records/bowling/most_wickets_career.html?id=117;type=trophy")
tab2 <- link2 %>%
html_nodes(xpath='//*[@id="ciHomeContentlhs"]/div[3]/div/table[1]') %>%
html_table()
tab2 <-data.frame(tab2)
even_r <- seq(2,100,2)
tab2 <- tab2 [-even_r,]
write.csv(tab2,"databowl.csv")

#Cleaning
x2 <- read.csv("databowl.csv")
x2$X <- NULL
write.csv(x2,"databowl.csv")

# Stats bowlers

# Bowler with the most innings
x2[which.max(x2$Inns),]

# Bowler with most wickets
x2[which.max(x2$Wkts),]

# Bowler with best strike rate
x2[which.min(x2$SR),]

# Bowler with best economy
x2[which.min(x2$Econ),]

# Bowler with the most maidens
x2[which.max(x2$Mdns),]

# Mean of wickets taken
mean(x2$Wkts)

# Standard deviation of wickets taken
sd(x2$Wkts)

# variance of wkts taken
var(x2$Wkts)

# mean economy
mean(x2$Econ)

# Standard deviation of economy
sd(x2$Econ)

# Mean strike rate
mean(x2$SR)

# Variance of strike rate
var(x2$SR)

# Standard deviation of strike rate
sd(x2$SR)

# Mean average
mean(x2$Ave)

# Variance Average
var(x2$Ave)

# Standard deviation of Average
sd(x2$Ave)

# ----------------------------------------------------------------------------------------

# Plotting of graph 5
# Converting Player names to data frame
player_name <- data.frame(x2$Player)

# Replicating top 5 player names 4 times
player_name <- c(as.character(rep(player_name[1,] , 4)) ,as.character(rep(player_name[2,] , 4)), as.character(rep(player_name[3,] , 4)), as.character(rep(player_name[4,] , 4)), as.character(rep(player_name[5,] , 4)))

# Replicating their 4 attributes (5 players)
Parameters <- rep(c("Wickets" ,"Economy", "Average" , "Strike Rate") , 5)

# Their values for these attributes
# Converting attributes of players to data frame
wkts_taken <- data.frame(x2$Wkts)
economy <- data.frame(x2$Econ)
average_player <- data.frame(x2$Ave)
strike_rate <- data.frame(x2$SR)
# Combining values
# The below statements will combine a player's Wickets, Economy ,average and strike rate and repeat for all
i <- seq(1:5)
Values <- NULL
for (x in i){Values <- c(Values, wkts_taken[x,], economy[x,], average_player[x,], strike_rate[x,])}

# Combining the above data
final_data <- data.frame(player_name,Parameters,Values)

# Plotting
ggplot(final_data, aes(fill=Parameters, y=Values, x=player_name)) + geom_bar(position="dodge", stat="identity")

# ----------------------------------------------------------------------------------------

# Plotting of graph 6
# Average(No. of runs conceded per wicket) vs. 
# strike rate(No.of balls bowled per wicket) for the bowler
ggplot(x2, aes(x=SR,y=Ave)) + geom_point(color="#660066") + ggtitle("Average vs Strike Rate")

# ----------------------------------------------------------------------------------------

# Plotting of graph 7
# Economy of top 10 bowlers(top in terms of wickets)
test<-NULL
test1<-NULL
test <- c(as.character(x2$Player[1:10]))
test1 <- c(as.numeric(x2$Econ[1:10]))
barplot(height=test1,names.arg=test,xlab="",ylab="Economy",col="#66ccff",ylim=c(0,10),cex.names=0.6,las=2,main="Economy of top 10 bowlers",border="black")
mtext("Bowler", side=1, line=4)

# ----------------------------------------------------------------------------------------

# Plotting of graph 8
# Pie chart for number of wickets
test <-c(as.numeric(x2$Wkts))
labels <- c(as.character(x2$Player))
pie(test, labels, main = "No. of wickets pie chart", col = rainbow(length(test)), radius=1.4, cex=0.6)

#----------------------------------------------------------------------------------------

# Ranking for top 25 players
# Storing score of each player by assigning priorities to each attribute
# Taken IPL 2018 data
# *********************************************************************************
# Batsman
link3 <- read_html("https://stats.espncricinfo.com/ci/engine/records/batting/most_runs_career.html?id=12210;type=tournament")
tab3 <- link3 %>%
html_nodes(xpath='//*[@id="ciHomeContentlhs"]/div[3]/div/table[1]') %>%
html_table()
tab3 <-data.frame(tab3)
even_r <- seq(2,100,2)
tab3 <- tab3 [-even_r,]
write.csv(tab3,"IPL2018-bat.csv")

#Cleaning
x3 <- read.csv("IPL2018-bat.csv")
x3$X <- NULL
x3 <- x3[-c(26:49),] # Only top 25
write.csv(x3,"IPL2018-bat.csv")

# Giving scores to each
x3$scores <- ((x3$Runs*0.25)+(x3$Ave*0.35)+(x3$SR*0.20)+(x3$X100*0.05)+(x3$X50*0.05)+(x3$X4s*0.03)+(x3$X6s*0.07))
#Assigning ranks
x3$ranks<-26-rank(x3$scores)
x3 <- x3[order(x3$ranks),]

# Plot for batsman Runs scored vs Rank--Plot 9
ggplot(x3, aes(x=ranks,y=Runs)) + geom_line(color="red") + ggtitle("Plot of Runs scored vs. Rank")

# *********************************************************************************
# Bowlers
link4 <- read_html("https://stats.espncricinfo.com/ci/engine/records/bowling/most_wickets_career.html?id=12210;type=tournament")
tab4 <- link4 %>%
html_nodes(xpath='//*[@id="ciHomeContentlhs"]/div[3]/div/table[1]') %>%
html_table()
tab4 <-data.frame(tab4)
even_r <- seq(2,100,2)
tab4 <- tab4 [-even_r,]
write.csv(tab4,"IPL2018-bowl.csv")

#Cleaning
x4<- read.csv("IPL2018-bowl.csv")
x4$X <- NULL
x4 <- x4[-c(26:44),] # Only top 25
write.csv(x4,"IPL2018-bowl.csv")

# Giving scores to each
x4$scores <- ((x4$Wkts*0.40)-(x4$Econ*0.30)-(x4$Ave*0.20)-(x4$SR*0.10))
#Assigning ranks
x4$ranks<-26-rank(x4$scores)
x4 <- x4[order(x4$ranks),]

# Plot for Wickets taken vs Rank -- Plot 10
ggplot(x4, aes(x=ranks,y=Wkts)) + geom_line(color="green") + ggtitle("Plot of Wickets taken vs. Rank")

#----------------------------------------------------------------------------------------

# Principal Component Analysis using IPL-2018 data
# Batsman
x5 <- matrix(,25,6)
x5[,1] <- x3$Runs
x5[,2] <- x3$Ave
x5[,3] <- x3$SR
x5[,4] <- x3$X4s
x5[,5] <- x3$X6s
x5[,6] <- x3$X50
colnames(x5) <- c("Runs", "Average", "StrikeRate", "Fours", "Sixes", "Fifties")

# Plot matrix for Runs, Average, Strike Rate, Fours, Sixes, Fifties
pairs(x5, pch=18, col="darkblue", main="Plot Matrix")

# Correlation coefficient for these variables
head(x5)
cor(x5)

# Covariance
xf <- cov(x5)
head(xf)

# Eigen values of xf
e_value <- eigen(xf)$values

# Eigen vectors of xf
e_vector <- eigen(xf)$vectors
head(e_vector)

# PCA using spectral decomposition approach
pc <- princomp(x5, cor = TRUE, scores = TRUE) # cor=TRUE use corelation matrix
# scores=TRUE score on each principal component is calculated

# No.of observations
pc$n.obs

# The matrix of variable loadings
pc$loadings

# Standard deviation of principal components
pc$sdev

# Means which were subtracted
pc$center

# Summary
summary(pc)

# Plots
plot(pc, type="l", col="red", main="Scree Plot", ylim=c(0,4)) #-- Plot 11
biplot(pc) # Plot 12

# PCA using singular value decomposition approach
pc_svd <- prcomp(t(x5), scale = TRUE, center = TRUE)
pc_svd$x
pc_svd.var <- pc_svd$sdev^2
pc_svd.var

#***********************************************************************************
# Bowlers
x6<- matrix(,25,4)
x6[,1] <- x4$Wkts
x6[,2] <- x4$Ave
x6[,3] <- x4$Econ
x6[,4] <- x4$SR
colnames(x6) <- c("Wkts", "Average", "Economy", "Strike Rate")

# Plot matrix for Runs, Average, Strike Rate, Fours, Sixes, Fifties
pairs(x6, pch=18, col="#ff5050", main="Plot Matrix")

# Correlation coefficient for these variables
head(x6)
cor(x6)

# Covariance
xv <- cov(x6)
head(xv)

# Eigen values of xv
e_valueb <- eigen(xv)$values

# Eigen vectors of xv
e_vectorb <- eigen(xv)$vectors
head(e_vectorb)

pc_a <- princomp(x6, cor = TRUE, scores = TRUE)

# Print the components
print(pc_a)

# No.of observations
pc_a$n.obs

# The matrix of variable loadings
pc_a$loadings

# Standard deviation of principal components
pc_a$sdev

# Means which were subtracted
pc_a$center

# Summary
summary(pc_a)

# Plots
plot(pc_a, type="l", col="#003366", main="Scree Plot", ylim=c(0,3)) #-- Plot 13
biplot(pc_a) #-- Plot 14

#-----------------------------------------------------------------------------------

# Ranking using Eigen Values
# Batsman

temp1 <- read.csv("IPL2018-bat.csv")
temp1$X <-NULL
temp1$scores <- ((temp1$Runs*0.99057315)+(temp1$Ave*0.07368117)+(temp1$SR*0.02828845)+(temp1$X4s*0.10551531)+(temp1$X6s*0.03571932)+(temp1$X50*0.01123998))

temp1$ranks<-26-rank(temp1$scores)
temp1 <- temp1[order(temp1$ranks),]

# Bowlers

temp2 <- read.csv("IPL2018-bowl.csv")
temp2$X <- NULL
temp2$scores <- ((temp2$Wkts*0.15918810)+(temp2$Ave*-0.85463423)+(x4$Econ*-0.06478018)+(temp2$SR*-0.48996225))
temp2$ranks<-26-rank(temp2$scores)
temp2 <- temp2[order(temp2$ranks),]

#-----------------------------------------------------------------------------------