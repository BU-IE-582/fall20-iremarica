### Homework 1

# reading the data
data20182019 <- read.csv("/Users/iremarica/Desktop/homework1/20182019.csv")
data20192020 <- read.csv("/Users/iremarica/Desktop/homework1/20192020.csv")
data20202021 <- read.csv("/Users/iremarica/Desktop/homework1/20202021.csv")

# libraries
library(data.table, warn.conflicts=FALSE)
library(ggplot2, warn.conflicts=FALSE)
library(dplyr, warn.conflicts = FALSE)

# combining data
dataset <- rbindlist(list(data20182019, data20192020, data20202021), fill = TRUE)

## Task 1
### Part 1
dataset$home_away <- as.numeric(dataset$FTHG-dataset$FTAG)
qplot(dataset$FTHG, geom="histogram", xlab="Home Goals", ylab="Number of Games",
      main="Histogram for Home Goals", col=I("black"), fill=I("darkturquoise"), binwidth=1)
qplot(dataset$FTAG, geom="histogram", xlab="Away Goals", ylab="Number of Games",
      main="Histogram for Away Goals", col=I("black"), fill=I("pink"), binwidth=1)
qplot(dataset$home_away, geom="histogram", xlab="Home Goals - Away Goals", 
      main="Histogram for Home Goals - Away Goals", ylab="Number of Games", 
      col=I("black"), fill=I("red3"), binwidth=1)

### Part 2
xfit <- seq(min(dataset$FTHG), max(dataset$FTHG))
yfit <- dpois(xfit, lambda=mean(dataset$FTHG))
mean(dataset$FTHG)
exppois <- as.numeric(length(dataset$FTHG)*yfit)
exppois
qplot(dataset$FTHG, geom="histogram", xlab="Home Goals", ylab="Number of Games", 
      main="Histogram for Home Goals", col=I("black"), fill=I("darkturquoise"), 
      binwidth=1) + geom_line(aes(xfit, exppois, col=I("red3")))

xfit <- seq(min(dataset$FTAG), max(dataset$FTAG))
yfit <- dpois(xfit, lambda=mean(dataset$FTAG))
mean(dataset$FTAG)
exppois <- as.numeric(length(dataset$FTAG))*yfit
exppois
qplot(dataset$FTAG, geom="histogram", xlab="Away Goals", ylab="Number of Games", 
      main="Histogram for Away Goals", col=I("black"), fill=I("pink"), 
      binwidth=1) + geom_line(aes(xfit, exppois, col=I("red3")))

## Task 2
### Part 1
# subset of odd values of four bookmakers and calculating probabilities
grep(c("B365H"), colnames(dataset))
grep(c("PSA"), colnames(dataset))
odds <- dataset[,24:35]
probs <- 1/odds
colnames(probs) <- paste0(rep("prob",12),colnames(probs))
dataset <- cbind(dataset, probs)
head(dataset[,126:137])

### Part 2
# normalization
attach(dataset)
dataset <- cbind(dataset, nprobB365H = probB365H*(1/(probB365H+probB365D+probB365A)),
                 nprobB365D = probB365D*(1/(probB365H+probB365D+probB365A)),
                 nprobB365A = probB365A*(1/(probB365H+probB365D+probB365A)))

dataset <- cbind(dataset, nprobBWH = probBWH*(1/(probBWH+probBWD+probBWA)), 
                 nprobBWD = probBWD*(1/(probBWH+probBWD+probBWA)), 
                 nprobBWA = probBWA*(1/(probBWH+probBWD+probBWA)))

dataset <- cbind(dataset, nprobIWH = probIWH*(1/(probIWH+probIWD+probIWA)), 
                 nprobIWD = probIWD*(1/(probIWH+probIWD+probIWA)), 
                 nprobIWA = probIWA*(1/(probIWH+probIWD+probIWA)))

dataset <- cbind(dataset, nprobPSH = probPSH*(1/(probPSH+probPSD+probPSA)), 
                 nprobPSD = probPSD*(1/(probPSH+probPSD+probPSA)), 
                 nprobPSA = probPSA*(1/(probPSH+probPSD+probPSA)))
head(dataset[,138:149])

### Part 3
# creating P(home win) - P(away win) values for each bookmaker
dataset$home_awayB365 <- dataset$nprobB365H-dataset$nprobB365A
dataset$home_awayBW <- dataset$nprobBWH-dataset$nprobBWA
dataset$home_awayIW <- dataset$nprobIWH-dataset$nprobIWA
dataset$home_awayPS <- dataset$nprobPSH-dataset$nprobPSA

# creating bins for each bookmaker
values <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
bins <- c("bin1", "bin2", "bin3", "bin4", "bin5", "bin6", "bin7", "bin8", "bin9", "bin10")
dataset <- dataset %>% mutate(bin_B365=cut(dataset$home_awayB365, breaks=values, labels=bins))
dataset <- dataset %>% mutate(bin_BW=cut(dataset$home_awayBW, breaks=values, labels=bins))
dataset <- dataset %>% mutate(bin_IW=cut(dataset$home_awayIW, breaks=values, labels=bins))
dataset <- dataset %>% mutate(bin_PS=cut(dataset$home_awayPS, breaks=values, labels=bins))

# actual results - B365
total_B365 <- as.numeric(table(dataset$bin_B365))
draw_B365 <- as.numeric(table(dataset$bin_B365[dataset$FTR=="D"]))
prob_bins_B365 <- draw_B365 / total_B365
prob_bins_B365[is.na(prob_bins_B365)] = 0
prob_bins_B365

# actual results - BW
total_BW <- as.numeric(table(dataset$bin_BW))
draw_BW <- as.numeric(table(dataset$bin_BW[dataset$FTR=="D"]))
prob_bins_BW <- draw_BW / total_BW
prob_bins_BW[is.na(prob_bins_BW)] = 0
prob_bins_BW

# actual results - IW
total_IW <- as.numeric(table(dataset$bin_IW))
draw_IW <- as.numeric(table(dataset$bin_IW[dataset$FTR=="D"]))
prob_bins_IW <- draw_IW / total_IW
prob_bins_IW[is.na(prob_bins_IW)] = 0
prob_bins_IW

# actual results - PS
total_PS <- as.numeric(table(dataset$bin_PS))
draw_PS <- as.numeric(table(dataset$bin_PS[dataset$FTR=="D"]))
prob_bins_PS <- draw_PS / total_PS
prob_bins_PS[is.na(prob_bins_PS)] = 0
prob_bins_PS

# plots of P(home win) - P(away win) and P (tie)
mean_values <- c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
par(mfrow=c(2,2))
plot(dataset$home_awayB365, dataset$nprobB365D, cex=0.5, col="blue", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="Bet365")
points(mean_values, prob_bins_B365, cex=1.5, col="red", pch=15)

plot(dataset$home_awayBW, dataset$nprobBWD, cex=0.5, col="purple", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="BW")
points(mean_values, prob_bins_BW, cex=1.5, col="red", pch=15)

plot(dataset$home_awayIW, dataset$nprobIWD, cex=0.5, col="forestgreen", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="IW")
points(mean_values, prob_bins_IW, cex=1.5, col="red", pch=15)

plot(dataset$home_awayPS, dataset$nprobPSD, cex=0.5, col="orange", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="PS")
points(mean_values, prob_bins_PS, cex=1.5, col="red", pch=15)

## Task 3
without_redcard <- subset(dataset, (dataset$HR == 0 & dataset$AR == 0))

# actual results - B365
total_B365_red <- as.numeric(table(without_redcard$bin_B365))
draw_B365_red <- as.numeric(table(without_redcard$bin_B365[without_redcard$FTR=="D"]))
prob_bins_B365_red <- draw_B365_red / total_B365_red
prob_bins_B365_red[is.na(prob_bins_B365)] = 0
prob_bins_B365_red

# actual results - BW
total_BW_red <- as.numeric(table(without_redcard$bin_BW))
draw_BW_red <- as.numeric(table(without_redcard$bin_BW[without_redcard$FTR=="D"]))
prob_bins_BW_red <- draw_BW_red / total_BW_red
prob_bins_BW_red[is.na(prob_bins_BW)] = 0
prob_bins_BW_red

# actual results - IW
total_IW_red <- as.numeric(table(without_redcard$bin_IW))
draw_IW_red <- as.numeric(table(without_redcard$bin_IW[without_redcard$FTR=="D"]))
prob_bins_IW_red <- draw_IW_red / total_IW_red
prob_bins_IW_red[is.na(prob_bins_IW)] = 0
prob_bins_IW_red

# actual results - PS
total_PS_red <- as.numeric(table(without_redcard$bin_PS))
draw_PS_red <- as.numeric(table(without_redcard$bin_PS[without_redcard$FTR=="D"]))
prob_bins_PS_red <- draw_PS_red / total_PS_red
prob_bins_PS_red[is.na(prob_bins_PS)] = 0
prob_bins_PS_red

# plots of P(home win) ??? P(away win) and P (tie)
par(mfrow=c(2,2))
plot(without_redcard$home_awayB365, without_redcard$nprobB365D, cex=0.5, col="blue", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="Bet365")
points(mean_values, prob_bins_B365_red, cex=2, col="black", pch=17)
points(mean_values, prob_bins_B365, cex=1.5, col="red", pch=15)

plot(without_redcard$home_awayBW, without_redcard$nprobBWD, cex=0.5, col="purple", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="BW")
points(mean_values, prob_bins_BW_red, cex=2, col="black", pch=17)
points(mean_values, prob_bins_BW, cex=1.5, col="red", pch=15)

plot(without_redcard$home_awayIW, without_redcard$nprobIWD, cex=0.5, col="forestgreen", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="IW")
points(mean_values, prob_bins_IW_red, cex=2, col="black", pch=17)
points(mean_values, prob_bins_IW, cex=1.5, col="red", pch=15)

plot(without_redcard$home_awayPS, without_redcard$nprobPSD, cex=0.5, col="orange", 
     xlab="P(Home)-P(Away)", ylab="P(Tie)", main="PS")
points(mean_values, prob_bins_PS_red, cex=2, col="black", pch=17)
points(mean_values, prob_bins_PS, cex=1.5, col="red", pch=15)