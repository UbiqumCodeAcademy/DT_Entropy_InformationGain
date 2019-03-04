# ---------------------------------- #
# Decision trees for classification  #
# Author: Guillem Perdigó            #
# Version 1 | 02.03.2019             #
# ---------------------------------- #

# resource1: https://www.youtube.com/watch?v=nodQ2s0CUbI&list=PLBv09BD7ez_4temBw7vLA19p3tdQH6FYO&index=4
# ressource2: https://www.saedsayad.com/decision_tree.htm

library(rpart)

# creating data ####
mydf <- data.frame(
  Outlook = c("Rainy", "Rainy", "Overcast", "Sunny", "Sunny", "Sunny", "Overcast", "Rainy", "Rainy", "Sunny", "Rainy", "Overcast", "Overcast", "Sunny"),
  Temp = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild"),
  Humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High"),
  Windy = c("False", "True", "False", "False", "False", "True", "True", "False", "False", "False", "True", "True", "False", "True"),
  PlaysGolf = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")
)

# We want to build a decision tree to predict if a golfer is going to play given some weather conditions.
# What attribute should we use first to begin splitting our data? To decide that, we need to understand Entropy and Information Gain.

# Entropy tells us how pure of impure is a subset named S. This is the formula:
# E(S) = -p(+) log2 p(+) - p(-) log2 p(-)
# S = subset of training examples
# p(+) and p(-) = % of positve / negative examples in S
# It will be 0 for a pure set and 1 for a totally impure set (split 50/50)
  # totally impure set (3 yes / 3 no):
  -(3/6) * log(3/6, base = 2) - (3/6) * log(3/6, base = 2) # Entropy = 1
  # pure set (4 yes / 0 no):
  -(4/4) * log(4/4, base = 2) - (0/4) * log(0/4, base = 2) # Entropy = NaN = 0

entropy <- function(p){
  return(-(p) * log(p, base = 2) - (1-p) * log(1-p, base = 2))
} 

entropy(1) #0.8112781

# The entropy for the initial node is:
table(mydf$PlaysGolf)
  #PNo Yes 
  # 5   9 
  ePlaysGolf <- -(9/14) * log(9/14, base = 2) - (5/14) * log(5/14, base = 2) # 0.940286

# Let's calculate entropy for 2 attributes
# We do it by taking the weighted average of the entropy for each branch. 
# The weight is the size of this subset divided by the overall number of examples
  
# PlaysGolf - Outlook
  table(mydf$PlaysGolf, mydf$Outlook)
  # PlaysGolf   Overcast Rainy Sunny
  # No          0        3     2
  # Yes         4        2     3
  eOvercast <- -(4/4) * log(4/4, base = 2) - (0/4) * log(0/4, base = 2) # NaN = 0
  eRainy <- -(2/5) * log(2/5, base = 2) - (3/5) * log(3/5, base = 2) # 0.971
  eSunny <- -(3/5) * log(3/5, base = 2) - (2/5) * log(2/5, base = 2) # 0.971
  
    ePlayOutlook <- 4/14 * eOvercast + 5/14 * eRainy + 5/14 * eSunny #0.6935361

# PlaysGolf - Wind
  table(mydf$PlaysGolf, mydf$Wind)
  # PlaysGolf   False True
  # No          2     3
  # Yes         6     3
  eWindF <- -(6/8) * log(6/8, base = 2) - (2/8) * log(2/8, base = 2) # 0.8112781
  eWindT <- -(3/6) * log(3/6, base = 2) - (3/6) * log(3/6, base = 2) # 1
  
    ePlayWind <- 8/14 * eWindF + 6/14 * eWindT #0.8921589

# PlaysGolf - Temp
  table(mydf$PlaysGolf, mydf$Temp)
  # PlaysGolf Cool Hot Mild
  # No        1   2    2
  # Yes       3   2    4
  
  eCool <- -(3/4) * log(3/4, base = 2) - (1/4) * log(1/4, base = 2) # 0.8112781
  eHot <- -(2/4) * log(2/4, base = 2) - (2/4) * log(2/4, base = 2) # 1
  eMild <- -(4/6) * log(4/6, base = 2) - (2/6) * log(2/6, base = 2) # 0.9182958
  
    ePlayTemp <- 4/14 * eCool + 4/14 * eHot * 6/14 * eMild #0.3442381
    
  # PlaysGolf - Humidity
  table(mydf$PlaysGolf, mydf$Humidity)
  # PlaysGolf High  Normal 
  # No        4     1  
  # Yes       3     6
    
    eHighHum <- -(3/7) * log(3/7, base = 2) - (4/7) * log(4/7, base = 2) # 0.9852281
    eNormalHum <- -(6/7) * log(6/7, base = 2) - (1/7) * log(1/7, base = 2) # 0.5916728

    ePlayHum <- 8/14 * eHighHum + 6/14 * eNormalHum #0.8165616
  
# Information Gain: is the decrease in entropy for a split.
    # Gain(T, X) = Entropy(T) - Entropy(T, X)
    ePlaysGolf - ePlayOutlook # 0.2467498
    ePlaysGolf - ePlayWind # 0.04812703
    ePlaysGolf - ePlayTemp # 0.04812703
    ePlaysGolf - ePlayHum # 0.1237244

# We now pick the attribute with the largest information gain: Outlook. 
# We make the split and we repeat the process for all the branches 
# (except for those ones that have entropy = 0, that become final nodes - also called leaves)

# We can now fit a decision tree to the data, to see how the first split is indeed Outlook
    
# Note: for now we use rpart, but it'd be nice to use an implementation of id3 in r
    
ctrl <- rpart.control(minsplit = 0, minbucket = 0)
GolfTree <- rpart(PlaysGolf ~., data = mydf, control = ctrl)
# 1) root 14 5 Yes (0.3571429 0.6428571)  
#   2) Outlook=Rainy,Sunny 10 5 No (0.5000000 0.5000000)  
#     4) Humidity=High 5 1 No (0.8000000 0.2000000)  
#       8) Outlook=Rainy 3 0 No (1.0000000 0.0000000) *
#       9) Outlook=Sunny 2 1 No (0.5000000 0.5000000)  
#         18) Windy=True 1 0 No (1.0000000 0.0000000) *
#         19) Windy=False 1 0 Yes (0.0000000 1.0000000) *
#     5) Humidity=Normal 5 1 Yes (0.2000000 0.8000000)  
#       10) Windy=True 2 1 No (0.5000000 0.5000000)  
#         20) Outlook=Sunny 1 0 No (1.0000000 0.0000000) *
#         21) Outlook=Rainy 1 0 Yes (0.0000000 1.0000000) *
#       11) Windy=False 3 0 Yes (0.0000000 1.0000000) *
#   3) Outlook=Overcast 4 0 Yes (0.0000000 1.0000000) *
GolfTree
par(xpd = TRUE) #enable things to be drawn outside the plot region
plot(GolfTree, compress = TRUE)
text(GolfTree, use.n = TRUE)



