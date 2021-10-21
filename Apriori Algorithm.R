#groceries data set -
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
data("Groceries") #1 month data from sale transactions
summary(Groceries) # total transactions 9835. items are 169 
#density = 0.02609146  - 169 cols and 9835 rows.
#sparse matrix. only 1 transaction with only 32 items
#9835*169*0.02609146 = 43367.01 gives all items/duplicare items. in all 43367 items are bught in a month. this is opbtained by density.

#chart of absolute item frequency plot -
#create an item frequency plot of 20 items
if(!require("RColorBrewer")){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
itemFrequencyPlot(Groceries, topN = 20, type = "absolute", col = brewer.pal(8,'Pastel2'),
                                                                         main= "Absolute Item Frequency Plot")
#brewer.pal = gives different colors.
#can see top 20 items. first 5 are same as received in summary

#apply apriori and create rules
rule1 <- apriori(Groceries, parameter = list(support=0.002, confidence = 0.5))
inspect(head(rule1, 10))
#we are saying 0.2% is the threshold for support
#first 2 rules - 2 items are involved. if buys cerieals he ll buy whole milk also.
#support of 0.003 means 0.3% that means 0.3% that trasanction ll occure for 36 times since count is 36
#confidence = there is 64% prob that the whole milk will appear with cerweal. coveage is support of left hand side.
#coverage get form confidence and support(ref formula in ppt)
#8th rule - 0.002 i .e 0.02% of transaction. 20 transacxtions can occur.
#Association rule - if this happens then that happens
#lift = support of items/support of each item
#Which rules to take ?

#suggest that rules with high lift are significant
inspect(head(sort(rule1, by = "lift"),5))
#prob is given by confidence. 
#it is adviced to compromise between lift and confidence -
#plotting t o figure out -
plot(rule1)
#lift - darker the shade = higher the lift. 
#max is 7 . the dark red is the top combinations. 
#for the dark red - support is low - 0.002 and conf is also low - 51%. thus choosing that doesnt make sens.
plot(rule1, method = "group")
#rule 3 = hard cheese and butter on lhs and rhs = whippd cream and sour cream. conf intervalis lower
#rulle 79 - supporrt high but lift lower
#if looking at few rules then one should go with higher lift.

#change min len = 5 i.e min should hae 5 items.-
rule2 <- apriori(Groceries, parameter =list(support = 0.002, confidence = 0.5, minlen= 5))
#we have 54 rules created. earlier there were 1039. rules limitted
inspect(head(rule2, 5))
plot(rule2, method = "grouped")
plot(rule2)
#generally offers on RHS Items.

#rule 3 - change support and conf-
rule3 <- apriori(Groceries, parameter = list(support = 0.007, confidence = 0.6))
inspect(head(rule3,4))
plot(rule3, method = "grouped")
#rule - butter and yogurt if bought together then whole milk is also bought - top combo
