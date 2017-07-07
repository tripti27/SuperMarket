# Load all the libraries
#arules for mining association rules
#arulesviz for visualizing the association rule (fancy graph)
#datasets for sample dataset in the csv file

library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)

# To Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules, pass minimum required support and confidence
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

#sorting the rules by confidence so most relevant rules appear
rules<-sort(rules, by="confidence", decreasing=TRUE)

#adding maxlen parameter for a more concise rule
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

#to remove the redundant or repeated rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#to target particular items, here we have used- 
#What are customers likely to buy before buying whole milk
#What are customers likely to buy if they purchase whole milk?
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


#set the left hand side to be "whole milk" and find its antecedents
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# Get the library.
library(plotrix)

# Create data for the graph.
x <-  c(2500, 1900, 1850,1700)
lbl <-  c("whole milk","other vegetables","rolls/bun","soda")

# Give the chart file a name.
png(file = "3d_pie_chart.jpg")

# Plot the chart.
pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of items ")

# Save the file.
dev.off()


v <- c(7,10,20,30,40)
t <- c(6,7,10,20,30)
r <- c(5,10,15,18,25)
y <- c(8,10,12,15,20)
f <- c(9,12,15,18,20)


# Give the chart file a name.
png(file = "line_chart_2_lines.jpg")

# Plot the bar chart.
plot(v,type = "o",col = "red", xlab = "frequency of item", ylab = "whole milk", 
     main = "association of items chart")

lines(t, type = "o", col = "blue")
lines(r, type = "o", col = "green")
lines(y, type = "o", col = "yellow")
lines(f, type = "o", col = "pink")

# Save the file.
dev.off()

#visualization of rules in the form of graph
library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)