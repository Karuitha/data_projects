## ----setup, include=FALSE----
knitr::opts_chunk$set(echo = FALSE)


## ---- echo=FALSE------------
## Load package manager
if (!require(pacman)) {
  install.packages("pacman")
}

## Load the required packages 
pacman::p_load(
  tidyverse, janitor, 
  skimr, kableExtra, 
  readxl, conflicted,
  GGally, ggthemes, 
  patchwork, stargazer, 
  arules, arulesViz
)

## Set digit precision
options(digits = 3)

## Set plots theme
ggplot2::theme_set(theme_wsj())



## ---- echo=FALSE------------
## Class of the data
data("Groceries")
class(Groceries)


## ---- echo=FALSE------------
## Preview the data

Groceries


## ---- echo=TRUE, fig.height=10, fig.width=7----
## Item frequency plot with support > 0.05
itemFrequencyPlot(Groceries, support = 0.05, main = "Item Frequency Plot: Support > 0.05")


## ---- echo=TRUE-------------
## Create the rules
my_rules <- apriori(data = Groceries, 
                    parameter = list(supp = 0.001, 
                                     conf = 0.5),
                    appearance = list (default="lhs", 
                                       rhs="whole milk"), 
                    control = list (verbose=F)
                    )


## ---- echo=TRUE-------------
## Sort the rules
right_rules_conf <- sort (my_rules, 
                    by="confidence", 
                    decreasing=TRUE) 

inspect(head(right_rules_conf))


## ---- echo=TRUE-------------
## Left rules
left_rules <- apriori(data = Groceries, 
                    parameter = list(supp = 0.001, 
                                     conf = 0.5),
                    appearance = list(default="rhs", 
                                       lhs="whole milk"), 
                    control = list (verbose=F)
                    )
left_rules


## ---- echo=TRUE-------------
## Inspect left rules
left_rules_conf <- sort(left_rules, 
                    by="confidence", 
                    decreasing=TRUE) 

inspect(head(left_rules_conf))


## ---- echo=TRUE-------------
## Plot the rules
plot(my_rules[1:3])


## ---- echo=TRUE-------------
plot(my_rules[1:3], method="graph", engine="htmlwidget")


## ---- echo=TRUE-------------
## Plot the rules
knitr::include_graphics("assoc.png")

