library(pdftools)
### READ PDF to get our items
imp <- pdf_text("/Users/jdchipox/Downloads/ElectronidexItems2017.pdf")
pretxt1 <- gsub('\n', ',', imp)
pretxt2 <- paste(pretxt1[1], pretxt1[2], pretxt1[3], pretxt1[4])
pretxt3 <- gsub(",$", "", pretxt2)
txt<-gsub(", +", ",", pretxt3)

txt<-unlist(strsplit(txt, split=","))
# Now elements can be accesed with indexes.
# z.B txt[5] prints "ASUS Chromebook'
# WARNING: Our text contains besides the items also the category names
# We need to get rid of them


# To find them, we need to tell first how they are called
categories <- c('Laptops', 'Desktop', 'Monitors', 'Computer Mice',
                'Keyboard', 'Mouse and Keyboard Combo', 'Computer Headphones',
                'Active Headphones', 'Computer Cords', 'Accessories', 'Speakers',
                'Printers', 'Printer Ink', 'Computer Stands', 'Computer Tablets', 
                'External Hardrives', 'Smart Home Devices')

categories_index <- c()
for(p in categories){
categories_index<- union(categories_index,c(grep(p,txt)[1]))
}

# categories_index prints: [1]   1  12  22  33  44  54  64  75  82  92  97 107 113 119 125 131 137
# Within these intervals lie the items that correspond to those categories.
laptops <- txt[2:11]
pc <- txt[13:21]
monitors <- txt[23:32]
mice <- txt[34:43]
keyboard <-txt[45:53]
combo_mouse_key <-txt[55:63] 
pc_headphones <- txt[65:74]
ac_headphones <- txt[76:81]
pc_chords <- txt[83:91]
accesories <- txt[93:96]
speakers <- txt[98:106]
printers <- txt[108:112]
printer_ink <- txt[114:118]
pc_stands <- txt[120:124]
pc_tables <- txt[126:130]
hard_drives <- txt[132:136]
smart_devices <- txt[138:142]

all_categories <- list(laptops, pc, monitors, mice, keyboard, combo_mouse_key, pc_headphones, 
                  ac_headphones, pc_chords, accesories, speakers, printers, printer_ink,
                  pc_stands, pc_tables, hard_drives, smart_devices)

# Now we have categories with their respective items.
# z.B print(monitors) returns all items from this product type.

### IMPORT TRANSACTIONS_ _ _ _ _ _ _ __ _ _ _ _ _ _ __ _ _ _ _ _ __ _ _ _ _ __ _ _ _ _
tra<- read.transactions("/Users/jdchipox/Downloads/ElectronidexTransactions2017.csv", format = "basket", sep=",", rm.duplicates=TRUE)
#Removing empty rows
tra1 <- tra[which(size(tra)!=0)] #tra1 would be the name of our transactions variable

## Counting Frequencies of Categories _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#This function returns how many transactions contain products from a specific category.
count_categories <- function(category, transactions){
  tr <- LIST(transactions)
  x <- which(sapply(tr, FUN=function(X) category%in% X)) 
  return(length(x)) 
}

#z.B count_categories(laptops, tra1) returns 6134. So 6134 laptops were purchased in total.

# We do the same for all categories
# First create empty vectors to hold our values
frequencies <- c()
labels <- c()
# We loop over all 17 categories (from 1 to 17) calling our function each time,
# and adding its result to the vector frequencies. 
for(i in c(1:17)){
  frequencies <- c(frequencies, count_categories(all_categories[[i]], tra1))
  labels <- c(labels, categories[i])
}
# Now name our frequencies vector with our labels
names(frequencies)<-labels

# Plot it!

barplot(frequencies, las=2, col='darkblue', ylab='Purchases', ylim=c(0,10000))


### RUNNING APRIORI _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __ _ _ _ _ _ __ 

# Min Support as 0.0015 , confidence as 0.2 (arbitrarily so)
association.rules <- apriori(tra1, 
                             parameter = list(supp=0.0015, conf=0.2,minlen=2,maxlen=15))
#supposedly removing redundant rules
association.rules <- association.rules[!is.redundant(association.rules)]

# ABSOLUTE KEY: data inside our defined rules, (in this case association.rules) can be accessed using @
# z.B association.rules@quality prints a dataframe with support confidence and lift.

# Because sorting rules by highest lift yields rules with low support and
# Sorting rules by count or support doesn't take into account the yield. 
# And we have a looot of rules.
# I thought of creating another score based on both, trying to favor frequent rules and high lift.

# In the rules@quality dataframe I created another column called MyScore. 
# Where I multiplied lift by count. This is very disputable (should be a better way)
# But the idea here is that the highest results are the most interesting to mine insights from.

association.rules@quality$MyScore <- association.rules@quality$lift * (association.rules@quality$count)
inspect(sort(association.rules, by='MyScore', decreasing=T)[1:10])

# Checking the RULES_ _ _ _ _ __ _ _ _ _ __ _ _ _ _ _ _ _ _ 

# To explore our data 
# We define a function that finds rules with and without a certain product,
# And plots a histogram and boxplot of the lifts of the product rules.

# After calling the function, the variable 'rules' is created.
# The top 15 results are shown.

find_product <- function(with=TRUE){
  product <<- readline(prompt="Enter a product: ")
  if(with == T){
  rules <<- sort(subset(association.rules, items %pin% c(product)), by = 'MyScore')
  }
  else if(with ==F){
  rules <<- sort(subset(association.rules, !items %pin% c(product)), by = 'MyScore')
  }
  print(paste('Variable rules defined with a total of',length(rules), 'rules'))
  
  upper_bound<- quantile(rules@quality$lift, p = c(.95))
  outliers <<- sum(rules@quality$lift > upper_bound)
  print(paste(outliers,'rules have a lift above the 95th percentile'))

  #create graphs
  par(mfrow=c(1,2))
  hist(rules@quality$lift)
  boxplot(rules@quality$lift, xlab=product, ylab='Lift')
  return(try(inspect(rules[1:15])))
}
# The function also plots a histogram and boxplot of the lift for the product rules.
# Because extremely high lifts can be very valuable, the variable 'outliers' is also created. 
# It counts how many rules have a lift above the 95th percentile of all lift's values. 

#INTERACTIVE PLOT of top 5 rules, based on 'MyScore' _ _ _ _ _ _ _ _
plot(association.rules[1:5], method = "graph",  engine = "htmlwidget")

### CATEGORY RULES_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __ 
# Getting all rules for a specific category a.k.a the dream
# Put them all inside a list called 'all_rules'.
# To get to them, we will use two for loops.

# for(products in all_categories){  # => loops each value from every category vector (variables laptops, pc, monitors etc.)
        #for(product in products)} # => loops each value (product) inside of every category. 
                                  #    And calls a function that subsets our rules based on that item

# Function to sort rules.
find_product1 <- function(product){
  rules <<- sort(subset(association.rules, items %pin% product), by = 'MyScore')
  return(rules)
}

all_rules <- list() # this will receive the sets of rules and labels.
product_rules <- c()

count <- 1 # we use this loop over categories <- c('Laptops' ..) with categories[count] for the labels. 
for( products in all_categories){
  print(categories[count])
  #all_rules<<- append(categories[count], all_rules)
  
  product_rules <- c()
  for(product in products){
    product_rules <- c(product_rules, find_product1(product))
  }
  all_rules<- append(all_rules, c(categories[count],product_rules))
  count <- (count + 1)
}  

# Calling categories_index as before, prints: [1]   1  12  22  33  44  54  64  75  82  92  97 107 113 119 125 131 137
# Within these intervals lie the rules that correspond to those categories.
# 125 sets of rules (for every product) and 17 strings, one for every category. A list of 142 objects.
# Takes up 6 Mb of storage

# I know we are just making several cuts (subsets) from the initially defined apriori rules 
# 'association.rules'. So we have quite a lot of redundancy in all of this. 
#  - rules with z.B 4 different items could in principle be picked up by 4 different categories.

# But were the data BIG enough, redundancy wouldn't matter too much, since we wouldn't 
# have time to look it up. Having the product rules associated by a common *attribute*
# could tell interesting things for let's say an extremely big data set.
# With not just 17 but with thousands of categories. Might help check different behaviours
# and keep tracks of things


### Finally, now that we have the categories. Let's compare these rules!

laptops_ <- all_rules[2:11]
pc_ <- all_rules[13:21]
monitors_ <- all_rules[23:32]
mice_ <- all_rules[34:43]
keyboard_ <-all_rules[45:53]
combo_mouse_key_ <- all_rules[55:63] 
pc_headphones_ <- all_rules[65:74]
ac_headphones_ <- all_rules [76:81]
pc_chords_ <- all_rules[83:91]
accesories_ <- all_rules[93:96]
speakers_ <- all_rules[98:106]
printers_ <- all_rules[108:112]
printer_ink_ <- all_rules[114:118]
pc_stands_ <- all_rules[120:124]
pc_tables_ <- all_rules[126:130]
hard_drives_ <- all_rules[132:136]
smart_devices_ <- all_rules[138:142]


#calling smart_devices_ => returns 5 sets of rules.
# Have the same order as smart_devices => 'Apple TV', 'Google Home', 'Smart Light Bulb' ...
# so smart_devices_[1] are the rules for smart_devices[1] (the Apple TV)

# Finally, lets compare different rules

#1. 


#2. 


#3. ## ANY ALGORITHM THAT GETS THE HIGHEST CONFIDENCE.. HIGHEST LIFT. HIGHEST SUPPORT?
# TO COMPUTE OUR NEXT COLUMN





### ITEM FREQUENCY PLOT _ _ _ _ _ _
library(RColorBrewer)
itemFrequencyPlot(tra1,topN=10,type="absolute",col=brewer.pal(8,'Dark2'), main="Absolute Item Frequency Plot")











#Finding the 'most relevant' products to mine info from
#median_lift <- function(item){
# rules <<- subset(association.rules, items %pin% c(item))
#print(paste(item, ' median lift: ', median(rules@quality$lift)))
#return(median(rules@quality$lift))
#}
#
#for(item in txt){
#  all_median_lifts <- c(all_median_lifts, median_lift(item))
#}
#epa <- as.numeric(all_median_lifts)
#
#max(all_median_lifts, na.rm=T)
