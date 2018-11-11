# Market Basket Analysis.

### Extracting items from the catalog
142 words are extracted from the catalog: ElectronidexItems2017.pdf using indexes.
- 17 of them are category names
- Enclosing the corresponding items

### Apriori
From package *arules* in R. 
- Function `find_product`: finds the purchase rules for a specific product and calculates 'extraordinary' products (based on lift) along with a histogram for lift of all the rules.
- With a nested for loop over the categories and their corresponding items, a list containing 'rules' objects is created. The rules for a product are organized in the same order as in the catalog. 
- Particularly cool is the function `plot(rules[1:5], method = "graph",  engine = "htmlwidget")`. Displays an interactive graph of the rules.

