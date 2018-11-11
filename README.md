# Market Basket Analysis.

### Extracting items from the catalog
$142$ words are extracted from the catalog: *ElectronidexItems2017.pdf* using indexes.
- $17$ of them are category names
- Enclosing the corresponding items

### Apriori
From package *arules* in R. 
- Function `find_product()`: finds the purchase rules for a specific product and calculates 'extraordinary' products (based on lift) along with a histogram for lift of all the rules.
- With a nested for loop over the categories and their corresponding items, a list containing 'rules' objects is created. The rules for a product are organized in the same order as in the catalog. 
- Particularly cool is the function `plot(rules[1:5], method = "graph",  engine = "htmlwidget")`. Displays an interactive graph of the rules.

### Some Theory

The rule $\{X_1, X_2 \Rightarrow Y\}$:

can be read as if items/sets $X_1$ and $X_2$ are together, $Y$ also occurs. Depending on the parameters before running apriori one can get rules even for only one observation/purchase. This would be the case if our minimum support is too low. After running Apriori on the transactional data *ElectronidexTransactions2017.csv* an object 'rules'. One can access a table of the quality of the rules with `rules@quality`. 

- $Support(X)$ is the number of transactions with item $X$ divided by the total number of transactions.


- $Confidence(X,Y) = \dfrac{Support(X \cup Y)}{Support(X)}$, is the same as the conditional probability $P(Y|X)$. Can be interpreted as the likelihood of item $Y$ being purchased when item $X$ is already in the shopping cart. 

Because confidence does not take into account the popularity or frequency of $Y$ but only $X$. It's common practice to also check the product of $Confidence(X,Y) \times \dfrac{1}{Support(Y)}$ or $Lift(X,Y)$. If lift is above one, the item $Y$ is likely to be bought together with $X$. Elif lift is below one, item $Y$ is unlikely to be bought together with $X$. Else, when lift is exactly one there is no relation whatsoever. 


