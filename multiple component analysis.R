library(FactoMineR)
library(factoextra)

data(poison)
res.mca <- MCA(poison,ncp=2,
               quanti.sup = 1, # numeric columns
               quali.sup = NULL, # categorical columns
               graph=T)

# Extract the results for variable categories
res.mca.var <- get_mca_var(res.mca)

# Extract the results for individuals
res.mcs.ind <- get_mca_ind(res.mca)

# Visualize variable categorie contributions on axes 1
fviz_contrib(res.mca, choice ="var", axes = 1)

# Visualize individual contributions on axes 1
# select the top 20
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)