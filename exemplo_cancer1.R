arq <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"

dados <- read.table(arq, sep=",", header=F, na.string = "?")


names (dados) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "marginalAdhesion",
                   "singlEpithelialCellSize", "bareNuclei",
                   "balndChromatin","NormalNucleoli",
                   "mitosis", "class")

dados$class <- factor(dados$class , 
                      levels = c(2, 4), 
                      labels = c("benigno", "maligno"))


library (rpart)
set.seed(1234)

dtree <- rpart (class ~ . ,
                data = dados [,-1], 
                method="class",
                parms=list(split="information") )

print(dtree)

library( rpart.plot)

rpart.plot(dtree, type=2, extra=104, fallen.leaves = T)
