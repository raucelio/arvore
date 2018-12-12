Tempo       <- c("S","S","N","C","C","C","N","S","S","C","S","N","N","C")
Temperatura <- c("A","A","A","B","B","B","B","M","B","M","M","M","A","B")
Umidade     <- c("M","A","A","A","M","B","B","A","B","M","B","A","B","A")
Vento       <- c("N","S","N","N","N","S","S","N","N","N","S","S","N","S")
Joga        <- c("N","N","S","S","S","N","S","N","S","S","S","S","S","N")

dados <- data.frame(Tempo, Temperatura, Umidade, Vento, Joga)

library(gmodels)
CrossTable(Tempo, Joga , prop.t=F,prop.c = T, prop.chisq = F )
CrossTable(Temperatura, Joga , prop.t=F,prop.c = T, prop.chisq = F )
CrossTable(Umidade, Joga , prop.t=F,prop.c = T, prop.chisq = F )
CrossTable(Vento, Joga , prop.t=F,prop.c = T, prop.chisq = F )




entropia <- function (x)
{
  a <- table (x)
  prob <- a/sum(a)
  -sum(prob * log2(prob))
}

entropia(Joga)

informacao <- function(x,y)
   {
    probx_y <- table(x,y)/rowSums(table(x,y))
    probx <-   table (x)/sum(table(x))
    probx_y[probx_y==0] <- 1   
    -sum(rowSums(probx_y * log2(probx_y))*probx)
}

entropia (Joga) - informacao(Vento, Joga)
entropia (Joga) - informacao(Temperatura, Joga)
entropia (Joga) - informacao(Umidade, Joga)
entropia (Joga) - informacao(Tempo, Joga)




modelo <- rpart(Joga ~Tempo + Temperatura + Umidade + Vento, data= dados, method = "class")
rpart.plot(modelo)
