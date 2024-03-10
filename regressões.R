#primeiro escolher as covariaveis, retirando as variaveis treat e re78
covariaveis <- colnames(lalonde)[-c(9, 12)]

#criar uma lista para todos os possiveis subconjuntos de covariaveis que
#serao guardados na forma re78 ~ treat + i + ... + j
formulas <- list()

#adicionar o primeiro conjunto
formulas[[1]] <- "re78 ~ treat"

#adicionar os outros 1023
for (i in seq_along(covariaveis)) {                
  aux <- combn(covariaveis, i)                  
  aux <- apply(aux, 2, paste, collapse = " + ") 
  aux <- paste0("re78 ~ treat + ", aux)          
  formulas[[i+1]] <- aux                        
}
#separa em conjuntos de i covariaveis
#junta as i covariaveis com um + entre duas
#cola re78 ~ treat + a esquerda em cada conjunto 
#adiciona todos os conjuntos na i+1 posicao da lista

#desfazer os conjuntos
#fazer uma lista com uma formula em cada posicao 
formulas <- unlist(formulas)

#tornar todas as strings formulas
formulas <- sapply(formulas, as.formula)

#fazer os modelos, aplicando a regressao em cada uma das formulas
modelos <- lapply(formulas, lm, data = lalonde)

#agora, listar os coeficientes relacionados ao tratamento
#criar duas listas, uma para o coeficiente e a outra para o p-valor
listacoef <- list()
listapval <- list()

#pegar os coeficientes requeridos
for (i in seq_along(modelos)) {
  a <- summary(modelos[[i]])$coefficients[2, 1]
  b <- summary(modelos[[i]])$coefficients[2, 4]
  listacoef[i] <- a
  listapval[i] <- b
}

#para verificar quantos coeficientes sao positivos
k <- 0
positivo <- listacoef > 0
for (j in seq_along(listacoef)) {
  if (positivo[i] == TRUE)
    k <- k + 1
}
print(k)

#para verificar o nivel de significancia
m <- 0
positivoval <- listapval < 0.05
for (j in seq_along(listapval)) {
  if (positivoval[i] == TRUE)
    m <- m + 1
}
print(m)
