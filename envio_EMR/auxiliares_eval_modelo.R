#funções auxiliares
# usadas ao rodar modelos

ordenacao_faixa <- function(score, resposta, faixas){
  
  faixas <- faixas + 1
  dt_faixas <- data.frame(quantile(score, prob = seq(0, 1, length = faixas)))
  dt_faixas <- data.frame(
    cbind(dt_faixas[1:(faixas-1), 1], 
          dt_faixas[2:faixas,1])
    )
  dt_faixas$ID <- seq.int(nrow(dt_faixas))
  
  colnames(dt_faixas) <- c("start","end","ID")
  
  dt_faixas$end[dim(dt_faixas)[1]] <- dt_faixas$end[dim(dt_faixas)[1]]+0.0001
  
  dt_score <- data.frame(score = score, ID = setDT(dt_faixas)[data.table(score), on = .(start <= score, end > score)]$ID, RESP = resposta)
  
  cont_bad_rate <- aggregate(dt_score$RESP, by = list(Category = dt_score$ID), FUN = sum)$x
  cont_total <- aggregate(dt_score$RESP, by = list(Category = dt_score$ID), FUN = length)$x
  
  final <- data.frame(faixa = dt_faixas$ID, inicio = dt_faixas$start, fim = dt_faixas$end, total = cont_total,
                      bad_rate = cont_bad_rate, perc = cont_bad_rate/cont_total)
  
  barplot(final$perc)
  
  final
}

KS_AUX_safra <- function(safra, score, resposta){
  op <- options(warn = (-1)) #suppress warnings
  
  auxiliar <- data.frame(safra = sort(unique(safra)))
  auxiliar$KS <- NaN
  auxiliar$AUC <- NaN
  auxiliar$COUNT <- NaN
  
  for(i in 1:dim(auxiliar)[1]){
    auxiliar$KS[i] <- ks.test(score[resposta == 0 & safra == auxiliar[i,1]],
                              score[resposta == 1 & safra == auxiliar[i,1]])
    auxiliar$AUC <- colAUC(score[safra == auxiliar[i,1]], resposta[safra==auxiliar[i,1]])
    auxiliar$COUNT <- length(resposta[safra == auxiliar[i,1]])
  }
  
  return(auxiliar)
  
  options(op)
}
