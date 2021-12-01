
##############################################################################################
#   title: "Uma conversa entre o R e o Power BI"
# subtitle: "RLadies São Paulo"
# author: 
#   - Adriana Letícia dos Reis^[https://www.linkedin.com/in/reis-al/]
# - Eliana Rigoni^[https://www.linkedin.com/in/elianarigoni/?originalSubdomain=br]
# - Ianní Muliterno^[https://www.linkedin.com/in/iannimuliterno/]
# - Regina Albanese Pose^[https://www.linkedin.com/in/regina-albanese-pose-2300b4110/]
# date: "29/11/2021"
##############################################################################################

source('auxiliares_eval_modelo.R')
source('lift_function.R')

library(ggplot2)
library(janitor)
library(extrafont)
library(tidyverse)
library(kableExtra)
library(data.table)
library(xgboost)
library(ggExtra)
library(ggthemes)
library(InformationValue)
library(ROCit)
library(corrplot)

# conheça as rladies
#https://r-ladies-sao-paulo.github.io/RLadiesTheme/#1


dt <- read.csv(paste0(path,'apresent_oficial/train.csv'))
test <- read.csv(paste0(path,'apresent_oficial/test.csv'))

#Análise Exploratória


# com churn e aging
df_boxplot_medidas <- dt %>%
  group_by(Churn) %>%
  summarise(
    media = mean(tenure),
    mediana = median(tenure),
    desvio_padrao = round(sd(tenure),0)
  )

ggplot(data=dt, aes(x = Churn, y = tenure))+
  geom_boxplot( fill="#AF9DB4")

ggplot(data=dt, aes(x = Churn, y = tenure))+
  geom_boxplot( fill="#AF9DB4")+

  labs(title = "Churn por aging",
       x = "Churn?",
       y = "Aging")+
  theme(plot.title = element_text(hjust = 0.5))+ #<<
  theme_minimal()+ #<<
  geom_point(data = df_boxplot_medidas,
             mapping = aes(x = Churn, y = media),
             colour = "#7A667F", size = 2)+
  geom_text(data = df_boxplot_medidas,
            mapping = aes(x = Churn, y = media,
                          label = format(media, digits = 0, nsmall = 0)),
            size = 3, vjust = 1.5
  )

#Cuidado com a escala

ggplot(data = dt,
       aes(x = MonthlyCharges ))+
  geom_histogram(color = "white", fill = "lightblue" , bins = 30) +
  scale_x_continuous(breaks = seq( 0,
                                   max(dt$MonthlyCharges),
                                   by = 10)) +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(Churn),  scales =  "free")+
  ggtitle("Churn vs Valor Mensal")+
  xlab("Valor mensal da Assinatura") +
  ylab("Frequência")+
  theme_minimal()


mc_plot <- dt %>%
  select_if(is.numeric) %>%
  ggplot(aes(x = MonthlyCharges)) +
  geom_histogram()+
  ggtitle('Monthly Charges')+
  theme_minimal() #<<

mc_plot_0_1 <- dt %>%
  ggplot(aes(x = MonthlyCharges)) +
  facet_wrap(vars(Churn)) +
  geom_histogram(alpha = 0.5)+
  ggtitle('Monthly Charges vs Churn')+
  theme_minimal()

gridExtra::grid.arrange(grobs = list(mc_plot,mc_plot_0_1))

mc_plot <- dt %>%
  select_if(is.numeric) %>%
  ggplot(aes(x = MonthlyCharges)) +
  geom_histogram()+
  ggtitle('Monthly Charges')+
  theme_minimal()

  dt %>%
  ggplot(aes(x = MonthlyCharges, fill  = InternetService)) + #<<
  facet_wrap(vars(Churn)) +
  geom_histogram(alpha = 0.5)+
  ggtitle('Monthly Charges vs Churn') +
  theme_minimal()

# correlacoes


numericas <-  dt %>%
  select_if(is.numeric) %>%
  filter(!is.na(TotalCharges)) %>% 
  select(-SeniorCitizen)

res <- cor(numericas)

corrplot(res, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


## IV e WOE

categoricas <- dt %>%
  mutate(SeniorCitizen = as.character(SeniorCitizen),
         churn_binario = as.character(ifelse(Churn == 'Yes',0,1))) %>%
  select_if(is.character) %>%
  select(-customerID,-Churn)


num_to_cat <- dt %>%
  select_if(is.numeric) %>%
  select(-1) %>%
  mutate(across(everything(),~cut(.x,breaks=5)))

names(num_to_cat)

num_to_cat %>% 
  group_by(tenure) %>% 
  count() %>% 
  bind_cols(num_to_cat %>% 
              group_by(MonthlyCharges) %>% count()) %>% 
                bind_cols(num_to_cat %>% 
                          group_by(TotalCharges) %>% 
                          count() %>% head(5)) %>% 
  janitor::clean_names() %>% 
  kableExtra::kable()

variaveis_cat <- names(categoricas)[-17]


aux_iv <- categoricas %>%
  bind_cols(num_to_cat)

variaveis_cat <- c(variaveis_cat,names(num_to_cat))

lista_saida <- list()


for(cats_index in variaveis_cat){

  i <- which(variaveis_cat == cats_index)

  aux <- aux_iv %>%
    mutate(churn_binario = as.numeric(churn_binario)) %>%
    select(variavel = cats_index,churn_binario)

lista_saida[[i]] <- c(cats_index,IV(as.factor(aux$variavel), aux$churn_binario))
}

tab_iv <- data.frame(matrix(unlist(lista_saida), nrow=length(lista_saida), byrow=TRUE)) %>%
  select(var = 1,IV = 2)%>%
  mutate(IV = round(as.numeric(IV),4)) %>%
  arrange(desc(IV)) 



tab_iv %>%
  mutate(var = factor(var,rev(tab_iv$var))) %>% 
  ggplot(aes(y = var, x = IV)) +
  geom_bar(stat = 'identity') 


churn_binario <- ifelse(dt$Churn == 'Yes',0,1)
aux_woe <- WOETable(X= as.factor(aux_iv %>% pull(variaveis_cat[1])), Y = churn_binario)
aux_woe <- aux_woe[0,]

for(cats_index in variaveis_cat){

  aux_woe <- aux_woe %>%
    bind_rows(
      WOETable(X= as.factor(aux_iv %>% pull(cats_index)), Y = churn_binario) %>%
        mutate(variavel = cats_index)

      )



}


aux_woe %>%
  filter(variavel %in% tab_iv$var[1:6]) %>%
 # ggplot(aes(x = fct_reorder(CAT,WOE), y = WOE)) +
   ggplot(aes(x = CAT, y = WOE)) +
  geom_bar(stat = 'identity')+
   facet_wrap(~variavel,scales = 'free') +
  ggtitle("WOE das 6 variáveis com maior IV*")+
  theme_minimal()+
theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(caption = "*Churn: sim = 0\n
                         não = 1")


# #Perfil do Churner 
# Com o resultado da análise exploratória bem comunicado é possível estruturar "O que esperar" e validar isso com seu _cliente_.
# 
# ### Neste caso, podemos esperar que um churner em potencial: 
# - Contrato mês-a-mês 
# - Paga com cheque eletrônico 
# - Tem pagamento mensal superior a $58,4 
# - Internet Service: Fibra ótica 
# - Não se interessa por serviços adicionais (aprofundar essa afirmação pode ser interessante)


#Dummyficação
leng_vars <- sapply(dt,
                        function(x) ifelse(is.character(x),n_distinct(x),NA))

leng_vars <- leng_vars[!is.na(leng_vars)]

categoricas <- dt %>%
     select_if(is.character) %>%
     select(names(leng_vars[leng_vars > 2])) %>%
   #  select(-customerID,-Churn)
    select(-customerID)

dummy <- dummyVars(" ~ .", data=categoricas)
newdata <- data.frame(predict(dummy, newdata = categoricas))

numericas <- dt %>%
  select_if(is.numeric)

entrada_xgb <- bind_cols(newdata,numericas)
entrada_xgb <- clean_names(entrada_xgb)
############################################################################
leng_vars <- sapply(test, function(x) ifelse(is.character(x),n_distinct(x),NA))
leng_vars <- leng_vars[!is.na(leng_vars)]

categoricas <- test %>%
  select_if(is.character) %>%
 select(names(leng_vars[leng_vars > 2])) %>%
  select(-customerID)

dummy <- dummyVars(" ~ .", data=categoricas)
newdata <- data.frame(predict(dummy, newdata = categoricas))

numericas <- test %>%
  select_if(is.numeric)

teste_xgb <- bind_cols(newdata,numericas)
teste_xgb <- clean_names(teste_xgb)

#Modelando com xgboost
DMatrix
index_treino <- sample(1:nrow(entrada_xgb),nrow(entrada_xgb)*0.75)

 treino <- entrada_xgb[index_treino,]
validacao <- entrada_xgb[-index_treino,]

treino <- xgb.DMatrix(as.matrix(treino)
                      , label = ifelse(dt$Churn[index_treino] == 'Yes',1,0)
                      )

validacao <- xgb.DMatrix(as.matrix(validacao)
                      , label = ifelse(dt$Churn[-index_treino] == 'Yes',1,0)
)



teste <- xgb.DMatrix(as.matrix(teste_xgb)
)

#Regularização com Xgboost
dt %>%
  summarise( sum(Churn == 'No')/sum(Churn == 'Yes'))

watchlist <- list(train = treino, eval = validacao)
parametros = list(
  max_depth = 3,
  eta = 0.15,
  colsample_bytree = 1,
  subsample = 1,
  objective = "binary:logistic",
  eval_metric = 'auc',
  scale_pos_weight = 2.76)

modelo <- xgb.train(data = treino,
                    nrounds = 20,
                    params = parametros,
                  watchlist)


modelo <- xgb.train(data = treino,
                    nrounds = 20,
                    params = parametros,
                  watchlist)


treino_pred <- predict(modelo, treino)
valid_pred <- predict(modelo,validacao)

#Avaliando Performance

dt[-index_treino,] %>%
   mutate(valid_pred) %>%
  ggplot(aes(x = valid_pred, fill = Churn)) +
  geom_histogram( alpha = 0.2)+
  ggtitle('Histograma do valor predito por Churn')

 ROCit_obj <- rocit(score=treino_pred,class=dt[index_treino,]$Churn)
 plot(ROCit_obj)
churn_bin_lift <- ifelse(dt[index_treino,]$Churn == 'Yes',1,0)

lift_plot <- ordenacao_faixa(treino_pred,churn_bin_lift,10) %>%
 ggplot(aes(x = factor(faixa), y = perc)) +
  geom_bar(stat = 'identity') +
geom_text( #<<
          mapping = aes(x = factor(faixa), #<<
                        y = perc+0.1, #<<
                        label = format(paste(round(perc*100,2),'%'), digits = 0, nsmall = 1)), #<<
          fontface = "bold", size = 4, vjust = -0.5) + #<<
  geom_text( 
    mapping = aes(x = factor(faixa),
                  y = -0.1,
                  label = format(total, digits = 0, nsmall = 1)),
    fontface = "bold", size = 4, vjust = -0.5) +
  theme_minimal() +
  xlab('Decil') +
  ylab('Churn (%)') +
  ggtitle("Lift")

lift_plot

##Avaliando Performance - Lift Metric 

validacao_pred <- predict(modelo, validacao)
valid_churn <- ifelse(dt[-index_treino,]$Churn == 'Yes',1,0)

ordenacao_faixa(validacao_pred,valid_churn,10) 
 
dt %>% 
  group_by(Churn) %>% 
  summarise(mean(MonthlyCharges))
