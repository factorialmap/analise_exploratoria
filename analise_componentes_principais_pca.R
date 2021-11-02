# Qual o uso prático dos resultados de uma PCA ?                            --------------------------------
"dados sintetizados com perda minima de informação - performance computacional"
"variaveis não correlacioadas - estabilidade de modelos lineares"
"exploração de dados, analisar fenomenos, criar hipoteses - compreender melhor os dados"
"reducao de ruido, dimensionadade, variaveis, sign extraction, feature extraction - big to small data"

# Há diferença nos resultados quando uso prcomp, factomineR ou tidymodels ? --------------------------
"não há diferença nos resultados, apenas no método de construção e respostas"

# pacotes
library(tidyverse)
library(janitor)
library(FactoMineR)
library(factoextra)
library(tidymodels)
library(corrplot)
library(caret)
library(broom)
library(patchwork)

#dados
data(iris)
iris <- iris %>% janitor::clean_names()

# pca_prcomp                                                                ---------------------------------
pca_iris_prc <- iris %>% select(-species) %>% prcomp(center = TRUE, scale. = TRUE)

#plot
fviz_pca_var(pca_iris_prc)

#pca_result_prcomp
loadings_prc <- pca_iris_prc$rotation
scores_prc   <- pca_iris_prc$x
variance_prc <- (pca_iris_prc$sdev)^2

# pca_factominer                                                            ---------------------------------
pca_iris_fcm <- iris %>% PCA(scale.unit = TRUE, quali.sup = 5, graph = TRUE )

pca_iris_fcm$ind

#pca_result_factominer
loadings_fcm  <- sweep(pca_iris_fcm$var$coord,2,sqrt(pca_iris_fcm$eig[,1]), FUN = "/")
scores_fcm   <- pca_iris_fcm$ind$coord
variance_fcm <- pca_iris_fcm$eig

#plot
fviz_pca_var(pca_iris_fcm)


# pca tidymodels                                                            ---------------------------------
pca_iris_tdm <- 
  iris %>% 
  recipe(species~.) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors()) %>% 
  prep()

summary(pca_iris_tdm$steps)

#pca_result_tidymodels
loadings_tdm <- pca_iris_tdm$steps[[2]]$res$rotation
scores_tdm   <- bake(pca_iris_tdm, new_data = NULL)
variance_tdm <- (pca_iris_tdm$steps[[2]]$res$sdev)^2

#plot
data_plot <-  tidy(pca_iris_tdm,2)

data_plot %>% 
  filter(component %in% paste0("PC", 1:2)) %>% 
  ggplot(aes(x=value, y=terms, fill = terms))+
  geom_col()+
  facet_grid(~component, scales = "free")+
  scale_fill_brewer(palette = "BrBG")

# conclusão:                                                                --------------------
loadings_prc %>% head()
loadings_fcm %>% head()
loadings_tdm %>% head()

scores_prc   %>% head()
scores_fcm   %>% head()
scores_tdm   %>% head()

variance_prc %>% head()
variance_fcm %>% head()
variance_tdm %>% head()

# Posso usar PCA para análise exploratória e como fazer ?                   ---------------------------------
"dataset decathlon e o livro do Francois Russon em analise exploratória"
"dados de desempenho de atletas em provas do DecaStar(evento anual Talence França) e Olimpiadas"
"41 observações | 13 variáveis "
"1:10    | var contínuas  | mede desempenho dos atletas nas modalidades"
"11 e 12 | var discretas  | mede pontuação e ranking"
"13      | var categórica | mostra a competição que os atletas participaram"
"O nome maiusculo(e.g SEBRLE) representa o desempenho no DecaStar"
"O nome do atleta em minúsculo(e.g Sebrle) refere-se o desempenho prova Olimpiadas"

#dados
data("decathlon")
decathlon <- decathlon %>% janitor::clean_names()

decathlon %>% head(14)

#nomes portugues pra facilitar entedimento
data_decat <- decathlon %>% 
  rename(salto_dist  = long_jump,
         arrem_peso  = shot_put, 
         salto_altur = high_jump, 
         x110m_barre = x110m_hurdle,
         arrem_disco = discus, 
         salto_vara  = pole_vault,
         lanca_dardo = javeline)

#outro check nos dados
data_decat %>% head(14)

#O que são variáveis suplementares PCA e como podem ser úteis na análise
#pca_decat_factominer
data_decat %>% head()

pca_decat_fcm <- data_decat %>% PCA(scale.unit = TRUE, graph = TRUE, quanti.sup = 11:12, quali.sup = 13)

g1 <- plot.PCA(pca_decat_fcm, choix = c("ind"))
g2 <- plot.PCA(pca_decat_fcm, choix = c("var"))

g1 | g2

#correlacação
corrplot(pca_decat_fcm$var$cor, addCoef.col = TRUE)

# PCA faz diferença em modelos de predição ?                                ----------------------------------

#info sobre os dados
"• sobre solubilidade de composto quimico - livro Applied Predictive Modeling do Max e Kjell"
"• sol: atividade importante na determinação de um composto medicamentos"
"• ajuda a definir se administracao será via oral ou injetvel"
"• amostra de 1267 compostos"
"• 208 binárias indicando presença ou ausencia de subestrutura quimica"
"• 20  contínuas indicando medidas (e.g peso molecular, qtd de atomos, area de superfície)"
"• 1   resposta que é solubility está em log10"
"• as variáveis contínuas foram transformadas com boxcox pra reduzir assimetrias"

#packages
library(tidyverse)
library(tidymodels)
library(janitor)
library(caret)
library(MASS, exclude = "select")

#dados
data(solubility, package = "AppliedPredictiveModeling")
ls(pattern = "solT")

#formatando
train_sol <- solTrainXtrans %>% mutate(solubility = solTrainY) %>% clean_names()
test_sol  <- solTestXtrans %>% clean_names()
resposta  <- solTestY

#check
train_sol %>% skimr::skim()

#modelos
mdl_lm_base <- lm(solubility~., data = train_sol)
summary(mdl_lm_base)
glance(mdl_lm_base)
plot(mdl_lm_base)
pred_lm_base <- predict(mdl_lm_base, newdata = test_sol)
compara <- data.frame(obs = resposta, pred = pred_lm_base)
caret::defaultSummary(compara)

#usando caret
mdl_lm_caret <- 
  train(solubility~., data = train_sol,
        method = "lm",
        trcontrol = trainControl(method = "cv", number = 5))
glance(mdl_lm_caret$finalModel)

mdl_rlm_caret <- 
  train(solubility~., data = train_sol,
        method = "rlm",
        trcontrol = trainControl(method = "cv", number = 5))

mdl_lm_pca <- 
  train(solubility~., data = train_sol,
        method = "lm",
        preProcess = c("center","scale","pca"),
        trcontrol = trainControl(method = "cv",
                                 number = 5,
                                 preProcOptions = list(pcaComp = 20)))
glance(mdl_lm_pca$finalModel)

mdl_lm_corr <- 
  train(solubility~., data = train_sol,
        method = "lm",
        preProcess = c("center","scale","corr"),
        trcontrol = trainControl(method = "cv",
                                 number = 5))
glance(mdl_lm_corr$finalModel)

#fazendo reducao dimensao na mao
dim(train_sol)

train_sol_corr <- preProcess(train_sol, method = c("center","scale","corr"))
train_sol_corr$method$remove

train_sol %>% select(209:220) %>% cor() %>% corrplot(order = "hclust")


#fazendo com tidymodels
rec_correlation <- 
  recipe(solubility~., data = train_sol) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors()) %>% 
  prep()

rec_correlation$steps[[2]]$removals

#aplicar os resultados e rodar o modelo
mdl_spec_tdm <- linear_reg() %>% set_engine("lm")

wkfl_lm_tdm <- 
  workflow() %>% 
  add_recipe(rec_correlation) %>% 
  add_model(mdl_spec_tdm)


#ajustar modelo
mdl_fit_tdm <-  fit(wkfl_lm_tdm, data = train_sol)
glance(mdl_fit_tdm)

#reamostragem
resample_tdm <- vfold_cv(train_sol, v=5)
resample_tdm

#ajuste do modelo reamostragem
mdl_fit_resample_tdm <- fit_resamples(object = wkfl_lm_tdm,
                                      resamples = resample_tdm,
                                      control = control_resamples(save_pred = TRUE))

collect_metrics(mdl_fit_resample_tdm)
show_best(mdl_fit_resample_tdm, metric = "rmse")

# referencias                                                               -------------------------------------------------------------------------------------------
"#" Francois Russon pca e mca
"#" Max Kuhn 2013 - pca tecator food and feed analyser pls
"#" Julia and Max  - pca sementes e agrocultura tidymodel with R
"#" Alisson Horst - pca arte, graficos para reprodução
"#" Abulcadel Kassambara - pca factoextra
"#" Real life example of Eigen values and Eigen vectors by solid mecanic classroom
