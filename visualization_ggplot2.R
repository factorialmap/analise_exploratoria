# objetivos         ---------------------------------------------------------------------------------------------
"1. apferfeicoar tecnica de construção de visualizações no R"
"2. permitir que pesquisadores consigam representar melhor seus dados com graficos"
"3. facilitar a vida do pesquisador (engloba times de manutenção, engenharia, pcp, cientistas, comercial, academica etc)"

# pacotes           -----------------------------------------------------------------------------------------------
library(tidyverse) #quando carregar este o ggplot2 já carrega junto se quiser separado library("ggplot2")
library(janitor)   #limpa e organiza dados
library(ggtext)    #rotulos 
library(ggrepel)   #lidar com overlaps
library(ggthemes)  #temas para graficos
library(scales)    #formatacao numerica
library(ggdist)    #complementos geometricos etc.
library(gghalves)  #complementos geometricos etc.


# dados             -------------------------------------------------------------------------------------------------
#caracteristicas de carros
mtcars <- mtcars %>% clean_names

#3 tipos de flores iris
iris <- iris %>% clean_names()

#from Wesley Cota EM wcota da Universidade Federal de Viçosa em https://covid19br.wcota.me/
#Repositório de dados no GitHub: wcota/covid19br
data_scov2 <- read.csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>%
  clean_names()


# graficos          ----------------------------------------------------------------------------------------------
#graficos base
mtcars %>% ggplot(aes(x=hp, y=mpg)) + geom_point()
mtcars %>% ggplot(aes(x=hp, y=mpg)) + geom_point(aes(size=5, col=factor(vs)))
mtcars %>% ggplot(aes(x=hp, y=mpg, color = vs)) + geom_point(aes(size=5))
mtcars %>% ggplot(aes(x=hp, y=mpg, color = vs, shape = factor(vs))) + geom_point(aes(size=5))

#inserindo labels nos graficos de base
mtcars %>% 
  ggplot(aes(x=factor(cyl))) +
  geom_bar(color = "black", fill = "lightblue")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -.6)+
  theme_hc()+
  labs(title = "Grafico de barras",
       subtitle = "Em quantidade",
       x = "# cilindros",
       y = "quantidade",
       caption = "Fonte: Carvalho Ribeiro")

#reprodutibilidade - usar as bases para novos graficos
mtcars %>% 
  ggplot(aes(x=factor(cyl))) +
  geom_bar(color = "black", fill = "gray20")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -.6)+
  theme_economist()+
  labs(title = "Grafico de barras",
       subtitle = "Em quantidade",
       x = "# cilindros",
       y = "quantidade",
       caption = "Fonte: Carvalho Ribeiro")

#associando outros pacotes com ggplot2
GGally::ggpairs(iris, ggplot2::aes(color = species))

#graficos facetados
iris %>% 
  pivot_longer(-species, names_to = "tipo_medida", values_to = "valor") %>% 
  mutate(tipo_medida = str_replace(tipo_medida, "_"," ")) %>% 
  group_by(species, tipo_medida) %>% 
  summarise(media = mean(valor)) %>% 
  ggplot(aes(species, media)) +
  geom_col(fill = "lightblue", color = "black")+
  facet_wrap(~tipo_medida)+
  geom_text(aes(label = round(media,2), vjust = -.5))+
  ylim(0,7)+
  theme_pander()+
  labs(title = "Conjunto dados iris",
       subtitle = "medias em cm",
       x = "especies",
       y = "media")+
  theme(axis.title.x = element_text(color = "grey20"),
        axis.title.y = element_text(color = "red"),
        plot.title = element_text(color = "navy"),
        plot.subtitle = element_text(color = "darkgreen"))

#rain cloud plot - mais sobre https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6480976/
iris %>% 
  select(species, sepal_width) %>% 
  ggplot(aes(x=species, y=sepal_width, fill = species))+
  ggdist::stat_halfeye(adjust = .5, width = .6, justification = -.2, .width = 0, point_color = NA)+
  geom_boxplot(width = .12, outlier.color = NA)+
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .3) +
  scale_fill_manual(values = c("#de8e59","#708a81","#46b29e"))+
  theme_classic()+
  labs(title = "Distribuição da largura da sepala 3 tipos de Flores Iris",
       subtitle = "Medidas em cm",
       x = "Espécie",
       y = "Largura")


#graficos com modelo estatistico descritivo
data_scov2 %>% 
  filter(state != "TOTAL") %>%
  mutate(date = as.Date(date)) %>% 
  select(date, state, new_deaths) %>% 
  ggplot(aes(x=date, y=new_deaths, group = state))+
  geom_smooth(method = "loess", se=FALSE, formula = y~x ) +
  scale_x_date(breaks = "6 months", labels = date_format("%b\n%y"))+
  facet_wrap(~state, scales = "free_y") +
  labs(title = "Obitos por coronavirus por estado no Brasil", 
       subtitle = "tentencia usando loess smooth regression",
       x = "date",
       y = "qty",
       caption = "Source: data WCOTA | Loess smoother regression analysis by Marcelo C Anjos")
  

#novos casos
data_scov2 %>% 
  filter(state != "TOTAL") %>%
  mutate(date = as.Date(date)) %>% 
  select(date, state, new_cases) %>% 
  ggplot(aes(x=date, y= new_cases, group = state))+
  geom_smooth(method = "loess", se=FALSE, formula = y~x ) +
  scale_x_date(breaks = "6 months", labels = date_format("%b\n%y"))+
  facet_wrap(~state, scales = "free_y") +
  labs(title = "Casos coronavirus por estado no Brasil", 
       subtitle = "tentencia usando loess smooth regression",
       x = "date",
       y = "qty",
       caption = "Source: data WCOTA | Loess smoother regression analysis by Marcelo C Anjos")




