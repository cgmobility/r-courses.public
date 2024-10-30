
# Carregando pacotes ------------------------------------------------------

if('tidyverse'%in% installed.packages()==FALSE){
  install.packages('tidyverse')
}
if('rstudioapi'%in% installed.packages()==FALSE){
  install.packages('rstudioapi')
}
if('ggthemes'%in% installed.packages()==FALSE){
  install.packages('ggthemes')
}
if('ggcorrplot'%in% installed.packages()==FALSE){
  install.packages('ggcorrplot')
}
if('LDA'%in% installed.packages()==FALSE){
  remotes::install_github('cgmobility/LDA')
}


library(tidyverse)
library(rstudioapi)
library(ggthemes)
library(lubridate)
library(LDA)
library(ggcorrplot)

rm(list = ls())

# Diretório de trabalho ---------------------------------------------------

# Use setwd para setar o diretório

setwd("seu diretório aqui")

# Ou salve o arquivo r na pasta desejada e use LDA::wd()

wd() # só funciona se LDA estiver carregada (ou seja, se você executou library(LDA))

LDA::wd() # Funciona se LDA estiver instalado


# Leitura dos dados -------------------------------------------------------

# Usando a base do R

tictoc::tic()
data <- read.csv('Dados/sales_fortaleza_dataset.csv', sep = ',')
tictoc::toc()

# Usando o pacote data.table

tictoc::tic()
data <- data.table::fread('dados/sales_fortaleza_dataset.csv', sep = ',',verbose = F) # serve para csv e txt local ou na web
tictoc::toc()


dim(data)
summary(data)

data <- as_tibble(data)

# Qual a região de maior venda --------------------------------------------

data %>% 
  group_by(category,neighborhood.Region) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  arrange(-sales)

data %>% 
  group_by(category,neighborhood.Region) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  group_by(category) %>% 
  filter(sales == max(sales))

data %>% 
  group_by(neighborhood.Region) %>% 
  reframe(n_units = length(unique(unit_id)))


data %>% 
  group_by(category,neighborhood.Region) %>% 
  reframe(
    sales = sum(Units_Sold),
    n_units = length(unique(unit_id))
  ) %>% 
  mutate(sales_unit = sales/n_units) %>% 
  group_by(category) %>% 
  filter(sales_unit == max(sales_unit))


# Qual o bairro de maior venda de cada tipo de produto? -------------------

data %>% 
  group_by(neighborhood,category) %>% 
  reframe(
    sales = sum(Units_Sold),
    n_units = length(unique(unit_id))
  ) %>% 
  mutate(sales_unit = sales/n_units) %>% 
  group_by(category) %>% 
  filter(sales_unit == max(sales_unit))


# Qual o mês de maior venda de cada tipo de produto? ----------------------

data %>% 
  group_by(month,category) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  group_by(category) %>% 
  filter(sales == max(sales))


# Qual a mês de maior venda no ano de maior venda? ------------------------

data %>% 
  group_by(category,year,month) %>% 
  reframe(month_sales = sum(Units_Sold)) %>% 
  group_by(category,year) %>% 
  mutate(year_sales = sum(month_sales)) %>% 
  group_by(category) %>% 
  filter(year_sales == max(year_sales)) %>% 
  group_by(category) %>% 
  filter(month_sales == max(month_sales))



# Qual o tipo de produto que mais vende? ----------------------------------

data %>% 
  group_by(category) %>% 
  reframe(sales  = sum(Units_Sold))

data %>% 
  group_by(category,product_name) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  group_by(category) %>% 
  reframe(sales = mean(sales))


# Que tipo de produto mais vende entre as mulheres? -----------------------

data %>% 
  mutate(women_percent = neighborhood.Women / neighborhood.Population) %>% 
  mutate(majority_women = women_percent>mean(women_percent)) %>% 
  group_by(category,product_name,majority_women) %>% 
  reframe(sales = sum(Units_Sold)) %>%
  group_by(category,majority_women) %>%
  reframe(sales = mean(sales)) %>% 
  arrange(majority_women,sales)


# Que tipo de produto mais vende entre os idosos? -------------------------

data %>% 
  mutate(seniors_percent = neighborhood.Old_People / neighborhood.Population) %>% 
  mutate(majority_seniors = seniors_percent>mean(seniors_percent)) %>% 
  group_by(category,product_name,majority_seniors) %>% 
  reframe(sales = sum(Units_Sold)) %>%
  group_by(category,majority_seniors) %>%
  reframe(sales = mean(sales)) %>% 
  arrange(majority_seniors,sales)



# Análise descritiva ------------------------------------------------------


  # Análise descritiva temporal ---------------------------------------------

# Ano
data %>% 
  group_by(year) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  ggplot(aes(x = year, y = sales))+
  geom_point()

data %>% 
  group_by(year) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  ggplot(aes(x = year, y = sales))+
  geom_point()+
  geom_line()

data %>% 
  group_by(year,category) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  ggplot(aes(x = year, y = sales))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_y_comma(labels = scales::label_comma(scale = 1e-6, suffix = 'M',accuracy = .1))+
  scale_x_continuous(n.breaks = length(unique(data$year)))+
  facet_wrap(~category,scales = 'free_y',nrow = 2)+
  theme_tt()+
  labs(
    title = 'Venda por ano',
    x  = 'Ano',
    y = 'Unidades vendidas'
  )

ggsave('imagens/queda_vendas.png',width = 16,height = 9)

# Mês

data %>% 
  group_by(year,month,category) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  group_by(month,category) %>% 
  reframe(sales = mean(sales)) %>% 
  ggplot(aes(x = month, y = sales))+
  geom_col(fill = '#0B486B')+
  scale_y_continuous(labels = scales::label_comma(scale = 1e-6, suffix = 'M'))+
  ggthemes::theme_economist()+
  facet_wrap(~category,scales = 'free_y')

data %>% 
  group_by(year,month,category) %>% 
  reframe(sales = sum(Units_Sold)) %>% 
  group_by(month,category) %>% 
  reframe(sales = mean(sales)) %>% 
  mutate(month = factor(month,
                        levels = month(1:12,label = T,abbr = F, locale = 'en_US'))) %>% 
  ggplot(aes(x = month, y = sales))+
  geom_col(fill = '#0B486B')+
  scale_y_continuous(labels = scales::label_comma(scale = 1e-6, suffix = 'M'))+
  ggthemes::theme_economist()+
  geom_label(aes(label = paste0(round(sales/1e6,2),'M')))+
  facet_wrap(~category,scales = 'free_y',nrow = 2)+
  theme(
    axis.text.x = element_text(angle = 20,vjust = 1)
  )+
  labs(
    title = 'Venda por categoria em cada mês',
    x = 'Meses',
    y = 'Vendas (milhões)'
  )




  # Análise descritiva demográfica ---------------------------------------

## Gênero

neighborhood_data <- data %>% 
  mutate(Woman_percent = neighborhood.Women/neighborhood.Population) %>% 
  mutate(Elderly_percent = neighborhood.Old_People/neighborhood.Population) %>% 
  group_by(neighborhood,year,month,category,product_name,Promotion,
           Woman_percent,Elderly_percent,neighborhood.Population,
           neighborhood.HDI_Income) %>% 
  reframe(
    sales = mean(Units_Sold)
  )

neighborhood_data %>% 
  group_by(category, neighborhood, Woman_percent) %>% 
  reframe(sales = sum(sales)) %>% 
  ggplot(aes(x = Woman_percent, y = sales))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_y_comma(labels = scales::label_comma(scale = 1e-6, suffix = 'M',accuracy = .1))+
  facet_wrap(~category,scales = 'free_y',nrow = 2)+
  theme_clean()+
  labs(
    title = 'Vendas por categoria x Percentual de mulheres',
    x  = 'Percentual de mulheres',
    y = 'Unidades vendidas'
  )


## Idade

neighborhood_data %>% 
  group_by(category, neighborhood, Ederly_percent) %>% 
  reframe(sales = sum(sales)) %>% 
  ggplot(aes(x = Woman_percent, y = sales))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_y_comma(labels = scales::label_comma(scale = 1e-6, suffix = 'M',accuracy = .1))+
  facet_wrap(~category,scales = 'free_y',nrow = 2)+
  theme_clean()+
  labs(
    title = 'Vendas por categoria x Percentual de mulheres',
    x  = 'Percentual de mulheres',
    y = 'Unidades vendidas'
  )



  # Análise descritiva espacial ---------------------------------------------

data %>% 
  group_by(neighborhood,neighborhood.Region,year,month,category,product_name) %>% 
  reframe(
    sales = mean(Units_Sold)
  ) %>% 
  group_by(neighborhood.Region,category) %>% 
  reframe(sales = sum(sales)) %>% 
  mutate(neighborhood.Region = as.factor(neighborhood.Region)) %>% 
  ggplot(aes(x = sales, y = neighborhood.Region))+
  geom_boxplot(color = '#0B486B')+
  theme_economist_white()+
  scale_x_comma(labels = scales::label_comma(scale = 1e-6, suffix = 'M',
                                             accuracy = .1))+
  labs(
    title = 'Vendas por região',
    x = 'Vendas',
    y = 'Região'
  )
  


# Correlação --------------------------------------------------------------


## Correlação simples

cor(data$price,data$Units_Sold)


cor.test(data$price,data$Units_Sold)



## Matriz de correlação

cor_data <- data %>% 
  filter(category == 'beauty product') %>% 
  mutate(high_sales_month = as.numeric(month %in% c('January','May','November','December'))) %>% 
  select(year,high_sales_month,price,Promotion,Units_Sold,neighborhood.HDI_Income,
         neighborhood.Population,neighborhood.Land_Value,
         neighborhood.Women,neighborhood.Old_People)

cor_matrix <- cor_data %>% 
  cor(use = 'complete.obs')

p.mat <- cor_pmat(cor_data)

View(cor_matrix)


ggcorrplot(cor_matrix,lab = TRUE, hc.order = TRUE, type = "lower",
           p.mat = p.mat,title = 'Matriz de correlação',
           ggtheme = theme_clean(),digits = 3,sig.level = 0.1)

# Análise preditiva -------------------------------------------------------


  # Regressão múltipla -------------------------------------------------------

### Medicine
medicine <- data %>% 
  filter(category=='medicine') 

medicine <- medicine %>% 
  mutate(high_sales_month = as.numeric(month %in% c('January','May','November','December')))

model_medicine <- medicine %>% 
  lm(
    data = .,
    formula = Units_Sold~neighborhood.HDI_Income+neighborhood.Women+
      neighborhood.Old_People+neighborhood.Population+
      Promotion+high_sales_month+price
  )

summary(model_medicine)

model_medicine <- medicine %>% 
  lm(
    data = .,
    formula = Units_Sold~0+neighborhood.HDI_Income+neighborhood.Women+
      neighborhood.Old_People+neighborhood.Population+
      Promotion+high_sales_month+price
  )

summary(model_medicine)


### Beauty products
beauty_products <- data %>% 
  filter(category=='beauty product')

beauty_products <- beauty_products %>% 
  mutate(high_sales_month = as.numeric(month %in% c('January','May','November','December')))
model_beauty_products <- beauty_products %>% 
  lm(
    data = .,
    formula = Units_Sold~0+neighborhood.HDI_Income+neighborhood.Women
    +neighborhood.Old_People+neighborhood.Population+
      Promotion+high_sales_month+price
  )

summary(model_beauty_products)


### Vitamins
vitamins <- data %>% 
  filter(category=='vitamins')

vitamins <- vitamins %>% 
  mutate(high_sales_month = as.numeric(month %in% c('January','May','November','December')))
model_vitamins <- vitamins %>% 
  lm(
    data = .,
    formula = Units_Sold~0+neighborhood.HDI_Income+neighborhood.Women+
      neighborhood.Old_People+neighborhood.Population+
      Promotion+high_sales_month+price
  )

summary(model_vitamins)




# Análise prescritiva -----------------------------------------------------

bairros_2024 <- fread('dados/bairros_2024.csv') %>% 
  as_tibble()


predict(model_vitamins,bairros_2024)

predict(
  model_vitamins,
  bairros_2024 %>% 
    mutate(
      Promotion = 0,
      high_sales_month = 0,
      price = 0
    )
)


bairros_2024$`Vitaminas - Mês de baixa` <- predict(
  model_vitamins,
  bairros_2024 %>% 
    mutate(
      Promotion = 1, ## A loja deverá realizar promoções na inauguração
      high_sales_month = 0, ## Indica-se a abertura ainda este mês
      ## O preço para esta estimativa é irrelevante, a média é utilizada para
      # que os valores estimados tenham sentido
      price = mean(vitamins$price) 
    )
)

bairros_2024$`Produtos de beleza - Mês de alta` <- predict(
  model_beauty_products,
  bairros_2024 %>% 
    mutate(
      Promotion = 1,
      high_sales_month = 1,
      price = mean(beauty_products$price)
    )
)

bairros_2024$`Medicamentos - Mês de baixa` <- predict(
  model_medicine,
  bairros_2024 %>% 
    mutate(
      Promotion = 1,
      high_sales_month = 0,
      price = mean(medicine$price)
    )
)




# Análise prescritiva - Em que bairro eu devo implantar uma farmácia e que produtos eu devo vender --------

bairros_2024 <- bairros_2024 %>% 
  mutate(
    vendas_outubro = `Medicamentos - Mês de baixa`+`Vitaminas - Mês de baixa`,
    vendas_novembro = `Produtos de beleza - Mês de alta`
  )


bairros_2024 %>% 
  filter(is.na(quant_units)) %>% 
  select(neighborhood,`Medicamentos - Mês de baixa`,`Vitaminas - Mês de baixa`,vendas_outubro) %>% 
  arrange(-vendas_outubro)

bairros_2024 %>% 
  filter(is.na(quant_units)) %>% 
  select(neighborhood,`Produtos de beleza - Mês de alta`,vendas_novembro) %>% 
  arrange(-vendas_novembro)







# Conteúdo adicional - Loops e iterações ----------------------------------------------

# Lista de vetores com média 10 e desvio 2

data <- replicate(100000,rnorm(100,10,2),simplify = F)
data.df <- do.call(rbind,data)

  # Loop for ----------------------------------------------------------------

for (i in 1:100) {
  cat(i)
}

for (i in 1:nrow(bairros_2024)) {
  bairros_2024$neighborhood.Women[i]/bairros_2024$neighborhood.Population[i]
}

for (i in 1:nrow(bairros_2024)) {
  print(bairros_2024$neighborhood.Women[i]/bairros_2024$neighborhood.Population[i])
}

#Cálculo da média de uma lista

mean(data)

medias <- NULL

for (i in data) {
  medias <- c(medias,mean(i))
}

mean(medias)
sd(medias)

  # Loop while --------------------------------------------------------------
i <- 1 ## Índice para avaliação
while (i < nrow(bairros_2024)) { ## Condição de parada
  print(bairros_2024$neighborhood.Women[i]/bairros_2024$neighborhood.Population[i])
  i <- i+1 ## Alteração do índice em cada loop
}
  
# Média da lista

tictoc::tic()
medias <- NULL
i <- 1
while(i < length(data)) {
  medias <- c(medias,mean(data[[i]]))
  i = i+1
}
tictoc::toc()

# Iteração lapply, apply e sapply -----------------------------------------

# Para data.frames, matrix ou tibbles

tictoc::tic()

medias <- apply(data.df, 1, mean)

tictoc::toc()


apply(
  X = data.df, ## X recebe uma matriz, data.frame ou tibble onde será aplicado a função
  MARGIN = 1, ## Margin 1 para aplicar a função em cada linha 2 para colunas
  FUN = mean ## Função
)

# Para listas ou vetores


tictoc::tic()
medias <- lapply(data,mean)
tictoc::toc()

medias <- unlist(medias)

tictoc::tic()
medias <- sapply(data,mean)
tictoc::toc()



# Conteúdo adicional - Criação de funções ------------------------------------------------------

# Função para calcular uma média
average <- function(x){
  sum(x)/length(x)
}

vector <- rnorm(1000,50,5)

options(digits = 10)
# Comparação da função com o padrão do r mean
average(vector)
mean(vector)

## Comparação FALSE por conta de pequenas diferenças no número
average(vector) == mean(vector)

options(digits = 18)
average(vector)
mean(vector)


## Grau de precisão da sua máquina
.Machine$double.eps

## Comparação TRUE ao arredondar para 10 casas decimais
round(average(vector),10) == round(mean(vector),10)


### Conversão de temperatura

f2celsius <- function(tempF, units = F){
  if(units){
    tempC <- (tempF - 32)*5 / 9 
    tempC <- round(tempC,1)
    units(tempC) <- 'celsius'
  }else{
    tempC <- (tempF - 32)*5 / 9 
  }
  return(tempC)
}


f2celsius(90)

f2celsius(90, units = T)
