# CURSO INTRODUCAO A CIENCIA DE DADOS COM R
# NELSON QUESADO E CAIO GUIMARAES
# AULA 01

# preambulo ---------------------------------------------------------------
# limpa a memória o ambiente global
rm(list = ls())
gc()

# usa a pasta do script como diretorio de trabalho
rstudioapi::getActiveDocumentContext()$path |>
  dirname() |>
  setwd()

# 1. introducao --------------------------------------------------------------
# ler csv 
read.csv(file = 'data.csv')

# tratando erros e pedindo ajuda sobre funcao
read.csv('data.csv')

read.csv(path = 'data.csv')

?read.csv()

# instalar pacote
install.packages('tidyverse')

# carregar pacote
library(tidyverse)

# plotar um grafico ordenando a variavel y em relação a variável x
read.csv('data.csv') %>% 
  ggplot() +
  geom_point(aes(x = total, y = reorder(state, total)))

# 2. carregando e inspecionando dados ------------------------------------------
# Criando, carregando e inspecionando dados 1
a <- 2
a
print(a)
class(a)
str(a)

a = 2
a == 2
a == a

# Criando, carregando e inspecionando dados 2
b <- 5
c <- -4

a == b
c == -4

class(a) == class(b)

a + b * c
(a + b) * c

# Criando, carregando e inspecionando dados 3
d <- (a + b) * c
d
class(d)

A <- '2'
print(A)
class(A)
str(A)

# Criando, carregando e inspecionando dados 4
class(A) == class(a)

class(a) <- 'character'

class(A) == class(a)

class(a) <- 'numeric'

class(A) == class(a)

ls() # list objects

rm(A, b, c, d)

# Criando, carregando e inspecionando dados 5
c(1, 2, 3, 4)
1:4

c(1, 2, 3, 4) == 1:4

x <- 1:4
x
print(x)
class(x)
str(x)

x == a

y <- x == a

y

class(y)

as.numeric(y)

sum(as.numeric(y))

length(y)

sum(as.numeric(y)) / length(y)

# Criando, carregando e inspecionando dados 6
x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# Criando, carregando e inspecionando dados 7
data <- read.csv('data.csv')

data

print(data)

head(data)

names(data)
      
class(data)

str(data)

summary(data)

c("Boston", "Dakota", "Washington") %in% data$state

# 3. manipulando dados -------------------------------------------------------
# parte 1
ls()
rm(list = ls())

data <- read.csv('data.csv')

tibble(data)

data <- tibble(data)

data

data$state
pull(data, state)
data[, 1]

data$state == pull(data, state)
data[, 1] == pull(data, state)

# parte 2
identical(data$state, pull(data, state))
identical(data[, 1], pull(data, state))

data$state %>% class
pull(data, state) %>% class
data[, 1] %>% class

data[1, ]
data[1, 1]
data[c(1, 2, 3), ]
data[, 2:4]

# parte 3
data$region

table(data$region)

data %>% 
  mutate(region = as_factor(region))

data %>% summary
data %>% str

data <- data %>% 
  mutate(region = as_factor(region))

data %>% summary
data %>% str

# part 4
data %>% mutate(dens = total/population)

data %>% 
  mutate(dens = total/population * 100000)

data %>% 
  mutate(dens = total/population * 100000,
         seguro = dens < 1) %>% 
  filter(seguro == TRUE) %>% 
  arrange(dens)

data %>% 
  mutate(dens = total/population * 100000,
         seguro = dens < 1) %>% 
  arrange(dens) %>% 
  select(state, dens, seguro)

# pate 5
data %>% 
  reframe(population = sum(population),
          total = sum(total))

data %>% 
  group_by(region) %>% 
  reframe(n = n(),
          pop.media = mean(population),
          tot.population = sum(population),
          tot.total = sum(total)) %>% 
  mutate(dens = tot.total/tot.population*100000,
         seguro = if_else(dens < 3, 'seguro', 'inseguro'))


# 4. data viz -------------------------------------------------------------
data %>% 
  ggplot() +
  geom_point(aes(total, reorder(state, total)))

data <- 
  data %>% 
  mutate(dens = total / population *100000)

# col
data %>% 
  ggplot() +
  geom_col(aes(x = abb, y = dens))

data %>% 
  ggplot() +
  geom_col(aes(y = abb, x = dens))

data %>% 
  ggplot() +
  geom_col(aes(y = reorder(abb, dens), x = dens))

data %>% 
  ggplot() +
  geom_col(aes(y = reorder(abb, dens), x = dens, fill = region))

data %>% 
  ggplot() +
  geom_col(aes(y = reorder(abb, dens), x = dens, fill = region), color = 'black')

data %>% 
  ggplot() +
  geom_col(aes(y = reorder(abb, dens), x = dens, fill = region), color = 'black') +
  labs(x = 'Mortes por 100 mil habitantes', y = 'Estado', title = 'Mortes por arma de fogo nos EUA em 2010', fill = 'Região')

data %>% 
  ggplot() +
  geom_col(aes(y = reorder(abb, dens), x = dens, fill = region), color = 'black') +
  labs(x = 'Mortes por 100 mil habitantes', y = 'Estado', title = 'Mortes por arma de fogo nos EUA em 2010', fill = 'Região') +
  theme_minimal() +
  theme(legend.position = 'bottom')

# point + boxplot
data %>%
  ggplot() +
  geom_point(aes(x = region, y = dens))

data %>%
  ggplot() +
  geom_jitter(aes(x = region, y = dens))

data %>%
  ggplot() +
  geom_jitter(aes(x = region, y = dens)) +
  geom_boxplot(aes(y = dens, x = region))

data %>%
  ggplot() +
  geom_boxplot(aes(y = dens, x = region)) +
  geom_jitter(aes(x = region, y = dens))

data %>%
  ggplot() +
  geom_boxplot(aes(y = dens, x = region, color = region)) +
  geom_jitter(aes(x = region, y = dens))

data %>%
  ggplot() +
  geom_boxplot(aes(y = dens, x = region, color = region)) +
  geom_jitter(aes(x = region, y = dens, color = region))

data %>%
  ggplot() +
  geom_boxplot(aes(y = dens, x = region, color = region)) +
  geom_jitter(aes(x = region, y = dens, color = region)) +
  labs(x = 'Região', y = 'Mortes por 100 mil habitantes', title = 'Mortes por arma de fogo nos EUA em 2010', color = 'Região') +
  theme_classic() +
  theme(legend.position = 'top')

# dispersão
p <- ggplot(data = data)
class(p)

p +
  geom_point(aes(x = population/100000, y = total))

p +
  geom_point(aes(x = population/100000, y = total)) +
  geom_text(aes(x = population/100000, y = total, label = abb))

p +
  geom_point(aes(x = population/100000, y = total)) +
  geom_label(aes(x = population/100000, y = total, label = abb))

p +
  geom_point(aes(x = population/100000, y = total), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 15)

p +
  geom_point(aes(x = population/100000, y = total), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

p +
  geom_point(aes(x = population/100000, y = total), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("População em 100 mil (escala log)") +
  ylab("Total de mortes (escala log)") +
  ggtitle("Mortes por arma de fogo nos EUA em 2010")

p +
  geom_point(aes(x = population/100000, y = total, color = region), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("População em 100 mil (escala log)") +
  ylab("Total de mortes (escala log)") +
  ggtitle("Mortes por arma de fogo nos EUA em 2010")

tx <- sum(data$total) / sum(data$population) * 100000
p +
  geom_point(aes(x = population/100000, y = total, color = region), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 0.075) +
  scale_x_log10() +
  geom_abline(intercept = log10(tx), slope = 1, linetype = 'dashed', linewidth = 1, color = 'gray40') +
  scale_y_log10() +
  xlab("População em 100 mil (escala log)") +
  ylab("Total de mortes (escala log)") +
  ggtitle("Mortes por arma de fogo nos EUA em 2010")

p +
  geom_point(aes(x = population/100000, y = total, color = region), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 0.075) +
  geom_abline(intercept = log10(tx), slope = 1, linetype = 'dashed', linewidth = 1, color = 'gray40') +
  scale_x_log10() +
  scale_y_log10() +
  xlab("População em 100 mil (escala log)") +
  ylab("Total de mortes (escala log)") +
  ggtitle("Mortes por arma de fogo nos EUA em 2010") +
  theme(legend.title = element_blank(), legend.position = 'top')

install.packages('ggthemes')
library(ggthemes)
p +
  geom_point(aes(x = population/100000, y = total, color = region), size = 3) +
  geom_text(aes(x = population/100000, y = total, label = abb), nudge_x = 0.075) +
  geom_abline(intercept = log10(tx), slope = 1, linetype = 'dashed', linewidth = 1, color = 'gray40') +
  scale_x_log10() +
  scale_y_log10() +
  xlab("População em 100 mil (escala log)") +
  ylab("Total de mortes (escala log)") +
  ggtitle("Mortes por arma de fogo nos EUA em 2010") +
  theme_economist() +
  theme(legend.title = element_blank(), legend.position = 'top')

ggsave('meu.primeiro.plot.png')

# extra - montando funcoes ------------------------------------------------
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))
