dados <- read.csv("Pokemon_full.csv")

head(dados) # olha as primeiras linhas
tail(dados, 12) # olha as últimas linhas

library(tidyverse)


names(dados)

# Seleciona coluna
select(dados, name, hp)
select(dados, name, hp, speed, attack)


# Filtra colunas
filter(dados, attack > 50)

# Operacoes

mutate(dados, x = attack + speed) # cria variável
mutate(dados, attack = attack/2)

# operador 

dados %>%
  select(name, hp, attack, speed) %>%
  filter(attack < 50) %>% 
  mutate(x = attack + speed)

