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
mutate(dados, IMC = weight/(height*height))

dados <- mutate(dados, IMC = weight/(height*height))

# Operador pipe: toma o que está à esquerda e coloca como primeiro argumento da próxima função

df <- select(dados, name, hp, attack, speed)
df <- filter(df, attack < 50)
df <- mutate(df, x = attack + speed)
df

## usando operador pipe

dados %>%
  select(name, hp, attack, speed) %>%
  filter(attack < 50) %>% 
  mutate(x = attack + speed)

# gsub modifica strings
x <- c("Thomas", "Fernando", "Thais")
# se colocar ponto, o pipe rastreia e substitui. Se não colocar, 
# põe como primeiro argumento
x %>%
  gsub("Th", "th", .)

dados %>%
  filter(height > 10) %>%
  select(name, height, weight) %>%
  mutate(imc = weight/(height*height)) %>%
  ggplot()+
  geom_density(aes(x=imc))

head(dados)
dados %>% head()

# comando interessante...

glimpse(dados)
summary(dados)
str(dados)

dados %>% pull(height) # retorna um vetor
dados %>% select(height) # retorna uma coluna

# agrupamento de dados

mean(c(1, 2, 3, 4))

dados %>%
  mutate(media = mean(height)) # cria e preenche uma coluna com o mesmo valor

dados %>%
  summarise(media = mean(height), desvio = sd(height)) # resume os dados, retornando uma coluna para cada variável

dados %>%
  group_by(type) %>%
  summarise(media = mean(height), desvio = sd(height))

dados %>%
  group_by(type) %>%
  mutate(media = mean(height)) %>% view

dados %>%
  group_by %>%
  mutate(media = mean(height)) %>% 
  filter(height > media) %>% view

# como desagrupar
df <- dados %>%
  group_by(type) %>%
  mutate(media = mean(height)) %>%
  filter(height > media) %>% View

df %>%
  ungroup()
mutate(media2 = mean(height))

# busca padrões
# aceita Regular Expressions (ReGex)

grep("saur|fly", dados$name)
grepl("saur|fly", dados$name)

grep("[Ss]aur", dados$name)

dados %>%
  filter(attack > 50)

dados$attack > 50

dados %>%
  filter(grepl("saur|fly", name), attack > 50, type != "grass")

### Trabalhando juntando dados

# bind row

df1 <- dados %>%
  filter(attack > 70)

df2 <- dados %>%
  filter(attack <= 70)

rbind(df1, df2) # juntar linhas

# com colunas diferentes

df1 <- dados %>%
  select(attack, speed, weight) %>%
  filter(attack > 70)

df2 <- dados %>%
  select(attack, weight, height, hp) %>%
  filter(attack <= 70)

rbind(df1, df2) # juntar linhas - não aceita dimensões e nomes diferentes
bind_rows(df1, df2) %>% View # juntar linhas - completa se não for compatível

# juntar colunas

df1 <- dados %>% head(100)
df2 <- dados %>% tail(100)

cbind(df1, df2)

bind_cols(df1, df2)

#############

df_resumo <- dados %>%
  group_by(type) %>%
  summarise(media = mean(height), desvio =sd(height)) 


# fazendo join
# left, right, full, inner

left_join(dados, df_resumo, by = "type") %>% view

df_resumo_mis <- df_resumo %>% filter(type != "grass")

left_join(dados, df_resumo_mis, by = "type") %>% view
right_join(dados, df_resumo_mis, by = "type") %>% view


