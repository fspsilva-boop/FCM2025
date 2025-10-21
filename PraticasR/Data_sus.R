library(tidyverse)

dengue_sp <- read.csv2("dengue_sp.csv")
View(dengue_sp)

dengue_sp <- dengue_sp %>%
  filter(Município.de.residência != "Total", 
         Município.de.residência != "&",
         !grepl("IGNORADO", Município.de.residência)
         ) # ! significa não

tail(dengue_sp)

View(dengue_sp)


## dados do IBGE

dados_ibge <- read.csv2("tabela4709.csv", skip = 3)

dados_ibge <- dados_ibge %>%
  rename(Cod = 1)

View(dados_ibge)


### Manipulando dados

# separar o cod. do município

dengue_sp <- dengue_sp %>%
  mutate(
    codigo = str_extract(Município.de.residência, "\\d{6}")
  )

dados_ibge <- dados_ibge %>%
  mutate(
    Cod2 = str_remove(Cod, "\\d$")
    )


## manipulando dengue

glimpse(dengue_sp)

dengue_sp <- dengue_sp %>%
  mutate(across(starts_with("x"), as.integer)) %>% # transforma em int
  replace(is.na(.), 0) # coloca 0 em lugar de NaN

dengue_sp <- dengue_sp %>%
  select(-Total) %>%
  pivot_longer(2:12, names_to = "Ano", values_to = "Casos")

dengue_sp <- dengue_sp %>%
  mutate(Ano = str_remove(Ano, "\\D")) %>%
  mutate(Ano = as.integer(Ano))

df_final <- dados_ibge %>%
  select(Cod = Cod2, Inhab = X2022) %>%
  right_join(dengue_sp, by = c("Cod" = "codigo")) 

df_final %>%
  mutate(
    incidencia = Casos/Inhab*100000
  )

df_final %>% filter(is.na(Inhab))

ggplot(df_final)+
  geom_boxplot(aes(x = factor(Ano), y = Casos))

ggplot(df_final)+
  geom_boxplot(aes(x = factor(Ano), y = incidencia))

