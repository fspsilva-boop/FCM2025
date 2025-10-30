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

### Lendo dados DBC

library(tidyverse)

devtools::install_github("danicat/read.dbc")

install.packages("read.dbc")

dados <- read.dbc::read.dbc("DENGBR24.dbc")

pkgbuild::check_build_tools(debug = TRUE)

devtools::install_github("danicat/read.dbc")

Sys.which("make")

writeLines('PATH="${RTOOLS45_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

dados <- read.dbc::read.dbc("DENGBR24.dbc")

Sys.which("make")
install.packages("read.dbc")
devtools::install_github("danicat/read.dbc")

dados <- read.dbc::read.dbc("DENGBR24.dbc")

dados_sp <- dados %>%
  filter(grepl("^35", ID_MN_RESI))

write.csv(dados_sp, "dados_dengue_sp.csv")

library(tidyverse)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir = "meubanco.duckdb")

dbExecute(con, "CREATE TABLE dados AS SELECT * FROM read_csv_auto('dados_dengue_sp.csv')")

dbWriteTable(con, "dados", dados_sp, overwrite = TRUE)

# Consultas SQL
dbGetQuery(con, "DESCRIBE dados")

sem_pri <- dbGetQuery(con, 'SELECT SEM_PRI FROM dados')
df_sem_prim <- data.frame(sem_pri)

df_sem_prim %>%
  group_by(SEM_PRI) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  ggplot()+
  geom_col(aes(x = SEM_PRI, y = n))+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5)
  )
  

