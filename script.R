
# DIRETÓRIO ---------------------------------------------------------------



getwd()

setwd("C:/Users/andre/Documents/Faculdade/Rstudio/Minicursos/Introdução ao R/")

getwd()

# ativando pacotes
library(tidyverse)

?dplyr::arrange
?dplyr

# Vetores -----------------------------------------------------------------
# Vetor numérico (inteiro)
a <- c(1:5)

# Vetor de caracteres
b <- c("a", "b", "c", "d", "e")

# Vetor lógico
c <- c(TRUE, T, FALSE, F, F)

# Vetor númerico (números reais)
d <- c(1.12, 2.32, 3.41, 4.03, 5.98)

a + d
a / d
a %% d
a %/% d

c(T, T, F) == c(1, 1, 0)

# Conversão de dados ------------------------------------------------------

c2 <- as.numeric(c)

a2 <- as.character(a)

A <- c(1:20)
A <- seq(from = 1, to = 20, by = 1)

a <- seq(from = 1, to = 20, 1)

a + A

# Acessar posição dos vetores ---------------------------------------------

nomes <- c("João", "Roberto", "Alice", "José")

nomes[1]
nomes[-1]
nomes[1:3]
nomes[c(1, 3)]

nomes[1, 3]

vetor_logico <- c(T, T, F, F)

nomes[vetor_logico]

# Função all e any
all(vetor_logico)

any(vetor_logico)

# Modificando os vetores --------------------------------------------------

a <- rep(10, 5)

a <- seq(1, 20, 1)

a[a > 10] <- "texto"

a[a > 10] <- 2

# Coeração dos dados ------------------------------------------------------
a <- c(1, 2, 3, T, "a")

class(a)

b <- c(T, F, 1, 2)

class(b)

intervalo <- c("1 a 2 anos", "3 a 4 anos")
class(intervalo)

as.factor(intervalo)

a <- c(1, 3, 5, NA)
class(a)

b <- c(1:5, "-")
class(b)


# Data frames -------------------------------------------------------------

base <- data.frame(
  a = a, b= b, c = c, d = d
)

base <- data.frame(
  nomes = c("Roberto", "Lucas", "Alice", "Fábio"),
  idade = c(20:23),
  altura = c(170, 150, 120, 180),
  massa = c(50, 60, 70, 80)
)

altura = c(170, 150, 120, 180)

# Acessando posição do data frame
base$altura[c(1, 3)]

class(base)

df <- data.frame(
  nome = c("Darth Vader", "Luke Skywalker", "Leia Organa", "Obi-Wan Kenobi"),
  altura = c(202, 172, 150, 182),
  massa = c(136, 77, 49, 77),
  genero = c("Masculino", "Masculino", "Feminino", "Masculino")
)

class(df)

dados <- tibble::as_tibble(df)

class(dados)

df

dplyr::glimpse(dados::dados_starwars)

sw <- dados::dados_starwars

# Categorizando os dados --------------------------------------------------

dados <- tibble(
  nome = c("Darth Vader", "Luke Skywalker", "Leia Organa", "Obi-Wan Kenobi"),
  altura = c(202, 172, 150, 182),
  massa = c(136, 77, 49, 77),
  genero = c("Masculino", "Masculino", "Feminino", "Masculino")
)

porte <- c("Muito alto", "Médio", "Baixo", "Alto")

levels(as.factor(porte))

porte_levels <- factor(porte, levels = c("Baixo", "Médio", "Alto", "Muito alto"),
                       labels = c("Baixo", "Médio", "Alto", "Muito alto"),
                       ordered = T)

# Adicionando aos dados
dados$porte <- porte_levels

class(dados$porte)

# Outra forma
dados$porte <- factor(porte, levels = c("Baixo", "Médio", "Alto", "Muito alto"),
                      labels = c("Baixo", "Médio", "Alto", "Muito alto"),
                      ordered = T)

# Exemplificando com NA's -------------------------------------------------

dados <- tibble(
  nome = c("Darth Vader", NA, "Leia Organa", "Obi-Wan Kenobi"),
  altura = c(202, 172, 150, NA),
  massa = c(136, 77, NA, 77),
  genero = c("Masculino", "Masculino", "Feminino", NA)
)

# Diferença entre NA e NULL
NA == NULL

class(dados$nome)

# Nomeando vetores --------------------------------------------------------
nomesnotas <- c("João" = 5, "Pedro" = 8, "Roberto" = 1, "Alice" = 10)

nomesnotas[1]
nomesnotas[2]

nomesnotas["João"]
nomesnotas["Alice"]

nomesnotas[nomesnotas > 5]

dados |>
  dplyr::arrange(desc(porte))

# Operações sem operador pipe
dados$teste <- (dados$altura * dados$massa) / 100

# Com dplyr
dados |>
  mutate(teste = altura * massa) |>
  arrange(desc(porte))


mean(dados$altura, na.rm = TRUE)

data(package = "datasets")

install.packages("devtools")

devtools::install_github("drewmelo/beautyxtrar", force = T)

library(dados)

mtcars

dados::mtcarros

matriz_empilhada = array(c(1:12), dim = c(2, 3, 2))

cars <- head(cars, n = 5)

lista <- list(
  dados,
  hobbies = c("Leitura", "Esportes", "Música"),
  matriz = matrix(1:9, nrow = 3, dimnames = list(c('A', 'B', 'C'),
                                                 c('A', 'B', 'C'))),
  matriz_empilhada = array(c(1:12), dim = c(2, 3, 2)),
  cars = head(cars, 5),
  mtcars = head(mtcars, 5)
)

lista$cars[[1]]

getwd()

# Necessário ter pacote openxlsx

install.packages("openxlsx")
library(openxlsx)

openxlsx::write.xlsx(dados, file = "dados/dados.xlsx", rowNames = FALSE)

wr(dados, file = "dados/dados.xlsx", rowNames = FALSE)


# Importando dados --------------------------------------------------------

# Ativando o pacote
library(readxl)

dados_importado <- readxl::read_excel(path = "dados/dados.xlsx")
