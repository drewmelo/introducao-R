library(tidyverse)


# DIRETÓRIO ---------------------------------------------------------------



getwd()

setwd("C:/Users/andre/Documents/Faculdade/Rstudio/Minicursos/Introdução ao R/")

getwd()

# ativando pacotes
library(tidyverse)

?dplyr::arrange
?dplyr
?rbcb

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

# devtools::install_github("drewmelo/beautyxtrar", force = T)

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

## PRÁTICAS NO R -----------------------------------------------------------

library(tidyverse)

dados <- tibble(
  nome = c("Darth Vader", "Luke Skywalker", "Leia Organa", "Obi-Wan Kenobi"),
  altura = c(202, 172, 150, 182),
  massa = c(136, 77, 49, 77),
  genero = c("Masculino", "Masculino", "Feminino", "Masculino")
)

dados

lista <- list(
  dados = dados,
  hobbies = c("Leitura", "Esportes", "Música"),
  matriz = matrix(1:9, nrow = 3, dimnames = list(c('A', 'B', 'C'),
                                                 c('A', 'B', 'C'))),
  matriz_empilhada = array(c(1:12), dim = c(2, 3, 2)),
  cars = head(cars, 5),
  mtcars = head(mtcars, 5)
)

lista$dados[[1]]

a <- c("Darth Vader",    "Luke Skywalker", "Leia Organa",    "Obi-Wan Kenobi")

lista[[1]][1 , ]

lista$dados[lista$dados$altura > 170]

# Outra alternativa
lista$dados[lista$dados$nome == "Luke Skywalker", "genero"] <- "Feminino"

library(readxl)
dados <- read_excel("C:/users/andre/Downloads/dados.xlsx")

arrange(.data = dados, genero)

# IF e ELSE ---------------------------------------------------------------

library(tidyverse)

nota <- 6

if (nota >= 7) {
  print("Parabéns! Você foi aprovado")
} else {
  print("Você não passou!")
}

nota <- 4

if (nota >= 7) {
  cat("Parabéns! passou.")
} else if (nota >= 4 & nota < 7) {
  cat("Você está de recuperação.")
} else {
  cat("Reprovado!")
}


# Diferenças entre operadores lógicos (& e &&) e (| e ||) -----------------

dados2 <- tibble(
  a = c(1, 0, 1),
  b = c(0, 0, 1),
  c = c("João", "João", "João"),
  d = c("João", "João", "João")
)

dados2 |>
  filter(a & b)

dados2 |>
  filter(a && b)

dados2 |>
  filter(a | b)

dados2 |>
  filter(a || b)

# Aplicando && e || em funções --------------------------------------------

a <- T
b <- F

if (a && b) {
  paste("Ambos são TRUE")
} else {
  paste("Um ou ambos são FALSE")
}

if (a || b) {
  cat("Pelo menos um entre 'a' e 'b' são TRUE")
} else {
  cat("Ambos são FALSE")
}

c <- T

if (a && c(c || d)) {
  cat("'a' é verdadeiro e pelo menos um entre 'b' e 'c' é verdadeiro")
} else if (!a && c(b && c)) {
  cat("'a' é falso e b e c são verdadeiros")
} else {
  cat("Nenhuma das condições foram atendidas")
}

# Opções alternativas
# ifelse e if_else

# Função Switch -----------------------------------------------------------

dia <- 3

switch (dia,
        "domingo" = {
          cat("Hoje é domingo.")
        },
        "segunda" = {
          cat("Hoje é segunda")
        },
        "terça" = {
          cat("Hoje é terça")
        }
)

# Função for
for (i in 1:10) {
  print(c("Estou em um loop", i))
}

max(cars$speed) - min(cars$speed)

veloc <- max(cars$speed)

# Definir os objetos do loop
veloc_min <- -99999999999999999
veloc_min <- -Inf

for (i in veloc) {
  if (i > veloc_min) veloc_min = i
}

#

# Função repeat -----------------------------------------------------------
# Inicializando uma variável
contador <- 1

# Loop repeat
repeat {
  print(contador)
  contador <- contador + 1
  if (contador > 5) {
    break
  }
}

n = 5
result = 1

# Loop repeat
repeat {
  result <-  result * n
  n <- n - 1
  if ( n <= 1) {
    break
  }
}

cat("A fatorial de 5 é:", result)

# Amostra aleatória
set.seed(125)

sample(x = 1:10, size = 20, replace = T)

x <- factor(sample(letters[1:10], size = 20, replace = T))

y <- ifelse(x %in% c("a", "b", "c"), yes = x, no = NA)

y_novo <- if_else(x %in% c("a", "b", "c"), true = x, false = NA)

# Função para calcular o IMC ----------------------------------------------

dados_sw <- dados::dados_starwars |>
  select(1:5)

calculo_imc <- function(data, peso_col, altura_col) {
  # Tornando o imc (vetor) númerico
  imc <- numeric(length = nrow(data))

  # Verificar se há NA nas colunas peso e altura (argumentos) -- Somente observações completas
  dados_full <- complete.cases(data[[peso_col]],  data[[altura_col]])

  imc[dados_full] <- data[[peso_col]][dados_full]  / ( data[[altura_col]][dados_full] / 100 )^2

  categorias <- character(length = length(imc))

  # Categorizando os resultados
  categorias[imc < 18.5] <- "Abaixo do peso"
  categorias[imc <= 18.5 & imc < 25] <- "Peso normal"
  categorias[imc >= 25] <- "Acima do peso"

  # Adicionando NA's
  categorias[!dados_full] <- NA

  return(categorias)
}

# Aplicando essa função aos nossos dados
dados_sw$imc_categorizado <- calculo_imc(data = dados_sw,
                                         altura_col = "altura",
                                         peso_col = "massa")


# Aplicando a função a outros dados
dados_pessoas <- dados::pessoas |>
  select(sobrenome, altura = peso, peso = altura)

dados_pessoas$novo_imc <- calculo_imc(altura_col = "altura",
                                      peso_col = "peso", data = dados_pessoas)

write.csv(x = dados_pessoas, file = "dados/dados_pessoas.csv", row.names = F)

# Importando os mesmos dados
dados <- read_csv(file = "dados/dados_pessoas.csv")

# ANÁLISE ESTATÍSTICA -----------------------------------------------------

as_tibble(pessoas) |>
  select(nome, peso, altura) |>
  mutate(categoria = calcular_imc(data = pessoas,
                                  peso_col = "peso",
                                  altura_col = "altura")) |>
  head(n = 5)


# ANÁLISE ESTATÍSTICA -----------------------------------------------------
dados2 <- dados |>
  mutate(novo_imc = factor(novo_imc,
                           levels = c("Abaixo do peso",
                                      "Peso normal",
                                      "Acima do peso"), ordered = TRUE),
         imc = peso / (altura / 100)^2 )

v <- c(rep(10, 8), 1:15, 20:40)

uniquev <- unique(v)

# Função para calcular a moda
calcular_moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

calcular_moda(v)

# Função para calcular todas as operações estatísticas em uma coluna do dataset starwars
resumo_estatistico <- function(df, coluna) {
  media <- mean(df[[coluna]], na.rm = TRUE)
  soma <- sum(df[[coluna]], na.rm = TRUE)
  variancia <- var(df[[coluna]], na.rm = TRUE)
  desvio_padrao <- sd(df[[coluna]], na.rm = TRUE)
  moda <- calcular_moda(df[[coluna]])

  return(list(
    media = media,
    soma = soma,
    variancia = variancia,
    desvio_padrao = desvio_padrao,
    moda = moda
  ))
}

# Exemplo de uso
resumo_altura <- resumo_estatistico(dados2, "altura")

summary(dados2)

dados2 |>
  ggplot(aes(x = altura, y = massa)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 159))

# Histograma da altura
hist(dados$altura, breaks = 10, col = "lightblue3", main = "Histograma de Altura", xlab = "Altura (cm)", ylab = "Frequência")

# Boxplot da massa
boxplot(dados$massa, main = "Boxplot de Massa", ylab = "Massa (kg)", col = "lightblue")


# Recapitulando -----------------------------------------------------------

a <- c(T, T, F)
b <- c(T, T, T)

a == b

a <- c(1, 1, 1)
b <- c(1, 1, 1)

a == b

# Continuação de funções --------------------------------------------------

install.packages("dados")

library(dados)

dados <- dados::dados_starwars |>
  select(1:5)

calculo_imc <- function(peso, altura) {
  # Imprimir um valor de erro
  if (peso <= 0) {
    stop("O peso deve ser um valor positivo")
  }
  if (altura <= 0) {
    stop("Altura deve ser um valor positivo")
  }

  # Calcular o IMC
  imc <- peso / (altura)^2

  # Determinar as categorias do IMC
  categorias <- if_else(imc < 18.5, true = "Abaixo do peso",
                        if_else(imc <= 25, true = "Peso normal",
                                if_else(imc > 25, true = "Peso normal", false = "Desconhecido") ) )
  return(list(valor_imc = round(imc, digits = 2), categoria = categorias))
}

dados2$altura_metros_rbase <- dados2$altura / 100

dados2 <- dados |>
  mutate(altura_categoria = if_else(altura > 180, true = "Alto", "Baixo ou Médio"),
         altura_metros = altura / 100,
         altura_metros_fator = factor(altura_categoria, levels = c("Baixo ou Médio", "Alto"),
                                      labels = c("Baixo ou Médio", "Alto"),
                                      ordered = T)) |>
  arrange(desc(altura_metros_fator))

peso <- 70
altura <- 1.68

resultado <- calculo_imc(peso, altura)

# ANÁLISE ESTATÍSTICA -----------------------------------------------------
med <- mean(dados$altura, na.rm = T)

var <- var(dados$altura, na.rm = T)

dp <- sd(dados$altura, na.rm = T)

resumo_estatistico <- function(df, coluna) {
  media <- mean(df[[coluna]], na.rm = T)
  soma <- sum(df[[coluna]], na.rm = T)
  variancia <- var(df[[coluna]], na.rm = T)
  desvio_padrao <- var(df[[coluna]], na.rm = T)

  return(tibble(
    media = media,
    soma = soma,
    variancia = variancia,
    desvio_padrao = desvio_padrao
  ))
}

resultado_estatistica <- resumo_estatistico(dados, "altura")

dplyr::group_by()
dplyr::select()

# VISUALIZAÇÃO DE DADOS ---------------------------------------------------

hist(mtcars$mpg, breaks = 5, col = "lightblue3", main = "Histograma de MPG",
     xlab = "MPG", ylab = "Frequência")

# Utilizando o ggplot2

mtcars |>
  ggplot() +
  geom_histogram(aes(x = mpg))

ggplot(data = dados, aes(x = altura, y = massa)) +
  geom_point() +
  geom_line()

# Outra forma
ggplot(data = dados2) +
  geom_point(aes(x = altura, y = massa))

# Links -------------------------------------------------------------------
# Banco Central do Brasil
"https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries"

# Inflação
"https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html?=&t=series-historicas"

install.packages("rbcb")
install.packages("zoo")

library(rbcb)
library(zoo)

ipca <- rbcb::get_series(code = "433", start_date = "2000-01-01") |>
  rename(ipca = `433`)

class(ipca$date)

install.packages("scales")
library(scales)

install.packages("devtools")

devtools::install_github("drewmelo/beautyxtrar", force = T)
library(beautyxtrar)


# GRÁFICO DO IPCA ---------------------------------------------------------

ipca_acumulado <- ipca |>
  mutate(acumulado_doze_meses = zoo::rollapply(ipca, 12,
                                               function(x)
                                                 (prod(1 + x / 100) - 1) * 100, # Referência: Análise macro
                                                 fill = NA,
                                                 align = "right"),
         acumulado_doze_meses = round(acumulado_doze_meses, digits = 2)
         ) |>
  tidyr::drop_na()

p <- ipca_acumulado |>
  ggplot(aes(x = date, y = acumulado_doze_meses)) +
  geom_line(linewidth = 1.1, col = "coral3") +
  scale_x_date(breaks = "2 year",
               labels = scales::label_date(format = "%Y")) +
  labs(x = "Ano", y = "Variação Doze Meses [%]") +
  beautyxtrar::theme_xtra()

ggsave(filename ="figuras/figura1.png", plot = p,
       width = 16, height = 9, dpi = 300)
