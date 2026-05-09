# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc = read.csv("SINASC_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
summary(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

summary(dados_sinasc_1$CODMUNRES)
UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)

# Considerando que a resolução é para o estado de Roraima, ou seja, UF = 14 
dados_sinasc_2 = dados_sinasc_1[UF == "14",]
dados_sinasc_2 = read.csv2("dados_sinasc_2.csv")
# Exportar o arquivo com o nome dados_sinasc_2.csv e apagando arquivos não mais necessários
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

rm(dados_sinasc, dados_sinasc_1)
gc()


# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA

dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "32 a 36 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5

# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))
dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)
dados_sinasc_2$F_PIG=ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                            ifelse(is.na(dados_sinasc_2$PESO)|is.na(dados_sinasc_2$PESO_P10)|is.na(dados_sinasc_2$PESO_P90),
                                   NA,
                                   ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                          ifelse(dados_sinasc_2$PESO<=dados_sinasc_2$PESO_P90, "AIG", "GIG"))))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))


# Tarefas 9 e 10 (reformulada)

# Crie um banco de dados contendo as 103 variáveis listadas no arquivo
# “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”
# O banco final deverá possuir:
#  103 colunas, correspondentes às variáveis especificadas;
#  n + 1 linhas, onde:
#  n corresponde ao número de municípios distintos da UF em análise
#  a primeira linha corresponde aos valores agregados para a UF como
# um todo;
# as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC 
# (dados_sinasc, dados_sinasc_1 e dados_sinasc_2), respeitando os nomes e 
# a ordem especificados.
#
#######################################################
# ETAPA 1: TAREFAS 9 E 10 - CONSOLIDAÇÃO DE INDICADORES
#######################################################

# 1. Funções auxiliares para manter a consistência do seu script
# Usamos as variáveis criadas nas Tarefas 7 e 8 [cite: 1]
calc_mun = function(coluna) as.vector(tapply(coluna, dados_sinasc_2$CODMUNRES, sum, na.rm = TRUE))
calc_mun_mean = function(coluna) as.vector(tapply(coluna, dados_sinasc_2$CODMUNRES, mean, na.rm = TRUE))
calc_mun_sd = function(coluna) as.vector(tapply(coluna, dados_sinasc_2$CODMUNRES, sd, na.rm = TRUE))
calc_mun_quant = function(coluna, p) as.vector(tapply(coluna, dados_sinasc_2$CODMUNRES, quantile, probs = p, na.rm = TRUE))

# 2. Criando o dataframe para os Municípios (Linhas n) [cite: 3]
municipios = data.frame(
  ANO = 2015, NIVEL = "MUNICIPIO", CODMUNRES = names(table(dados_sinasc_2$CODMUNRES)), # Col 1-3
  TN = as.vector(table(dados_sinasc_2$CODMUNRES)), # Col 4
  TNRC = as.vector(tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)), # Col 5
  TNRCR = as.vector(tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)), # Col 6
  
  # Informações sobre as gestantes - Idade (Col 7-16) [cite: 1]
  TGI_15 = calc_mun(dados_sinasc_2$IDADEMAE < 15),
  TGI_15_19 = calc_mun(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19),
  TGI_20_24 = calc_mun(dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24),
  TGI_25_29 = calc_mun(dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29),
  TGI_30_34 = calc_mun(dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34),
  TGI_35_39 = calc_mun(dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39),
  TGI_40_44 = calc_mun(dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44),
  TGI_45_49 = calc_mun(dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49),
  TGI_50 = calc_mun(dados_sinasc_2$IDADEMAE >= 50),
  TGIF = calc_mun(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 49),
  
  # Estatísticas de Idade Materna (Col 17-21) [cite: 1]
  IM_P25 = calc_mun_quant(dados_sinasc_2$IDADEMAE, 0.25),
  IM_P50 = calc_mun_quant(dados_sinasc_2$IDADEMAE, 0.50),
  IM_P75 = calc_mun_quant(dados_sinasc_2$IDADEMAE, 0.75),
  IM_MD = calc_mun_mean(dados_sinasc_2$IDADEMAE),
  IM_DP = calc_mun_sd(dados_sinasc_2$IDADEMAE),
  
  # Escolaridade e Raça (Col 22-32) [cite: 1]
  EM_S = calc_mun(dados_sinasc_2$ESCMAE2010 == "Sem escolaridade"),
  EM_FI = calc_mun(dados_sinasc_2$ESCMAE2010 == "Fundamental I (1ª a 4ª série)"),
  EM_FII = calc_mun(dados_sinasc_2$ESCMAE2010 == "Fundamental II (5ª a 8ª série)"),
  EM_M = calc_mun(dados_sinasc_2$ESCMAE2010 == "Médio (antigo 2º grau)"),
  EM_SI = calc_mun(dados_sinasc_2$ESCMAE2010 == "Superior incompleto"),
  EM_SC = calc_mun(dados_sinasc_2$ESCMAE2010 == "Superior completo"),
  TGRC_B = calc_mun(dados_sinasc_2$RACACORMAE == "Branca"),
  TGRC_PT = calc_mun(dados_sinasc_2$RACACORMAE == "Preta"),
  TGRC_A = calc_mun(dados_sinasc_2$RACACORMAE == "Amarela"),
  TGRC_PD = calc_mun(dados_sinasc_2$RACACORMAE == "Parda"),
  TGRC_I = calc_mun(dados_sinasc_2$RACACORMAE == "Indígena"),
  
  # Estado Civil e Paridade (Col 33-36) [cite: 1]
  TGSC = calc_mun(dados_sinasc_2$ESTCIV == "Sem companheiro"),
  TGCC = calc_mun(dados_sinasc_2$ESTCIV == "Com companheiro"),
  TGPRI = calc_mun(dados_sinasc_2$PARIDADE == "Nulípara"),
  TGNPRI = calc_mun(dados_sinasc_2$PARIDADE == "Multípara"),
  
  # Duração da Gestação (Col 37-52) [cite: 1, 3]
  TGU = calc_mun(dados_sinasc_2$GRAVIDEZ == "Única"),
  TGG = calc_mun(dados_sinasc_2$GRAVIDEZ %in% c("Dupla", "Tripla ou mais")),
  TGD_22 = calc_mun(dados_sinasc_2$GESTACAO == "Menos de 22 semanas"),
  TGD_22_27 = calc_mun(dados_sinasc_2$GESTACAO == "22 a 27 semanas"),
  TGD_28_31 = calc_mun(dados_sinasc_2$GESTACAO == "28 a 31 semanas"),
  TGD_32_36 = calc_mun(dados_sinasc_2$GESTACAO == "32 a 36 semanas"),
  TGD_37_41 = calc_mun(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41),
  TGD_42 = calc_mun(dados_sinasc_2$GESTACAO == "42 semanas e mais"),
  TGD_PRT = calc_mun(dados_sinasc_2$SEMAGESTAC < 37),
  TGD_AT = calc_mun(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41),
  TGD_PST = calc_mun(dados_sinasc_2$SEMAGESTAC >= 42),
  DG_P25 = calc_mun_quant(dados_sinasc_2$SEMAGESTAC, 0.25),
  DG_P50 = calc_mun_quant(dados_sinasc_2$SEMAGESTAC, 0.50),
  DG_P75 = calc_mun_quant(dados_sinasc_2$SEMAGESTAC, 0.75),
  DG_MD = calc_mun_mean(dados_sinasc_2$SEMAGESTAC),
  DG_DP = calc_mun_sd(dados_sinasc_2$SEMAGESTAC),
  
  # Pré-natal e Peregrinação (Col 53-59) [cite: 3]
  TKC_NR = calc_mun(dados_sinasc_2$KOTELCHUCK == "Não realizou pré-natal"),
  TKC_ID = calc_mun(dados_sinasc_2$KOTELCHUCK == "Inadequado"),
  TKC_IT = calc_mun(dados_sinasc_2$KOTELCHUCK == "Intermediário"),
  TKC_AD = calc_mun(dados_sinasc_2$KOTELCHUCK == "Adequado"),
  TKC_MAD = calc_mun(dados_sinasc_2$KOTELCHUCK == "Mais que adequado"),
  TGPRG_S = calc_mun(dados_sinasc_2$PERIG == "Sim"),
  TGPRG_N = calc_mun(dados_sinasc_2$PERIG == "Não"),
  
  # Parto e Apresentação (Col 60-64) [cite: 3]
  TPV = calc_mun(dados_sinasc_2$PARTO == "Vaginal"),
  TPC = calc_mun(dados_sinasc_2$PARTO == "Cesário"),
  TRAP_C = calc_mun(dados_sinasc_2$TPAPRESENT == "Cefálico"),
  TRAP_P = calc_mun(dados_sinasc_2$TPAPRESENT == "Pélvica ou podálica"),
  TRAP_T = calc_mun(dados_sinasc_2$TPAPRESENT == "Transversa"),
  
  # Grupos de Robson (Col 65-74) [cite: 3]
  TGROB_1 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 1"),
  TGROB_2 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 2"),
  TGROB_3 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 3"),
  TGROB_4 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 4"),
  TGROB_5 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 5"),
  TGROB_6 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 6"),
  TGROB_7 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 7"),
  TGROB_8 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 8"),
  TGROB_9 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 9"),
  TGROB_10 = calc_mun(dados_sinasc_2$TPROBSON == "Grupo 10"),
  
  # Local do Nascimento (Col 75-79) [cite: 3]
  TNLOC_H = calc_mun(dados_sinasc_2$LOCNASC == "Hospital"),
  TNLOC_ES = calc_mun(dados_sinasc_2$LOCNASC == "Outros estabelecimentos de saúde"),
  TNLOC_D = calc_mun(dados_sinasc_2$LOCNASC == "Domicílio"),
  TNLOC_O = calc_mun(dados_sinasc_2$LOCNASC == "Outros"),
  TNLOC_AI = calc_mun(dados_sinasc_2$LOCNASC == "Aldeias indígenas"),
  
  # Informações do Recém-nascido (Col 80-94) [cite: 3, 4]
  TRS_M = calc_mun(dados_sinasc_2$SEXO == "Masculino"),
  TRS_F = calc_mun(dados_sinasc_2$SEXO == "Feminino"),
  TRRC_B = calc_mun(dados_sinasc_2$RACACOR == "Branca"),
  TRRC_PT = calc_mun(dados_sinasc_2$RACACOR == "Preta"),
  TRRC_A = calc_mun(dados_sinasc_2$RACACOR == "Amarela"),
  TRRC_PD = calc_mun(dados_sinasc_2$RACACOR == "Parda"),
  TRRC_I = calc_mun(dados_sinasc_2$RACACOR == "Indígena"),
  TRP_BP = calc_mun(dados_sinasc_2$F_PESO == "Baixo peso"),
  TRP_N = calc_mun(dados_sinasc_2$F_PESO == "Peso normal"),
  TRP_M = calc_mun(dados_sinasc_2$F_PESO == "Macrossomia"),
  PESO_P25 = calc_mun_quant(dados_sinasc_2$PESO, 0.25),
  PESO_P50 = calc_mun_quant(dados_sinasc_2$PESO, 0.50),
  PESO_P75 = calc_mun_quant(dados_sinasc_2$PESO, 0.75),
  PESO_MD = calc_mun_mean(dados_sinasc_2$PESO),
  PESO_DP = calc_mun_sd(dados_sinasc_2$PESO),
  
  # PIG, Apgar e Anomalias (Col 95-103) [cite: 4]
  TRPIG_P = calc_mun(dados_sinasc_2$F_PIG == "PIG"),
  TRPIG_A = calc_mun(dados_sinasc_2$F_PIG == "AIG"),
  TRPIG_G = calc_mun(dados_sinasc_2$F_PIG == "GIG"),
  TRAPG5_B = calc_mun(dados_sinasc_2$F_APGAR5 == "Baixo"),
  TRAPG5_N = calc_mun(dados_sinasc_2$F_APGAR5 == "Normal"),
  APG5_MD = calc_mun_mean(dados_sinasc_2$APGAR5),
  APG5_DP = calc_mun_sd(dados_sinasc_2$APGAR5),
  TRAC = calc_mun(dados_sinasc_2$IDANOMAL == "Sim"),
  TRSAC = calc_mun(dados_sinasc_2$IDANOMAL == "Não")
)

# 3. Criando a linha para a UF (Total do Estado) 
# IMPORTANTE: Esta linha deve conter as mesmas 103 colunas na mesma ordem
uf_linha = data.frame(
  ANO = 2015, NIVEL = "UF", CODMUNRES = "14",
  TN = nrow(dados_sinasc_2),
  TNRC = sum(complete.cases(dados_sinasc_2)),
  TNRCR = sum(complete.cases(dados_sinasc_2)),
  
  TGI_15 = sum(dados_sinasc_2$IDADEMAE < 15, na.rm = T),
  TGI_15_19 = sum(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19, na.rm = T),
  TGI_20_24 = sum(dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24, na.rm = T),
  TGI_25_29 = sum(dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29, na.rm = T),
  TGI_30_34 = sum(dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34, na.rm = T),
  TGI_35_39 = sum(dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39, na.rm = T),
  TGI_40_44 = sum(dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44, na.rm = T),
  TGI_45_49 = sum(dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49, na.rm = T),
  TGI_50 = sum(dados_sinasc_2$IDADEMAE >= 50, na.rm = T),
  TGIF = sum(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 49, na.rm = T),
  IM_P25 = quantile(dados_sinasc_2$IDADEMAE, 0.25, na.rm = T),
  IM_P50 = quantile(dados_sinasc_2$IDADEMAE, 0.50, na.rm = T),
  IM_P75 = quantile(dados_sinasc_2$IDADEMAE, 0.75, na.rm = T),
  IM_MD = mean(dados_sinasc_2$IDADEMAE, na.rm = T),
  IM_DP = sd(dados_sinasc_2$IDADEMAE, na.rm = T),
  
  EM_S = sum(dados_sinasc_2$ESCMAE2010 == "Sem escolaridade", na.rm = T),
  EM_FI = sum(dados_sinasc_2$ESCMAE2010 == "Fundamental I (1ª a 4ª série)", na.rm = T),
  EM_FII = sum(dados_sinasc_2$ESCMAE2010 == "Fundamental II (5ª a 8ª série)", na.rm = T),
  EM_M = sum(dados_sinasc_2$ESCMAE2010 == "Médio (antigo 2º grau)", na.rm = T),
  EM_SI = sum(dados_sinasc_2$ESCMAE2010 == "Superior incompleto", na.rm = T),
  EM_SC = sum(dados_sinasc_2$ESCMAE2010 == "Superior completo", na.rm = T),
  TGRC_B = sum(dados_sinasc_2$RACACORMAE == "Branca", na.rm = T),
  TGRC_PT = sum(dados_sinasc_2$RACACORMAE == "Preta", na.rm = T),
  TGRC_A = sum(dados_sinasc_2$RACACORMAE == "Amarela", na.rm = T),
  TGRC_PD = sum(dados_sinasc_2$RACACORMAE == "Parda", na.rm = T),
  TGRC_I = sum(dados_sinasc_2$RACACORMAE == "Indígena", na.rm = T),
  
  TGSC = sum(dados_sinasc_2$ESTCIV == "Sem companheiro", na.rm = T),
  TGCC = sum(dados_sinasc_2$ESTCIV == "Com companheiro", na.rm = T),
  TGPRI = sum(dados_sinasc_2$PARIDADE == "Nulípara", na.rm = T),
  TGNPRI = sum(dados_sinasc_2$PARIDADE == "Multípara", na.rm = T),
  
  TGU = sum(dados_sinasc_2$GRAVIDEZ == "Única", na.rm = T),
  TGG = sum(dados_sinasc_2$GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), na.rm = T),
  TGD_22 = sum(dados_sinasc_2$GESTACAO == "Menos de 22 semanas", na.rm = T),
  TGD_22_27 = sum(dados_sinasc_2$GESTACAO == "22 a 27 semanas", na.rm = T),
  TGD_28_31 = sum(dados_sinasc_2$GESTACAO == "28 a 31 semanas", na.rm = T),
  TGD_32_36 = sum(dados_sinasc_2$GESTACAO == "32 a 36 semanas", na.rm = T),
  TGD_37_41 = sum(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41, na.rm = T),
  TGD_42 = sum(dados_sinasc_2$GESTACAO == "42 semanas e mais", na.rm = T),
  TGD_PRT = sum(dados_sinasc_2$SEMAGESTAC < 37, na.rm = T),
  TGD_AT = sum(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41, na.rm = T),
  TGD_PST = sum(dados_sinasc_2$SEMAGESTAC >= 42, na.rm = T),
  DG_P25 = quantile(dados_sinasc_2$SEMAGESTAC, 0.25, na.rm = T),
  DG_P50 = quantile(dados_sinasc_2$SEMAGESTAC, 0.50, na.rm = T),
  DG_P75 = quantile(dados_sinasc_2$SEMAGESTAC, 0.75, na.rm = T),
  DG_MD = mean(dados_sinasc_2$SEMAGESTAC, na.rm = T),
  DG_DP = sd(dados_sinasc_2$SEMAGESTAC, na.rm = T),
  
  TKC_NR = sum(dados_sinasc_2$KOTELCHUCK == "Não realizou pré-natal", na.rm = T),
  TKC_ID = sum(dados_sinasc_2$KOTELCHUCK == "Inadequado", na.rm = T),
  TKC_IT = sum(dados_sinasc_2$KOTELCHUCK == "Intermediário", na.rm = T),
  TKC_AD = sum(dados_sinasc_2$KOTELCHUCK == "Adequado", na.rm = T),
  TKC_MAD = sum(dados_sinasc_2$KOTELCHUCK == "Mais que adequado", na.rm = T),
  TGPRG_S = sum(dados_sinasc_2$PERIG == "Sim", na.rm = T),
  TGPRG_N = sum(dados_sinasc_2$PERIG == "Não", na.rm = T),
  
  TPV = sum(dados_sinasc_2$PARTO == "Vaginal", na.rm = T),
  TPC = sum(dados_sinasc_2$PARTO == "Cesário", na.rm = T),
  TRAP_C = sum(dados_sinasc_2$TPAPRESENT == "Cefálico", na.rm = T),
  TRAP_P = sum(dados_sinasc_2$TPAPRESENT == "Pélvica ou podálica", na.rm = T),
  TRAP_T = sum(dados_sinasc_2$TPAPRESENT == "Transversa", na.rm = T),
  
  TGROB_1 = sum(dados_sinasc_2$TPROBSON == "Grupo 1", na.rm = T),
  TGROB_2 = sum(dados_sinasc_2$TPROBSON == "Grupo 2", na.rm = T),
  TGROB_3 = sum(dados_sinasc_2$TPROBSON == "Grupo 3", na.rm = T),
  TGROB_4 = sum(dados_sinasc_2$TPROBSON == "Grupo 4", na.rm = T),
  TGROB_5 = sum(dados_sinasc_2$TPROBSON == "Grupo 5", na.rm = T),
  TGROB_6 = sum(dados_sinasc_2$TPROBSON == "Grupo 6", na.rm = T),
  TGROB_7 = sum(dados_sinasc_2$TPROBSON == "Grupo 7", na.rm = T),
  TGROB_8 = sum(dados_sinasc_2$TPROBSON == "Grupo 8", na.rm = T),
  TGROB_9 = sum(dados_sinasc_2$TPROBSON == "Grupo 9", na.rm = T),
  TGROB_10 = sum(dados_sinasc_2$TPROBSON == "Grupo 10", na.rm = T),
  
  TNLOC_H = sum(dados_sinasc_2$LOCNASC == "Hospital", na.rm = T),
  TNLOC_ES = sum(dados_sinasc_2$LOCNASC == "Outros estabelecimentos de saúde", na.rm = T),
  TNLOC_D = sum(dados_sinasc_2$LOCNASC == "Domicílio", na.rm = T),
  TNLOC_O = sum(dados_sinasc_2$LOCNASC == "Outros", na.rm = T),
  TNLOC_AI = sum(dados_sinasc_2$LOCNASC == "Aldeias indígenas", na.rm = T),
  
  TRS_M = sum(dados_sinasc_2$SEXO == "Masculino", na.rm = T),
  TRS_F = sum(dados_sinasc_2$SEXO == "Feminino", na.rm = T),
  TRRC_B = sum(dados_sinasc_2$RACACOR == "Branca", na.rm = T),
  TRRC_PT = sum(dados_sinasc_2$RACACOR == "Preta", na.rm = T),
  TRRC_A = sum(dados_sinasc_2$RACACOR == "Amarela", na.rm = T),
  TRRC_PD = sum(dados_sinasc_2$RACACOR == "Parda", na.rm = T),
  TRRC_I = sum(dados_sinasc_2$RACACOR == "Indígena", na.rm = T),
  TRP_BP = sum(dados_sinasc_2$F_PESO == "Baixo peso", na.rm = T),
  TRP_N = sum(dados_sinasc_2$F_PESO == "Peso normal", na.rm = T),
  TRP_M = sum(dados_sinasc_2$F_PESO == "Macrossomia", na.rm = T),
  PESO_P25 = quantile(dados_sinasc_2$PESO, 0.25, na.rm = T),
  PESO_P50 = quantile(dados_sinasc_2$PESO, 0.50, na.rm = T),
  PESO_P75 = quantile(dados_sinasc_2$PESO, 0.75, na.rm = T),
  PESO_MD = mean(dados_sinasc_2$PESO, na.rm = T),
  PESO_DP = sd(dados_sinasc_2$PESO, na.rm = T),
  
  TRPIG_P = sum(dados_sinasc_2$F_PIG == "PIG", na.rm = T),
  TRPIG_A = sum(dados_sinasc_2$F_PIG == "AIG", na.rm = T),
  TRPIG_G = sum(dados_sinasc_2$F_PIG == "GIG", na.rm = T),
  TRAPG5_B = sum(dados_sinasc_2$F_APGAR5 == "Baixo", na.rm = T),
  TRAPG5_N = sum(dados_sinasc_2$F_APGAR5 == "Normal", na.rm = T),
  APG5_MD = mean(dados_sinasc_2$APGAR5, na.rm = T),
  APG5_DP = sd(dados_sinasc_2$APGAR5, na.rm = T),
  TRAC = sum(dados_sinasc_2$IDANOMAL == "Sim", na.rm = T),
  TRSAC = sum(dados_sinasc_2$IDANOMAL == "Não", na.rm = T)
)

# Combinando UF e Municípios e Exportando
SINASC_RR = rbind(uf_linha, municipios)


# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv2(SINASC_RR, "SINASC_RR.csv")

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commit "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM e vá para ela
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

dados_sim = read.csv2("Mortalidade_Geral_2015.csv")

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, 
# CODMUNRES, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_1 = dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de 
# CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

dados_sim_2 = subset(dados_sim_1, substr(CODMUNRES, 1, 2) == "14")


# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645    
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975


# Exportar o arquivo com o nome dados_sim_2.csv
write.csv2(dados_sim_2, "dados_sim_2.csv")

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE


# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main
