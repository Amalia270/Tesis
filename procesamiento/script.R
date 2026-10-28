# PASO 0: PAQUETES Y OPCIONES
options(repos = c(CRAN = "https://cloud.r-project.org"))
   if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

rm(list = ls())                         # limpiar el entorno
options(scipen = 999)                   # sin notación científica
options(survey.lonely.psu = "adjust")   # por si un estrato tiene un solo conglomerado

pacman::p_load(tidyverse,   # manipulación de datos (dplyr, tidyr, readr)
               haven,       # bases con etiquetas
               survey,      # diseño muestral complejo
               srvyr,       # survey con sintaxis de dplyr
               kableExtra)  # formato de tablas (Viewer y PDF)

# PASO 1: CARGAR LA BASE Y SELECCIONAR VARIABLES
load("input/data/casen_2024.RData")
dim(casen_2024)

casen <- casen_2024 %>%
  select(edad, sexo, educc,                       # predisponentes
         s13, ypchtotcor, region, area, v35c,     # facilitadores
         s16, h7a, h7b, h7c, h7d, h7e, h7f,       # necesidad
         s20a_preg, s23a_preg, s27a,              # utilización
         expr, varstrat, varunit)                 # diseño muestral

rm(casen_2024)
dim(casen)


# PASO 2: FACTORES PREDISPONENTES
cortes_edad  <- c(17, 34, 49, 64, 79, Inf)     # 17 = límite inferior (los menores de 18 quedan NA)
nombres_edad <- c("18-34", "35-49", "50-64", "65-79", "80+")

casen <- casen %>%
  mutate(edad = as.numeric(edad),
         grupo_edad = cut(edad, breaks = cortes_edad, labels = nombres_edad))

table(casen$grupo_edad, useNA = "ifany")

# Sexo
casen <- casen %>%
  mutate(sexo = factor(as.numeric(sexo), levels = c(1, 2),
                       labels = c("Hombre", "Mujer")))

table(casen$sexo, useNA = "ifany")

# Nivel educacional
casen <- casen %>%
  mutate(educc_num = as.numeric(educc),
         educc_num = ifelse(educc_num < 0, NA, educc_num),   # -88 "no sabe" -> NA
         educ = case_when(
           educc_num %in% 0:1 ~ "Básica incompleta o menos",
           educc_num %in% 2:3 ~ "Básica completa o media incompleta",
           educc_num == 4     ~ "Media completa",
           educc_num %in% 5:6 ~ "Superior"),
         educ = factor(educ, levels = c("Básica incompleta o menos",
                                        "Básica completa o media incompleta",
                                        "Media completa",
                                        "Superior")))

table(casen$educc_num, casen$educ, useNA = "ifany")   


# PASO 3: FACTORES FACILITADORES
# Previsión de salud: solo FONASA e Isapre. 
casen <- casen %>%
  mutate(s13_num = as.numeric(s13),
         s13_num = ifelse(s13_num < 0, NA, s13_num),     # -88 "no sabe" -> NA
         prevision = case_when(
           s13_num == 1 ~ "FONASA",
           s13_num == 2 ~ "Isapre"),
         prevision = factor(prevision, levels = c("FONASA", "Isapre")))

table(casen$s13_num, casen$prevision, useNA = "ifany")

# Quintiles de ingreso total per cápita del hogar
# Los cortes se calculan con el factor de expansión, sobre la población total
cuantil_pond <- function(x, peso, probs) {
  datos <- data.frame(x = x, peso = peso)
  datos <- datos[!is.na(datos$x) & !is.na(datos$peso), ]   # sacar perdidos
  datos <- datos[order(datos$x), ]                         # ordenar por ingreso
  acumulado <- cumsum(datos$peso) / sum(datos$peso)        # % acumulado de población
  sapply(probs, function(p) datos$x[which(acumulado >= p)[1]])
}

cortes <- cuantil_pond(casen$ypchtotcor, casen$expr, c(0.2, 0.4, 0.6, 0.8))
cortes

casen <- casen %>%
  mutate(quintil = cut(ypchtotcor,
                       breaks = c(-Inf, cortes, Inf),
                       labels = c("Q1", "Q2", "Q3", "Q4", "Q5")))

table(casen$quintil, useNA = "ifany")

# Región (para los modelos), área y cercanía a un centro de salud
casen <- casen %>%
  mutate(region_f = relevel(factor(as.numeric(region)), ref = "13"),   # 13 = RM
         area_f   = factor(as.numeric(area), levels = c(1, 2),
                           labels = c("Urbano", "Rural")),
         v35c_num = as.numeric(v35c),
         v35c_num = ifelse(v35c_num < 0, NA, v35c_num),                # -88 "no sabe" -> NA
         cerca_cs = factor(v35c_num, levels = c(1, 2), labels = c("Sí", "No")))

table(casen$area_f, useNA = "ifany")
table(casen$cerca_cs, useNA = "ifany")


# PASO 4: FACTORES DE NECESIDAD
# Problema de salud reciente (s16)
casen <- casen %>%
  mutate(s16_num = as.numeric(s16),
         s16_num = ifelse(s16_num < 0, NA, s16_num),     # -88 "no sabe" -> NA
         prob_reciente = case_when(
           s16_num %in% 1:4 ~ 1,     # tuvo algún problema de salud
           s16_num == 5     ~ 0))    # no tuvo ninguno

table(casen$s16_num, casen$prob_reciente, useNA = "ifany")

# Limitación funcional (h7a a h7f)
# 4a: pasar a número y los códigos negativos a NA
casen <- casen %>%
  mutate(h7a = ifelse(as.numeric(h7a) < 0, NA, as.numeric(h7a)),
         h7b = ifelse(as.numeric(h7b) < 0, NA, as.numeric(h7b)),
         h7c = ifelse(as.numeric(h7c) < 0, NA, as.numeric(h7c)),
         h7d = ifelse(as.numeric(h7d) < 0, NA, as.numeric(h7d)),
         h7e = ifelse(as.numeric(h7e) < 0, NA, as.numeric(h7e)),
         h7f = ifelse(as.numeric(h7f) < 0, NA, as.numeric(h7f)))

# 4b: 1 si hay "mucha dificultad" (3) o "no puede" (4) en al menos un dominio
casen <- casen %>%
  mutate(
    mucha_dif  = h7a %in% 3:4 | h7b %in% 3:4 | h7c %in% 3:4 |
      h7d %in% 3:4 | h7e %in% 3:4 | h7f %in% 3:4,
    falta_dato = is.na(h7a) | is.na(h7b) | is.na(h7c) |
      is.na(h7d) | is.na(h7e) | is.na(h7f),
    lim_func = case_when(
      mucha_dif  ~ 1,          # tiene limitación
      falta_dato ~ NA_real_,   # faltan datos y no hay limitación en los demás
      TRUE       ~ 0))         # no tiene limitación

table(casen$lim_func, useNA = "ifany")

# 4c: versiones con etiquetas (Sí/No) para la Tabla 1
casen <- casen %>%
  mutate(prob_reciente_f = factor(prob_reciente, levels = c(1, 0), labels = c("Sí", "No")),
         lim_func_f      = factor(lim_func,      levels = c(1, 0), labels = c("Sí", "No")))


# PASO 5: UTILIZACIÓN DE SERVICIOS (VARIABLES DEPENDIENTES)
casen <- casen %>%
  mutate(s20_num = ifelse(as.numeric(s20a_preg) < 0, NA, as.numeric(s20a_preg)),
         s23_num = ifelse(as.numeric(s23a_preg) < 0, NA, as.numeric(s23a_preg)),
         s27_num = ifelse(as.numeric(s27a)      < 0, NA, as.numeric(s27a)))

casen <- casen %>%
  mutate(
    util_mg   = case_when(s20_num == 1 ~ 1, s20_num == 2 ~ 0),   # medicina general (3 meses)
    util_esp  = case_when(s23_num == 1 ~ 1, s23_num == 2 ~ 0),   # especialidad (3 meses)
    util_hosp = case_when(                                       # hospitalización (12 meses)
      s27_num %in% c(1, 3, 4) ~ 1,     # enfermedad, accidente u otra razón
      s27_num == 5            ~ 0),    # no se hospitalizó
    # el código 2 (embarazo, parto o cesárea) queda NA
    hosp_parto = ifelse(s27_num == 2, 1, 0))

table(casen$s20_num, casen$util_mg,   useNA = "ifany")
table(casen$s23_num, casen$util_esp,  useNA = "ifany")
table(casen$s27_num, casen$util_hosp, useNA = "ifany")


# PASO 6: REVISAR CASOS Y PERDIDOS EN ADULTOS
adultos <- casen %>% filter(edad >= 18)   # solo para contar casos sin ponderar

nrow(adultos)                                 
sum(adultos$hosp_parto == 1, na.rm = TRUE)    # hospitalizaciones por parto excluidas
sum(adultos$s13_num %in% 3:5)                 # personas con otra previsión (quedan fuera)

# % de perdidos por variable 
vars_perdidos <- c("educ", "prevision", "quintil", "area_f", "cerca_cs",
                   "prob_reciente", "lim_func",
                   "util_mg", "util_esp", "util_hosp")
round(100 * colMeans(is.na(adultos[, vars_perdidos])), 1)

# N de la muestra analítica para cada servicio
vars_modelo <- c("grupo_edad", "sexo", "educ", "prob_reciente", "lim_func",
                 "prevision", "quintil", "area_f", "cerca_cs", "region_f")

for (y in c("util_mg", "util_esp", "util_hosp")) {
  cat(y, ":", sum(complete.cases(adultos[, c(vars_modelo, y)])), "\n")
}

# Casos por grupo de edad y previsión, sin ponderar (solo para revisar)
table(adultos$grupo_edad, adultos$prevision)


# PASO 7: GUARDAR LA BASE PROCESADA
saveRDS(casen, "input/data/casen_procesada.rds")


# PASO 8: DECLARAR EL DISEÑO MUESTRAL
# Se declara con la base completa y LUEGO se filtra a adultos.Así la varianza se calcula correctamente (estimación por subpoblación).
dis <- casen %>%
  as_survey_design(ids = varunit, strata = varstrat, weights = expr, nest = TRUE)

dis_adultos <- dis %>% filter(edad >= 18)

# Comprobar los quintiles: % de cada quintil en la población total (debería ser ~20% cada uno)
dis %>%
  filter(!is.na(quintil)) %>%
  group_by(quintil) %>%
  summarise(pct = survey_prop(vartype = NULL) * 100)


# PASO 9: FUNCIONES PARA LAS TABLAS
# 9.1 Un decimal con coma
fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

# 9.2 Pasa a porcentaje (con formato) todas las columnas numéricas
a_porcentaje <- function(tabla) {
  tabla %>% mutate(across(where(is.numeric), ~ fmt1(100 * .x)))
}

# 9.3 Distribución de una variable con categorías (% ponderado)
tabla_cat <- function(diseno, variable, titulo) {
  diseno %>%
    filter(!is.na(.data[[variable]])) %>%
    mutate(categoria = .data[[variable]]) %>%
    group_by(categoria) %>%
    summarise(pct = survey_prop(vartype = NULL)) %>%
    ungroup() %>%
    mutate(Variable  = c(titulo, rep("", n() - 1)),
           Categoría = as.character(categoria),
           `%`       = fmt1(100 * pct)) %>%
    select(Variable, Categoría, `%`)
}

# 9.4 Necesidad y utilización: % de cada indicador (se usa con group_by o sin él)
resumen <- function(d) {
  d %>%
    summarise(`Problema reciente`    = survey_mean(prob_reciente, na.rm = TRUE, vartype = NULL),
              `Limitación funcional` = survey_mean(lim_func,      na.rm = TRUE, vartype = NULL),
              `Medicina general`     = survey_mean(util_mg,       na.rm = TRUE, vartype = NULL),
              `Especialidad`         = survey_mean(util_esp,      na.rm = TRUE, vartype = NULL),
              `Hospitalización`      = survey_mean(util_hosp,     na.rm = TRUE, vartype = NULL))
}

# 9.5 Utilización de los tres servicios según las categorías de un factor
util_por <- function(diseno, variable, titulo) {
  diseno %>%
    filter(!is.na(.data[[variable]])) %>%
    mutate(categoria = .data[[variable]]) %>%
    group_by(categoria) %>%
    resumen() %>%
    ungroup() %>%
    a_porcentaje() %>%
    mutate(Variable  = c(titulo, rep("", n() - 1)),
           Categoría = as.character(categoria)) %>%
    select(Variable, Categoría, `Medicina general`, `Especialidad`, `Hospitalización`)
}

# 9.6 Partes de las notas que se repiten
n_adultos <- formatC(nrow(adultos), format = "d", big.mark = ".", decimal.mark = ",")
nota_fuente <- paste("Fuente: elaboración propia con datos de CASEN 2024.",
                     "Porcentajes ponderados con el factor de expansión.",
                     "Se excluyen los casos sin información.")


# PASO 10: TABLA 1. CARACTERIZACIÓN DE LA POBLACIÓN DE 18 AÑOS Y MÁS
t1 <- bind_rows(
  tabla_cat(dis_adultos, "grupo_edad",      "Grupo de edad"),
  tabla_cat(dis_adultos, "sexo",            "Sexo"),
  tabla_cat(dis_adultos, "educ",            "Nivel educacional"),
  tabla_cat(dis_adultos, "prob_reciente_f", "Problema de salud reciente"),
  tabla_cat(dis_adultos, "lim_func_f",      "Limitación funcional"),
  tabla_cat(dis_adultos, "prevision",       "Previsión de salud"),
  tabla_cat(dis_adultos, "quintil",         "Quintil de ingreso"),
  tabla_cat(dis_adultos, "area_f",          "Área de residencia"),
  tabla_cat(dis_adultos, "cerca_cs",        "Centro de salud a menos de 20 cuadras o 2,5 km"))

nota1 <- paste0("Población de 18 años y más (n = ", n_adultos, "). ",
                "Los quintiles de ingreso total per cápita del hogar se calcularon sobre la población total, ",
                "por lo que su distribución en la población adulta no es de 20% cada uno. ", nota_fuente)
t1


# PASO 11: TABLA 2. NECESIDAD Y UTILIZACIÓN SEGÚN GRUPO DE EDAD
t2_edad <- dis_adultos %>%
  group_by(grupo_edad) %>%
  resumen() %>%
  ungroup() %>%
  mutate(grupo_edad = as.character(grupo_edad))

t2_total <- dis_adultos %>%
  resumen() %>%
  mutate(grupo_edad = "Total 18 años y más")

t2 <- bind_rows(t2_edad, t2_total) %>%
  a_porcentaje() %>%
  rename(`Grupo de edad` = grupo_edad)

nota2 <- paste("Población de 18 años y más. Problema reciente, medicina general y especialidad: últimos 3 meses.",
               "Hospitalización o intervención quirúrgica: últimos 12 meses; se excluyen las",
               "hospitalizaciones por embarazo, parto o cesárea. Limitación funcional: mucha dificultad",
               "o no puede en al menos uno de seis dominios. Como los",
               "períodos de referencia difieren, se comparan patrones y no niveles entre servicios.",
               nota_fuente)
t2


# PASO 12: TABLA 3. UTILIZACIÓN SEGÚN INGRESO, PREVISIÓN Y TERRITORIO
t3 <- bind_rows(
  util_por(dis_adultos, "quintil",    "Quintil de ingreso"),
  util_por(dis_adultos, "prevision",  "Previsión de salud"),
  util_por(dis_adultos, "area_f",     "Área de residencia"),
  util_por(dis_adultos, "cerca_cs",   "Centro de salud cercano"))

nota3 <- paste("Población de 18 años y más. Porcentaje de personas que utilizó cada servicio",
               "dentro de cada categoría. Medicina general y especialidad: últimos 3 meses.",
               "Hospitalización o intervención quirúrgica: últimos 12 meses, sin embarazo, parto o cesárea.",
               nota_fuente)
t3


# PASO 13: MOSTRAR LAS TABLAS CON KABLEEXTRA
tabla_kbl <- function(tabla, titulo, nota) {
  kbl(tabla, booktabs = TRUE, linesep = "", caption = titulo) %>%
    kable_styling(full_width = FALSE, font_size = 10,
                  latex_options = c("hold_position", "scale_down")) %>%
    row_spec(0, bold = TRUE) %>%
    footnote(general = nota, general_title = "Nota.",
             threeparttable = TRUE, footnote_as_chunk = TRUE)
}

tabla_kbl(t1, "Tabla 1. Caracterización de la población de 18 años y más (%)", nota1)
tabla_kbl(t2, "Tabla 2. Necesidad de salud y utilización de servicios según grupo de edad (%)", nota2)
tabla_kbl(t3, "Tabla 3. Utilización de servicios según ingreso, previsión y territorio (%)", nota3)


# PASO 14: GUARDAR TABLAS Y NOTAS
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

tablas <- list(t1 = t1, t2 = t2, t3 = t3,
               nota1 = nota1, nota2 = nota2, nota3 = nota3)

saveRDS(tablas, "output/tables/tablas_descriptivas.rds")


# PASO 15 : GUARDAR LAS TABLAS COMO IMAGEN PARA MOSTRARLAS
if (requireNamespace("webshot2", quietly = TRUE)) {
  library(webshot2)

save_kable(tabla_kbl(t1, "Tabla 1. Caracterización de la población de 18 años y más (%)", nota1),
           "output/tables/tabla1.png")

save_kable(tabla_kbl(t2, "Tabla 2. Necesidad de salud y utilización de servicios según grupo de edad (%)", nota2),
           "output/tables/tabla2.png")

save_kable(tabla_kbl(t3, "Tabla 3. Utilización de servicios según ingreso, previsión y territorio (%)", nota3),
           "output/tables/tabla3.png")
}