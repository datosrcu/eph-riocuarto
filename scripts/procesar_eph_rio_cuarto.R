# ============================================================
# Procesamiento EPH - Gran Río Cuarto + comparación de aglomerados
# ============================================================
# Qué hace este script:
#   1. Descarga la base completa del país (individual y hogar) de
#      un trimestre con eph::get_microdata() — SIN filtrar por
#      aglomerado todavía. Esto es clave para poder comparar: si
#      filtráramos a Río Cuarto en la descarga, no tendríamos con
#      qué comparar sin volver a bajar todo de nuevo.
#   2. Guarda backup del crudo completo en Dropbox.
#   3. Arma la lista de "entidades" a comparar: los 31 aglomerados
#      que trae la base (tomados automáticamente, no a mano) más el
#      total país, y una función que calcula los mismos indicadores
#      para cualquiera de ellas, con la misma fórmula.
#   4. Arma una tabla larga: una fila por (trimestre, entidad),
#      lista para que el HTML elija qué entidades graficar juntas
#      (la lógica de "comparación de aglomerados" que viste en
#      Infodash).
#
# IMPORTANTE antes de correrlo:
#   - Instalá el paquete eph una sola vez:
#       install.packages("remotes")
#       remotes::install_github("ropensci/eph")
#   - Revisá con ?eph::get_microdata y ?eph::organize_labels que
#     los nombres de parámetros coincidan con tu versión del
#     paquete.
#   - Las fórmulas de mercado de trabajo siguen la metodología de
#     INDEC (denominador = población total para actividad/empleo,
#     PEA para desocupación). Comparalas contra las tasas
#     nacionales oficiales del mismo trimestre antes de confiar en
#     el resultado.
#   - "Total pais" acá NO es la cifra oficial que publica INDEC:
#     es el mismo cálculo que para cada aglomerado, aplicado sin
#     filtrar. Se hace así a propósito, para que la comparación
#     entre Río Cuarto y el total sea metodológicamente consistente
#     (la misma fórmula de los dos lados). Si en algún gráfico
#     puntual del tablero preferís mostrar la cifra oficial de
#     INDEC en vez de esta, se puede cargar aparte.
# ============================================================

library(eph)
library(dplyr)
library(readr)
library(jsonlite)
library(purrr)

# ------------------------------------------------------------
# 0. PARÁMETROS
# ------------------------------------------------------------
anio       <- 2026
trimestre  <- 1

carpeta_crudo <- sprintf("C:/Users/Arg/Dropbox/EPH/crudo/%s_T%s", anio, trimestre)
dir.create(carpeta_crudo, showWarnings = FALSE, recursive = TRUE)

carpeta_repo_data <- "C:/Users/Arg/Desktop/Subse_Estadistica/tablero-eph-rio-cuarto/data"
dir.create(carpeta_repo_data, showWarnings = FALSE, recursive = TRUE)

ruta_csv  <- file.path(carpeta_repo_data, "indicadores_comparacion.csv")
ruta_json <- file.path(carpeta_repo_data, "indicadores_comparacion.json")

# ------------------------------------------------------------
# 1. DESCARGAR LA BASE COMPLETA DEL PAÍS (sin filtrar aglomerado)
# ------------------------------------------------------------
base_individual_cruda <- eph::get_microdata(
  year      = anio,
  trimester = trimestre,
  type      = "individual"
)

base_hogar_cruda <- eph::get_microdata(
  year      = anio,
  trimester = trimestre,
  type      = "hogar"
)

# Backup del crudo completo (todos los aglomerados) en Dropbox
write_csv(base_individual_cruda,
          file.path(carpeta_crudo, sprintf("individual_%s_T%s.csv", anio, trimestre)))
write_csv(base_hogar_cruda,
          file.path(carpeta_crudo, sprintf("hogar_%s_T%s.csv", anio, trimestre)))

# Etiquetado según diseño de registro del período
base_individual <- eph::organize_labels(base_individual_cruda, type = "individual")
base_hogar      <- eph::organize_labels(base_hogar_cruda, type = "hogar")

# organize_labels() deja las variables categóricas como clase "labelled"
# (internamente sigue siendo el número de siempre, con una etiqueta de
# texto pegada encima). Comparar esa columna directamente contra un
# string escrito a mano (ESTADO == "Ocupado") NO funciona con esa
# clase — hay que convertirla a texto plano primero. Lo hacemos acá
# con ESTADO porque es la que usamos más abajo; si en el futuro
# comparás otra columna (CH04, NIVEL_ED, etc.) contra texto escrito a
# mano, va a necesitar esta misma conversión.
base_individual <- base_individual %>%
  mutate(ESTADO = as.character(ESTADO))

# ------------------------------------------------------------
# 2. LISTA DE ENTIDADES: los 31 aglomerados que trae la base + total país
# ------------------------------------------------------------
# En vez de escribir una lista fija a mano, tomamos todos los códigos
# de aglomerado que aparecen en la base de este trimestre (deberían
# ser los 31 aglomerados urbanos que releva la EPH). Así, si mañana
# querés comparar con cualquier otro aglomerado además de Río Cuarto
# y Gran Córdoba, ya está calculado — no hay que tocar el código.
codigos_aglomerado <- sort(unique(base_individual$AGLOMERADO))

entidades <- as.list(codigos_aglomerado)
names(entidades) <- as.character(codigos_aglomerado)  # por ahora, nombre = código

# Los dos que más nos interesan, con nombre legible en vez de código.
# Para ponerle nombre a los otros 29 también, revisá la estructura de
# eph::diccionario_aglomerados (con `View(eph::diccionario_aglomerados)`
# o `names(eph::diccionario_aglomerados)`) y sumá un join acá — no lo
# escribo a ciegas porque no estoy seguro de los nombres exactos de
# columna en tu versión del paquete.
names(entidades)[codigos_aglomerado == 36] <- "Rio Cuarto"
names(entidades)[codigos_aglomerado == 13] <- "Gran Cordoba"

# "Total pais": mismo cálculo que cada aglomerado, pero sin filtrar
# (no es la cifra oficial de INDEC — ver nota metodológica arriba).
entidades[["Total pais"]] <- NA

# ------------------------------------------------------------
# 3. FILTRAR POR AGLOMERADO (o no filtrar, para el total país)
# ------------------------------------------------------------
filtrar_aglomerado <- function(base, aglomerado) {
  if (is.na(aglomerado)) return(base)   # sin filtro = total país
  base %>% filter(AGLOMERADO == aglomerado)
}

# ------------------------------------------------------------
# 4. CALCULAR INDICADORES DE MERCADO DE TRABAJO
#    (misma fórmula para cualquier entidad: un aglomerado o el total)
# ------------------------------------------------------------
# Códigos ORIGINALES de ESTADO en la EPH (así viene la base cruda,
# antes de organize_labels()):
#   0 = entrevista individual no realizada
#   1 = ocupado
#   2 = desocupado
#   3 = inactivo
#   4 = menor de 10 años (fuera de la población en edad de trabajar)
#
# PERO organize_labels() convierte esta columna a texto (confirmado
# corriéndolo: queda "Ocupado", "Desocupado", "Inactivo", "Menor de
# 10 anios.", "Entrevista individual no realizada..."). Por eso acá
# abajo comparamos contra el texto, no contra los números. Si en tu
# base el texto viniera con alguna tilde/mayúscula distinta, ajustá
# estos strings para que coincidan exacto con lo que te dio
# `table(base_individual$ESTADO)`.

calcular_indicadores_trabajo <- function(base_ind_filtrada) {
  base_ind_filtrada %>%
    summarise(
      poblacion_total = sum(PONDERA),
      pea             = sum(PONDERA[ESTADO %in% c("Ocupado", "Desocupado")]),
      ocupados        = sum(PONDERA[ESTADO == "Ocupado"]),
      desocupados     = sum(PONDERA[ESTADO == "Desocupado"]),

      tasa_actividad      = round(pea / poblacion_total * 100, 1),
      tasa_empleo          = round(ocupados / poblacion_total * 100, 1),
      tasa_desocupacion    = round(desocupados / pea * 100, 1)

      # ------------------------------------------------------------
      # Acá se suman más indicadores de esta misma página (subocupación,
      # informalidad) cuando los definamos. La función ya queda lista
      # para crecer sin tocar el resto del script.
      # ------------------------------------------------------------
    )
}

# ------------------------------------------------------------
# 5. CALCULAR PARA CADA ENTIDAD Y APILAR EN UNA TABLA LARGA
# ------------------------------------------------------------
# Resultado: una fila por (trimestre, entidad). Este es el formato
# que necesita el HTML para el gráfico de "comparación de
# aglomerados": elegís una columna (ej. tasa_desocupacion) y
# graficás una línea por cada valor distinto de `entidad`.

resultados <- map_dfr(names(entidades), function(nombre_entidad) {
  codigo_aglomerado <- entidades[[nombre_entidad]]

  ind_filtrada <- filtrar_aglomerado(base_individual, codigo_aglomerado)

  if (nrow(ind_filtrada) == 0) {
    stop(sprintf("No se encontraron registros para '%s' (AGLOMERADO == %s). ",
                  nombre_entidad, codigo_aglomerado),
         "Revisá el código de aglomerado o el trimestre pedido.")
  }

  calcular_indicadores_trabajo(ind_filtrada) %>%
    mutate(
      anio      = anio,
      trimestre = trimestre,
      entidad   = nombre_entidad,
      .before   = 1
    )
})

# ------------------------------------------------------------
# 6. ACTUALIZAR LA TABLA HISTÓRICA (upsert por año + trimestre + entidad)
# ------------------------------------------------------------
if (file.exists(ruta_csv)) {
  historico <- read_csv(ruta_csv, show_col_types = FALSE)
  historico <- historico %>%
    filter(!(anio == !!anio & trimestre == !!trimestre & entidad %in% names(entidades))) %>%
    bind_rows(resultados) %>%
    arrange(entidad, anio, trimestre)
} else {
  historico <- resultados
}

write_csv(historico, ruta_csv)
write_json(historico, ruta_json, pretty = TRUE, auto_unbox = TRUE)

cat(sprintf(
  "Listo. Backup crudo (todos los aglomerados) guardado en:\n  %s\n",
  carpeta_crudo
))
cat(sprintf(
  "Tabla de comparación actualizada con %s filas (una por entidad) para %s-T%s en:\n  %s\n  %s\n",
  nrow(resultados), anio, trimestre, ruta_csv, ruta_json
))
cat("Ahora falta: revisar el resultado, y hacer commit + push en el repo de GitHub.\n")
