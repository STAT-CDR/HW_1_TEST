library(testthat)
library(jsonlite)
library(stringr)

# Cargar tests desde JSON
if (!file.exists("config/tests.json")) {
  stop("No se encontró el archivo tests.json")
}

tests <- fromJSON("config/tests.json")

qmd_estudiante <- "HW1.qmd"
if (!file.exists(qmd_estudiante)) {
  stop("No se encontró el archivo QMD del estudiante")
}

leer_codigo_qmd_con_label <- function(archivo) {
    lineas <- readLines(archivo)
    codigo <- list()
    dentro_bloque <- FALSE
    bloque_actual <- c()
    nombre_actual <- NULL

    for (linea in lineas) {
        if (grepl("^```\\{.*\\}$", linea)) { # Inicio de un bloque de código
            dentro_bloque <- TRUE
            bloque_actual <- c()
            nombre_actual <- NULL # Reinicia el nombre
        } else if (linea == "```" && dentro_bloque) { # Cierre del bloque
            dentro_bloque <- FALSE
            if (!is.null(nombre_actual)) {
                codigo[[nombre_actual]] <- bloque_actual # Usa el nombre del label
            } else {
                codigo <- append(codigo, list(bloque_actual)) # Si no hay nombre, añade sin nombre
            }
        } else if (dentro_bloque) {
            if (grepl("^#\\| label:", linea)) { # Busca la etiqueta del bloque
                nombre_actual <- trimws(sub("^#\\| label:\\s*", "", linea))
            } else {
                bloque_actual <- c(bloque_actual, linea) # Agrega líneas al bloque
            }
        }
    }

    return(codigo)
}

# Leer código del estudiante
codigo_por_ejercicio <- leer_codigo_qmd_con_label(qmd_estudiante)

# Crear carpeta para las correcciones

# Ejecutar tests y generar reportes
resultados <- data.frame()

qmd_output <- paste0("corrected_", qmd_estudiante)

# Leer el contenido original del QMD del estudiante
contenido_original <- readLines(qmd_estudiante)
contenido_qmd <- c()

dentro_bloque <- FALSE
nombre_actual <- NULL

for (linea in contenido_original) {
    if (grepl("^```\\{[a-zA-Z]+.*\\}$", linea)) { # Inicio de un chunk
        dentro_bloque <- TRUE
        nombre_actual <- NULL # Reiniciar nombre para cada chunk
        contenido_qmd <- c(contenido_qmd, linea)
    } else if (linea == "```" && dentro_bloque) { # Cierre del chunk
        dentro_bloque <- FALSE
        print(nombre_actual)

        if (!is.null(nombre_actual) && nombre_actual %in% names(tests)) {
            codigo_test <- tests[[nombre_actual]]$test
            mensaje <- tests[[nombre_actual]]$mensaje
            puntos <- tests[[nombre_actual]]$puntos

            # Evaluar código del estudiante antes del test
            eval_result <- tryCatch(
                {
                    eval(parse(text = paste(codigo_por_ejercicio[[nombre_actual]], collapse = "\n")))
                    TRUE
                },
                error = function(e) {
                    FALSE
                }
            )

            if (eval_result) {
                test_output <- tryCatch(
                    {
                        test_output <- capture_output({
                            eval(parse(text = codigo_test))
                        })
                    },
                    error = function(e) {
                        test_output <- "FAILED"
                    }
                )

                fallo <- any(grepl("FAILED", test_output))
                resultado_texto <- if (fallo) {
                    "❌ **Hay errores en el test.**"
                } else {
                    "✅ **Correcto!**"
                }
            } else {
                test_output <- "Error en la ejecución del código del estudiante. No se pudieron aplicar los tests."
                resultado_texto <- "❌ **Error en la ejecución del código.**"
                fallo <- TRUE
            }
            
            resultados <- rbind(resultados, data.frame(
                "nombre" = nombre_actual,
                "fallo" = fallo,
                "mensaje" = mensaje,
                "puntos" = ifelse(!fallo, puntos, 0)
            ))
            

            contenido_qmd <- c(
                contenido_qmd,
                "```",
                "\n",
                "**Test de corrección**",
                "```{r}",
                if (fallo) "#| error: true",
                codigo_test,
                "```",
                "\n",
                "**Resultado**",
                resultado_texto,
                "",
                paste("**Mensaje:**", mensaje),
                paste("**Puntos:**", ifelse(!fallo, puntos, 0)),
                ""
            )
        } else {
            contenido_qmd <- c(contenido_qmd, "```")
        }

    } else if (dentro_bloque) {
        if (grepl("^#\\| label:", linea)) { # Capturar la etiqueta
            nombre_actual <- trimws(sub("^#\\| label:\\s*", "", linea))
        }
        contenido_qmd <- c(contenido_qmd, linea)
    } else {
        contenido_qmd <- c(contenido_qmd, linea)
    }
}



writeLines(contenido_qmd, qmd_output)

quarto::quarto_render(qmd_output, "html")

knitr::purl(qmd_output)

testthat::test_file(
    gsub(".qmd",".R", qmd_output)
)

if (file.exists("results.csv")) {
  file.remove("results.csv")
}

write.csv(resultados, "results.csv", row.names = FALSE) 

file.remove(gsub(".qmd",".R", qmd_output))

