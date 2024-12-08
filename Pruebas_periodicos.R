# Librerias utilizadas
library(rvest)
library(foreach)
library(doParallel)
library(data.table)
library(lubridate)
library(ggplot2)

# palabras claves
arrayKeyword <- c("planeros")

# Array de sitios base
sites <- c(
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.lanacion.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.clarin.com&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.infobae.com&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.tiempoar.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.pagina12.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.sitioandino.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.losandes.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.mdzol.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.diariomendoza.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fciudadano.news&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.voxpopuli.net.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.elsol.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419",
  "https://news.google.com/search?q=KEYWORD%20site%3Ahttps%3A%2F%2Fwww.diariouno.com.ar&hl=es-419&gl=AR&ceid=AR%3Aes-419"
)

# Registrar un cluster paralelo para foreach
registerDoParallel(cores = 2) # Ajusta el número de núcleos según tu sistema

# Función para extraer datos de una página
extraer_datos <- function(url) {
  pagina_web <- read_html(url)
  titulos_html <- html_elements(pagina_web, "a.JtKRv") # Ajuste el selector según la estructura de la página
  titulos_texto <- html_text(titulos_html)
  fechas_html <- html_elements(pagina_web, "time") # Ajuste el selector según la estructura de la página
  fechas_texto <- html_text(fechas_html)
  
  if (length(fechas_texto) == 0) {
    fechas_texto <- rep("Sin fecha", length(titulos_texto)) # Rellenar con "Sin fecha" si no hay fechas
  }
  
  links <- html_attr(titulos_html, "href")
  
  # Formato url de la fuente
  site_url <- sub("^(.*site%3Ahttps%3A%2F%2F)(www\\.[^&]+)&.*$", "\\2", url)
  links <- ifelse(grepl("^http", links), links, gsub("/\\.\\./", "/", paste0("https://news.google.com", links)))
  
  resultados <- list()
  for (i in seq_along(titulos_texto)) {
    noticia <- list(
      fuente = site_url,
      fecha = ifelse(i <= length(fechas_texto), fechas_texto[i], "Sin fecha"),
      titulo = titulos_texto[i],
      link = links[i]
    )
    resultados[[i]] <- noticia
  }
  return(resultados)
}

# Crear todas las combinaciones de URLs basadas en keywords y sitios
urls <- unlist(lapply(arrayKeyword, function(keyword) {
  sapply(sites, function(site) {
    gsub("KEYWORD", keyword, site)
  })
}))

# Ejecutar el script en cada URL y almacenar los resultados
resultados_totales <- foreach(url = urls, .combine = c, .packages = c("rvest")) %dopar% {
  extraer_datos(url)
}

# Detener el cluster paralelo
stopImplicitCluster()

# Convertir resultados_totales en un data frame
resultados_df <- rbindlist(resultados_totales)

# Crear un vector de meses en español y su correspondiente en inglés
meses_esp <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
meses_eng <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Función para convertir las fechas del formato "24 ene 2018", "12 feb" o "Hace 5 días" al formato inglés
convertir_fecha <- function(fecha) {
  # Manejar fechas relativas como "Hace 5 días"
  if (grepl("Hace", fecha)) {
    dias_atras <- as.numeric(sub("Hace (\\d+) días", "\\1", fecha))
    fecha <- Sys.Date() - days(dias_atras)
    return(format(fecha, "%d %b %Y"))
  }
  
  # Añadir año 2024 o 2023 si falta
  if (!grepl("\\d{4}$", fecha)) {
    if (grepl("dic", fecha)) {
      fecha <- paste(fecha, "2023")
    } else {
      fecha <- paste(fecha, "2024")
    }
  }
  
  # Reemplazar meses en español por inglés
  for (i in seq_along(meses_esp)) {
    fecha <- gsub(meses_esp[i], meses_eng[i], fecha)
  }
  
  return(fecha)
}

# Aplicar la conversión a las fechas
resultados_df$fecha <- sapply(resultados_df$fecha, convertir_fecha)

# Convertir las fechas a formato date usando lubridate
resultados_df$fecha_n <- dmy(resultados_df$fecha)

print(resultados_df)

# Especifica la ruta y el nombre del archivo CSV
nombre_archivo <- "resultados.csv"

# Exporta el dataframe a un archivo CSV
write.csv(resultados_df, file = nombre_archivo, row.names = FALSE)

# Mensaje de confirmación
cat("Los datos se han exportado correctamente a", nombre_archivo, "\n")

# Agregar los gráficos

# Extraer el año de la fecha
resultados_df$año <- year(resultados_df$fecha_n)

# Contar la cantidad de resultados por año
conteo_por_año <- resultados_df[, .N, by = .(año)]

# Gráfico lineal
grafico_lineal <- ggplot(conteo_por_año, aes(x = año, y = N)) +
  geom_line() +
  geom_point() +
  labs(title = "Cantidad de resultados año a año",
       x = "Año",
       y = "Cantidad de resultados") +
  theme_minimal()

# Guardar el gráfico lineal como imagen
ggsave("grafico_lineal_resultados_por_año.png", plot = grafico_lineal)

# Mostrar el gráfico lineal en la pantalla
print(grafico_lineal)

# Gráfico de barras
grafico_barras <- ggplot(conteo_por_año, aes(x = factor(año), y = N)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de resultados por año",
       x = "Año",
       y = "Cantidad de resultados") +
  theme_minimal()

# Guardar el gráfico de barras como imagen
ggsave("grafico_barras_resultados_por_año.png", plot = grafico_barras)

# Mostrar el gráfico de barras en la pantalla
print(grafico_barras)
