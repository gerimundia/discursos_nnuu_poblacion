rm(list=ls())
gc()

sesiones <- dir("UNGDC_1946-2024/TXT")

folder <- as.character(sesiones[1])
files <- list.files(path = paste0("UNGDC_1946-2024/TXT/",folder), pattern = "\\.txt$", full.names = TRUE)

discursos <- data.frame(
  archivo = basename(files), 
  texto = sapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),
  stringsAsFactors = FALSE)

for(sesion in sesiones[2:length(sesiones)]){
  folder <- as.character(sesion)
  
  files <- list.files(path = paste0("UNGDC_1946-2024/TXT/",folder), pattern = "\\.txt$", full.names = TRUE)
  
  discursos0 <- data.frame(
    archivo = basename(files), 
    texto = sapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),
    stringsAsFactors = FALSE)
  
  message(paste("------------ Folder:", folder, "------------ "))
  message(paste("------------ No. de discursos:", nrow(discursos0), "------------ "))
  Sys.sleep(1)
  
  discursos <- rbind(discursos, discursos0)
}

rownames(discursos) <- c(1:nrow(discursos))
discursos$anio <- as.integer(substr(discursos$archivo, 8, 11))

discursos <- discursos %>% 
  filter(anio>1000)

discursos$pais <- substr(discursos$archivo, 1, 3)
names(discursos)
discursos <- discursos[, c("anio", "pais", "texto", "archivo")]

str(discursos)
summary(discursos$anio)

save(discursos, file="discursos_nnuu.RData")
