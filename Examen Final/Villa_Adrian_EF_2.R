
Problema2 <- function (secuencias) { 
  library (Biostrings)
  
  # Parte 1
  
  vibrioph <- readBStringSet ("NC_023863.fna") ## Secuencia de Vibrio Phage
  length1 <- nchar(vibrioph)
  length1 ## Tamaño de la secuencia
  
  flaviv <- readBStringSet ("NC_026620.fna") ## Secuencia de Flavivirus
  length2 <- nchar (flaviv)
  length2 ## Tamaño de la secuencia
  
  if (length1 == length2) {print ("Son iguales ambas secuencias")
  }else if (length1 > length2) {print(paste("El virus vibrio phage", length1, "es mayor que flavivirus", length2))
  }else {print(paste("El virus flavivirus", length2, "es mayor que vibrio phage", length1))
  } ## Dependiendo de cuál secuencia sea la más larga (en este caso la de Vibrio phage), imprimirá el mensaje pertinente
  
  # Parte 2
  
  vibrioph1 <- as.character(vibrioph) ## Para ver el % de guaninas y citocinas, toma el 
  vibrioph1 <- strsplit(vibrioph, " ")
  vibrioph1 <- unlist(vibrioph)
  a <-table(vibrioph) ## Con esta se obtiene el porcentaje de todos los aminoacidos
  citosina1 <- a[2] / length1 *100 ## Toma el total de las citocinas y lo divide entre 100 para obtener el %, lo mismo con las guaninas
  guanina1 <- a[3] / length1 *100
  print(paste("Vibrio Phage: % citocinas:", citosina1, "% guanina:", guanina1))
  print(paste("Total GC% :", sum(citosina1, guanina1))) ## Suma el % de ambas
  
  flaviv1 <- as.character(flaviv) ## Lo mismo que con el caso anterior
  flaviv1 <- strsplit(flaviv, " ")
  flaviv1 <- unlist(flaviv)
  b <-table(flaviv)
  citosina2 <- b[2] / length2 *100
  guanina2 <- b[3] / length2 *100
  print(paste("Flavivirus: % de citosina:", citosina2, "% de guanina:", guanina2))
  print(paste("Total GC% :", sum(citosina2, guanina2)))
  
  # Parte 3
  
  vphague <-substr(vibrioph, start = 0, stop = length2) ## Extraer los elementos del vector de inicio a fin tomando en cuenta la longitud del más corto
  flav <-substr(flaviv, start = 0, stop = length2)
  
  hamming <- adist(vphague, flav) ## Mide las diferencias de las secuencias
  print(paste("Distancia de Hamming:", hamming))
  
}

