
Problema3 <- function (Secuencia) { 
  respuesta <- 1
  
  while(respuesta == 1){
    respuesta <- readline(prompt = "Ingresa una secuencia de 3 nucleótidos: ")
    aminoacid <- 0
    uno <- 0
    dos <- 0
    tres <- 0
    
    # Tabla de aminoácidos
    
    if(respuesta == "TTT" | respuesta == "TTC"){aminoacid <- "Fenilalanina"
    }else if(respuesta == "TTA" | respuesta == "TTG" | respuesta == "CTT" | respuesta == "CTA" | respuesta == "CTG"){aminoacid <- "Leucina"
    }else if(respuesta == "ATT" | respuesta == "ATC" | respuesta == "ATA"){aminoacid <- "Isoleucina"
    }else if(respuesta == "ATG"){aminoacid <- "Metionina"
    }else if(respuesta == "GTT" | respuesta == "GTC" | respuesta == "GTA" | respuesta == "GTG"){aminoacid <- "Valina"
    }else if(respuesta == "TCT" | respuesta == "TCC" | respuesta == "TCA" | respuesta == "TCG" | respuesta == "ACT" | respuesta == "AGC"){aminoacid <- "Serina"
    }else if(respuesta == "CCT" | respuesta == "CCC" | respuesta == "CCA" | respuesta == "CCG"){aminoacid <- "Prolina"
    }else if(respuesta == "ACT" | respuesta == "ACC" | respuesta == "ACA" | respuesta == "ACG"){aminoacid <- "Treonina"
    }else if(respuesta == "GCT" | respuesta == "GCC" | respuesta == "GCA" | respuesta == "GCG"){aminoacid <- "Alanina"
    }else if(respuesta == "TAT" | respuesta == "TAC"){aminoacid <- "Tirosina"
    }else if(respuesta == "TAA" | respuesta == "TAG" | respuesta == "TGA"){aminoacid <- "Codón de paro"
    }else if(respuesta == "CAT" | respuesta == "CAC"){aminoacid <- "Histidina"
    }else if(respuesta == "CAA" | respuesta == "CAG"){aminoacid <- "Glutamina"
    }else if(respuesta == "AAT" | respuesta == "AAC"){aminoacid <- "Asparagina"
    }else if(respuesta == "AAA" | respuesta == "AAG"){aminoacid <- "Lisina"
    }else if(respuesta == "GAT" | respuesta == "GAC"){aminoacid <- "Aspartato"
    }else if(respuesta == "GAA" | respuesta == "GAG"){aminoacid <- "Glutamato"
    }else if(respuesta == "TGT" | respuesta == "TGC"){aminoacid <- "Cisteína"
    }else if(respuesta == "TGG"){aminoacid <- "Triptófano"
    }else if(respuesta == "CGT" | respuesta == "CGC" | respuesta == "CGA" | respuesta == "CGG" | respuesta == "AGA" | respuesta == "AGG"){aminoacid <- "Arginina"
    }else if(respuesta == "GGT" | respuesta == "GGC" | respuesta == "GGA" | respuesta == "GGG"){aminoacid <- "Glicina"
    }else(print("nada"))
    
    # Complementaria
    ## Toma en cuenta cada lugar del triplete ingresado "1" "2" y "3" para cada lugar en el orden de esta.
    
    if(substr(respuesta, start = 1, stop = 1) == "A"){uno <- "T"
    }else if(substr(respuesta, start = 1, stop = 1) == "T"){uno <- "A"
    }else if(substr(respuesta, start = 1, stop = 1) == "G"){uno <- "C"
    } else if(substr(respuesta, start = 1, stop = 1) == "C"){uno <- "G"}else(print("nada")) 
    
    if(substr(respuesta, start = 2, stop = 2) == "A"){dos <- "T"
    }else if(substr(respuesta, start = 2, stop = 2) == "T"){dos <- "A"
    }else if(substr(respuesta, start = 2, stop = 2) == "G"){dos <- "C"
    }else if(substr(respuesta, start = 2, stop = 2) == "C"){dos <- "G"}else(print("nada"))  
    
    if(substr(respuesta, start = 3, stop = 3) == "A"){tres <- "T"
    }else if(substr(respuesta, start = 3, stop = 3) == "T"){tres <- "A"
    }else if(substr(respuesta, start = 3, stop = 3) == "G"){tres <- "C"
    }else if(substr(respuesta, start = 3, stop = 3) == "C"){tres <- "G"}else(print("nada")) 
    print(paste("El Aminoácido:", aminoacid, "y", "complementaria:", paste0(uno, dos, tres)))
    respuesta <- readline(prompt = "¿Deseas probar con otra secuencia? (Sí=1, No=0)  ")
    respuesta <- as.numeric(respuesta)
  }
}

## Este programa, usa la tabla de aminoácidos de los tripletes, tiene asignado los nombres de cada aminoácido, dependiendo de qué se ingrese, arrojará el aminoácido correspondiente.
## Para la parte de la secuencia complementaria, toma cada posible combinación de nucleotidos y se encarga de imprimir el complementario de la secuencia originalmente ingresada.
## Permite hacerlo cuantas veces se quiera hasta que el usuario ingrese que no desea continuar probando.