
# Packages laden ----------------------------------------------------------

library(readxl)
library(dplyr)
library(imputeTS)


# Daten import ----------------------------------------------------------
  

  ## Dateipfad abfragen (Auswahl der durch 3DFin erstellten .xlsx-Datei der Untersuchungsgruppe)
  datei_path<- choose.files()
  
  ## Daten metrics
  data_metrics      <- read_xlsx(datei_path, sheet = 1,range = "A1:E300", col_names = TRUE)
  data_metrics[,-1] <- round(data_metrics[,-1],digits = 4)
  data_metrics      <- na.omit(data_metrics)
  
  
  ## Daten diameter
  data_diameters <- read_xlsx(datei_path, sheet = 2,range = "A1:FS300", col_names = TRUE)
  data_diameters[,-1] <- round(data_diameters[,-1],digits = 4)
  data_diameters <- na.omit(data_diameters)
  

  
  ## Daten overall_quality
  data_overall_quality <- read_xlsx(datei_path, sheet = 6,range = "A1:FS300", col_names = TRUE)
  data_overall_quality <- na.omit(data_overall_quality)




# Daten aufbereiten ----------------------------------------------------------
  
  ## Chek Vertrauenwürdigkeit-----
    
    ### Multiplikation diameters & overall_quality
    data_vertrauenswürdig <- data_diameters[,-1]*data_overall_quality[,-1]
      
    ### Hinzufügen der Baum_Nr
    # data_vertrauenswürdig<- cbind(data_metrics$Tree_Nr, data_vertrauenswürdig)
      
  ## Interpolation fehlender Werte-----
      
    ### 0 in NA umwandeln
    data_vertrauenswürdig <- replace(data_vertrauenswürdig, data_vertrauenswürdig == 0, NA)
    data_test <- data_vertrauenswürdig
      
    ### Interpolation Baum für Baum
    for (i in 1:nrow(data_vertrauenswürdig)) {
      data_row <- as.numeric(data_vertrauenswürdig[i,])
      data_row <- na_interpolation(data_row, option = "linear", rule =1)
      data_vertrauenswürdig[i,]<- data_row
    }
    
    ### Abholzigkeit berechnen (gesamter Stamm)
    
    function_abholzigkeit<- function(zeile) {
      zeile <- zeile[!is.na(zeile)] # NA entfernen
      if (length(zeile) < 2) { # Überprüfung min. 2 Werte
        return(NA)
      }
      differenzen <- diff(zeile) # Differenzen der Zeile berechnen
      differenzen <- abs(differenzen) # Absolutwerte Differenzen
      durchschnitt <- mean(differenzen) # durchscnittliche Abholzigkeit berechnen
    }
    
    abholzigkeit<- apply(data_vertrauenswürdig, 1, function_abholzigkeit) # Anholzigkeiten der Bäume berechnen
    
    
    ### Extrapolation der Durchmesser (Baum für Baum)
    anz_col <- ncol(data_vertrauenswürdig)
    y<- 1
    
    for (i in 1:nrow(data_vertrauenswürdig)) {
      y <-1
      while (y<=anz_col) {
        if(is.na(data_vertrauenswürdig[i,y])){
          # diff<- data_vertrauenswürdig[i,y-2]-data_vertrauenswürdig[i, y-1]
          # diff <- abs(diff)
          data_vertrauenswürdig[i,y] <- data_vertrauenswürdig[i,y-1]-abholzigkeit[i]
          if (data_vertrauenswürdig[i,y]<0) {
            data_vertrauenswürdig[i,y] <- 0
          }
          y<- y+1
        } else {
          y<- y+1
        }
      }
    }
    

# Daten berechnen ----------------------------------------------------------
    
  ## Berechnung der Bezugswalze----
  data_metrics$G_DBH<- (pi/4)*data_metrics$DBH^2
  data_metrics$V_Walze<- data_metrics$TH*data_metrics$G_DBH
    
  ## Berechnung des wahren Baumvolumens----
  data_vertrauenswürdig$v_wahr <- rowSums(data_vertrauenswürdig^2)*pi/4*0.2
  data_vertrauenswürdig$v_wahr <- round(data_vertrauenswürdig$v_wahr,2)
    

  ## Zusammenführen der Datensätze----
  data_collection<- cbind.data.frame(
                      data_metrics$Tree_Nr,
                      data_metrics$TH,
                      data_metrics$DBH,data_metrics$G_DBH,
                      data_metrics$V_Walze, 
                      data_vertrauenswürdig$v_wahr
                      )
    
  colnames(data_collection)<- c("Tree_Nr", "TH", "DBH", 
                                  "G_DBH", "V_Walze","V_wahr")
    
  ## Berechnung Formzahl----
  data_collection$f_13 <- data_collection$V_wahr/data_collection$V_Walze
      
      

# Exportdatei erstellen ----------------------------------------------------------
    
  ## Exportdatei erstellen  (Benneung anpassen an Untersuchungsgruppe)
  write.csv2(data_collection,"data_collection_straße.csv", row.names=TRUE) # Benneung anpassen an Utersuchungsgruppe
  write.csv2(data_vertrauenswürdig,"data_vertrauenswürdig_straße.csv", row.names=TRUE)
      
    
    
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      