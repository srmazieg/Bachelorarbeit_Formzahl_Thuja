# Pakcages laden ----------------------------------------------------------

library(readxl)
library(dplyr)
library(openxlsx)


# Daten importieren ----------------------------------------------------------

  ## Dateipfad abfragen (Auswahl der durch 3DFin erstellten .xlsx-Datei der Untersuchungsgruppe)
  datei_path<- choose.files()

  ## Daten metrics
  data_metrics      <- read_xlsx(datei_path, sheet = 1,range = "B3:F300", col_names = TRUE)
  data_metrics      <- na.omit(data_metrics)
  data_metrics      <- data_metrics %>% rename(Tree_Nr=...1)
  
  ## Daten diameter
  data_diameters <- read_xlsx(datei_path, sheet = 2,range = "B3:FT300", col_names = TRUE)
  data_diameters <- na.omit(data_diameters)
  data_diameters <- data_diameters %>% rename(Tree_Nr=...1)

  ## Daten X
  data_x <- read_xlsx(datei_path, sheet = 3,range = "B3:FT300", col_names = TRUE)
  data_x <- na.omit(data_x)
  data_x <- data_x %>% rename(Tree_Nr=...1)
  
  ## Daten Y
  data_y <- read_xlsx(datei_path, sheet = 4,range = "B3:FT300", col_names = TRUE)
  data_y <- na.omit(data_y)
  data_y <- data_y %>% rename(Tree_Nr=...1)
  
  
  # Daten Sections
  data_sec <- read_xlsx(datei_path, sheet = 5,range = "B3:FT300", col_names = TRUE)
  data_sec <- na.omit(data_sec)
  
  ## Daten overall_quality
  data_overall_quality <- read_xlsx(datei_path, sheet = 6,range = "B3:FT300", col_names = TRUE)
  data_overall_quality <- na.omit(data_overall_quality)
  data_overall_quality <- data_overall_quality %>% rename(Tree_Nr=...1)
  
  ## Daten Q1(Outlier Probability)
  data_q1 <- read_xlsx(datei_path, sheet = 7,range = "B3:FT300", col_names = TRUE)
  data_q1 <- na.omit(data_q1)
  data_q1 <- data_q1 %>% rename(Tree_Nr=...1)
  
  ## Daten Q2(Sector Occupancy)
  data_q2 <- read_xlsx(datei_path, sheet = 8,range = "B3:FT300", col_names = TRUE)
  data_q2 <- na.omit(data_q2)
  data_q2 <- data_q2 %>% rename(Tree_Nr=...1)
  
  
  ## Daten Q3(Points Inner Circle)
  data_q3 <- read_xlsx(datei_path, sheet = 9,range = "B3:FT300", col_names = TRUE)
  data_q3 <- na.omit(data_q3)
  data_q3 <- data_q3 %>% rename(Tree_Nr=...1)
  
  
# Gefilterte Datensätze erstellen ----------------------------------------------------------
    
    ## Anzahl Objekte bestimmen
    Anz_Objekte_Ursprung <- nrow(data_metrics)
    Anz_Stichprobe_20 <- round(Anz_Objekte_Ursprung*0.2,0)
    
    ## Baumnummern Bestimmen
    data_20 <- data_metrics[order(-data_metrics$DBH),]
    Baum_Nr <- data_20$Tree_Nr[1:Anz_Stichprobe_20]
    
    ## Erstellen gefilterte Datensätze
    data_metrics_20         <- data_metrics   %>% filter(Tree_Nr %in% Baum_Nr)
    data_diameterss_20      <- data_diameters %>% filter(Tree_Nr %in% Baum_Nr)
    data_x_20               <- data_x %>% filter(Tree_Nr %in% Baum_Nr)
    data_y_20               <- data_y %>% filter(Tree_Nr %in% Baum_Nr)
    data_overall_quality_20 <- data_overall_quality %>% filter(Tree_Nr %in% Baum_Nr)
    data_q1_20              <- data_q1 %>% filter(Tree_Nr %in% Baum_Nr)
    data_q2_20              <- data_q2 %>% filter(Tree_Nr %in% Baum_Nr)
    data_q3_20              <- data_q3 %>% filter(Tree_Nr %in% Baum_Nr)
  
# Exportdatei erstellen ----------------------------------------------------------
    
    ## Workbook erstellen
    wb <- createWorkbook()
    
    ## Arbeitsmappen erstellen
    addWorksheet(wb, "Plot Metrics")
    addWorksheet(wb, "Diameters")
    addWorksheet(wb, "X")
    addWorksheet(wb, "Y")
    addWorksheet(wb, "Sections")
    addWorksheet(wb, "Q(Overall Quality 0-1)")
    addWorksheet(wb, "Q1(Outlier Probability)")
    addWorksheet(wb, "Q2(Sector Occupancy)")
    addWorksheet(wb, "Q3(Points Inner Circle)")

    ## Arbeitsblätter füllen
    writeData(wb, "Plot Metrics", data_metrics_20)
    writeData(wb, "Diameters", data_diameterss_20)
    writeData(wb, "X", data_x_20)
    writeData(wb, "Y", data_y_20)
    writeData(wb, "Sections", data_sec)
    writeData(wb, "Q(Overall Quality 0-1)", data_overall_quality_20)
    writeData(wb, "Q1(Outlier Probability)", data_q1_20)
    writeData(wb, "Q2(Sector Occupancy)", data_q2_20)
    writeData(wb, "Q3(Points Inner Circle)", data_q3_20)
    
    ## Speichern der Excel (Benneung anpassen an Untersuchungsgruppe)
    saveWorkbook(wb, "ziegeler_Scan01_Bestandesrand_30m_20Prozent.xlsx")
  