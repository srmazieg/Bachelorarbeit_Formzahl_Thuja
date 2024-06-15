# Packages laden ----------------------------------------------------------
    
  library(psych)
  library(ggplot2)
  library(dplyr)
  library(qqplotr)
  library(MESS)

# Auswertung Formzahlen ---------------------------------------------------


  ## Einlesen der Daten ------------------------------------------------------
  
    ### Einlesen der Daten(Formzahlen)
    setwd(choose.dir())
    data_bestand  <- read.csv2("data_collection_Bestand.csv")
    data_straße   <- read.csv2("data_collection_straße.csv")

    ### Unterscuhunggruppe hinterlegen
    data_bestand$Gruppe <- "Bestand"
    data_straße$Gruppe  <- "Bestandesrand"
    
    ### Datensätze zusammenführen
    data_zusammen <- rbind(data_bestand,data_straße)
    data_zusammen <- data_zusammen[c(9,2,3,4,5,6,7,8)]
    data_zusammen$Gruppe <- as.factor(data_zusammen$Gruppe)
  


  ## Deskriptive Statistik ---------------------------------------------------
  
    ### Kennwerte berechnen====
    stats_bestand       <- describe(data_bestand$f_13, interp = FALSE, 
                              fast = NULL,quant=c(.25,.75))
    stats_bestandesrand <- describe(data_straße$f_13, interp = FALSE, 
                                    fast = NULL, quant=c(.25,.75))
    stats_gesamt        <- describe(data_zusammen$f_13,  interp = FALSE, 
                                    fast = NULL,quant=c(.25,.75))
    
    
    ### Kennwerte runden (2 Nachkommastellen)
    stats_gesamt        <- round(stats_gesamt,2)
    stats_bestand       <- round(stats_bestand, 2)
    stats_bestandesrand <- round(stats_bestandesrand,2)
    
    ### Kennwerte zusammenführen
    stats_gesamt$Gruppe         <- "Kollektiv"
    stats_bestand$Gruppe        <- "Bestand"
    stats_bestandesrand$Gruppe  <- "Bestandesrand"
    stats_zusammengeführt <- rbind(stats_bestand,stats_bestandesrand,
                                   stats_gesamt)
    stats_zusammengeführt <- stats_zusammengeführt[c(16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
      
    ### Kennwerte exportieren====
    # write.table(stats_zusammengeführt, choose.files(), 
    #             sep = ";", col.names = TRUE, row.names = FALSE)
    write.csv2(stats_zusammengeführt, "statistische_Kennzahlen_zusammengeführt.csv", row.names = FALSE)
      
    ### Boxplots====
    boxplots <- ggplot(data_zusammen, aes(x = Gruppe, y = f_13))+
      stat_boxplot(geom = "errorbar", width = 0.25)+
      geom_boxplot()+
      labs(x= "Gruppe", y = "Formzahl")
    ggsave(filename = "Boxplots.png", plot = boxplots, width = 8, height = 6, units = "cm")
      
  ## Test Normalverteilung ---------------------------------------------------

    ### Q-Q-Diagramm Facet  Formzahlen====
    qq_facet<- ggplot(data_zusammen, aes(sample =f_13))+
      stat_qq_band(conf = 0.9)+
      stat_qq_line(color = "red")+
      stat_qq_point(size = 1,color = "black")+
      labs(x= "theoretische Quantile", y = "tatsächliche Quantile")+
      facet_wrap( ~Gruppe, , scales = "free")
        
    ggsave(filename = "qqplot_facet.png", plot = qq_facet, width = 12, height = 6, units = "cm")
                      

      
  ## Signifikanz Test --------------------------------------------------------

    ### Stichprobenumfang t-Test berechnen====
    n_t_Test <- power_t_test(delta = 0.5, sig.level = 0.05, 
                           power = 0.8, ratio = 1, type = "two.sample", 
                           alternative = "two.sided")
    
    ### Mann-Whintey-U-Test====
    signifikanz <- wilcox.test(f_13~Gruppe, data = data_zusammen, 
                             exact = TRUE, correct = FALSE)
    z <- qnorm(signifikanz$p.value/2)
    r <- abs(z/sqrt(nrow(data_zusammen)))
    





