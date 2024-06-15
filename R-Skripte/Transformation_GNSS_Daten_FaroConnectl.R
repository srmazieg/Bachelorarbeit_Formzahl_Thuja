
#Einführung----------


# Dieses R-Script dient der Datentransformation von Geodaten aus Emlid Studio in ein für FARO Connect
# gültiges Format. Mit Hilfe dieser transformierten Daten ist beispielsweise eine Georeferenzierung
# von Punktwolken mit FARO Connect möglich.

#_________________________________________________________________________________________________________________

# Einstellungen Emlid Studio----


# Folgende Einstellungen sind dabei in Emlid Studio zu verwenden:

# Rover       <-  Raw-Datei
# base        <-  Base-Datei
# Navigation  <-  Raw-Datei

# Anpassung der Einstellungen (Zahnrad)

# File name       <-  Dateiname         (Anpassung des Dateinamens)
# Results folder  <-  Zielordners       (Festlegung des Zielordners)
# Solution Format <-  Lat/Long/Height   (Einstellung der Koordinatenangabe)
# Time Format     <-  ddd.ddddddd       (Einstellung des GPS-Zeitformates)

# Einstellungen speichern mit "Save"
# GNSS-Daten prozessieren mit "Process"

#_________________________________________________________________________________________________________________


# R-Code ----


# Datenimport

# Anzahl zu importierender Stellen erhöhen
options(digits = 25)

# Einlesen Emlid-Datei 
data_emlid<- read.delim(choose.files(), 
                        header = FALSE,
                        dec = ".",
                        stringsAsFactors = FALSE, 
                        sep = "", 
                        skip = 10, 
                        blank.lines.skip = TRUE,
                        strip.white= TRUE)

colnames(data_emlid)<- c("gps_week","gps_time","lat","long","elev","Q","ns",
                         "sdn","sde","sdu","sdne","sdeu","sdun","age","ratio")

#data_emlid[is.na(data_emlid)]<- 0 

# Datenaufbereitung

# Entfernen Dezimaltrennteichen aus Spalte 2
data_emlid$gps_time<- format(round(data_emlid$gps_time, 0), nsmall = 0)

# Verkettung der Spalten 1+ 2 in neuer Spalte
data_emlid$gps<- paste(data_emlid$gps_week,data_emlid$gps_time,sep='')


# Genauigkeit positives Vorzeichen
data_emlid$sdne<- abs(data_emlid$sdne)


# Datenexport

# Erstellung Export_Datensatz
data_faro<- data_emlid[,c("gps","long","lat","elev","sdne")]


# Zeichenanzahl begrenzen

#data_faro$data_emlid.GPST <- trimws(substr(data_faro$data_emlid.GPST, 1, 10))
#data_faro$data_emlid.V9   <- trimws(format(data_faro$data_emlid.V9,width = 10, format = "d",justify = "left"))
#data_faro$data_emlid.V5   <- trimws(format(data_faro$data_emlid.V5,width = 9, format = "d",justify = "left"))
#data_faro$data_emlid.V12  <- trimws(format(data_faro$data_emlid.V12,width = 10, format = "d",justify = "left"))
#data_faro$data_emlid.V30  <- trimws(format(data_faro$data_emlid.V30,width = 8, format = "d",justify = "left"))


# Entfernen von Nullzeilen ->_ data_faro-Wert ggf. anpassen 
#data_faro <- data_faro[rowSums(is.na(data_faro) | data_faro < 6) < ncol(data_faro), ]



# Spaltennamen entfernen
colnames(data_faro)<- NULL

# Export des Export_Datensatzes im csv-Format
write.table(data_faro,
            file = "GNSS_Daten_GeoRef_Faro_Buchennullfaeche.csv",
            sep = ",",
            dec = ".", 
            row.names = FALSE, 
            col.names = FALSE,
            quote = FALSE)

