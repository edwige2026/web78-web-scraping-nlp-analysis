#Aufgabe 1  

# Teilaufgabe 1a): Datenbereinigung

  # Daten einlesen und Speicherung in einem Dataframe
  loan <- read.csv('loan.csv')
  
  loan_backup <- loan #Um die ursprünglichen Werte beizubehalten.
  
  loan <- read.csv('loan.csv',na.strings = c("") ,stringsAsFactors=TRUE ,header=TRUE) #Import mit: 
                                                                                       #"na.string" um leeren Werten in NA zu ersetzen, 
                                                                                       #"stringsAsFactors" um Zeichenketten in Faktortypen zu konvertieren,
                                                                                       #"header" um sich zu versichern, dass R die erste Zeile der Daten als Überschrift verwendet.
  
  head(loan) # Die Funktion "head()" ermöglicht es, einen Überblick über alle Spaltennamen zu erhalten.
  
  str(loan)  # Die Funktion "str()" liefert Informationen über Typ und Struktur des Dataframe (loan). 

  #Datenbereinigung
    #Zeilen mit fehlenden Daten entfernen
    library(dplyr)
    loan <- na.omit(loan)#Ergebnis: 680 obs. von 111 Variable  
    
    #Spalten mit fehlenden Daten entfernen. 
      #Da alle Berechnungen, die mindestens ein NA-Wert enthalten, als Ergebnis NA liefern, werden alle Spalten mit NA-Werten entfernt.
      loan[,c("total_rev_hi_lim","inq_fi","total_cu_tl","inq_last_12m","acc_open_past_24mths","avg_cur_bal","bc_open_to_buy","bc_util")]<-list(NULL)
      loan[,c("mo_sin_old_il_acct","mo_sin_old_rev_tl_op","mo_sin_rcnt_rev_tl_op","mo_sin_rcnt_tl","mort_acc","mths_since_recent_bc")]<-list(NULL)
      loan[,c("mths_since_recent_bc_dlq","mths_since_recent_inq","mths_since_recent_revol_delinq","num_accts_ever_120_pd","num_actv_bc_tl")]<-list(NULL)
      loan[,c("num_actv_rev_tl","num_bc_sats","num_bc_tl","num_il_tl","num_op_rev_tl","num_rev_accts","num_rev_tl_bal_gt_0","num_sats","num_tl_120dpd_2m")]<-list(NULL)
      loan[,c("num_tl_30dpd","num_tl_90g_dpd_24m","num_tl_op_past_12m","pct_tl_nvr_dlq","ercent_bc_gt_75","tot_hi_cred_lim","total_bal_ex_mort","total_bc_limit","total_il_high_credit_limit")]<-list(NULL)
      loan[,c("all_util","max_bal_bc","open_rv_24m","open_rv_12m","il_util","total_bal_il","mths_since_rcnt_il","open_il_24m","open_il_12m","open_il_6m","open_acc_6m","tot_cur_bal","tot_coll_amt","verification_status_joint","mths_since_last_delinq","mths_since_last_record","mths_since_last_major_derog","percent_bc_gt_75","dti_joint","annual_inc_joint")]<-list(NULL)
    
      #jetzt haben wir ein Datensatz mit 680 obs und 55 Variablen
    
#Teilaufgabe 1b: Explorative Datenanalyse
    #Frequenzdiagramme für die Variable „loan_amnt“ erstellen, wenn der Zweck (Spalte „purpose“) den Wert „car“, „house“, „credit_card“ oder „small_business“ hat.
      #Als Vorarbeit für die Erstellung von Diagrammen wird hier erstmal die Faktorisierung von betrachteten Variablen durchgeführt.
        #Da die Variable "loan_amnt" bereist als Integer hinterlegt wurde, brauchen wir nicht mehr sie zu faktorisieren.
        #Die Variable "purpose" wurde als character hinterlegt und gleichzeitig beim Import faktorisiert.
          loan$purpose<-as.factor(loan$purpose)
          str(loan$purpose) #zur Prüfung der Struktur
        #hier wurde zunächst versucht, eine Teilmenge der Daten herauszufiltern, die die Werte "car", "house", "credit_card" und "small_business" enthält.
          subset_loan<-subset(loan, (purpose=="car" | purpose=="house" | purpose=="credit_card" | purpose=="small_business"))
          
      #Histogramm für die Teilmenge "subset_loan" mit ggplot2
        library(ggplot2)
        ggplot(subset_loan, aes(x = loan_amnt, fill = purpose, color = purpose, label = after_stat(count))) + geom_histogram(binwidth=1000, position = "stack", color = "white") + labs(x = "Loan_amnt", y = "Häufigkeit") + ggtitle("Histogramm") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + geom_text(stat = "bin", vjust = -0.5, binwidth = 1000, position = "stack", color = "black") + scale_fill_manual(labels = c("car", "house", "credit_card", "small_business"), values = c("darkred", "steelblue", "yellow", "green"))
      #Boxplot  
      boxplot(subset_loan$loan_amnt~subset_loan$purpose, xlab = "Purpose", ylab = "Häufigkeit", main="Boxplot für die Variable loan_amnt")
    
    #Frequenzdiagram für den Zinssatz „int_rate“. 
      #Transformation des Datensatzes von einem String in einen nummerischen Wert. 
        loan$int_rate<-as.factor(loan$int_rate)
        str(loan$int_rate)
        num_int_rate = as.numeric(loan$int_rate)
      ggplot(loan, aes(x = num_int_rate, label = after_stat(count))) + geom_histogram(position = "stack") + labs(x = "int_rate", y = "Häufigkeit") + ggtitle("Histogramm") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + geom_text(stat = "bin", vjust = -0.5, position = "stack") 
      
    #Durchführung einer deskriptive statistische Analyse für die Spalten "loan_amnt“, "installment“, "int_rate“, "funded_amnt“, "annual_inc“, "grade“ und "sub_grade“.
      #Spalte "loan_amnt“
        #Struktur Überprüfung
          str(loan$loan_amnt) #Ergebnis: numerischer Wert. Das heißt, wir brauchen hier nicht zu faktorisieren
        #Überblick descriptive Werten
          summary(loan$loan_amnt) #Ergebnis: Damit bekommen wir das Minimum (1500), 1.(12000) sowie 3.(24000) Quartil, Median(16000) und Mittelwert(17609) der Variable loan_amnt.
        
        #Breiter Überblick descriptive Werten
          install.packages("psych")
          library(psych)
          describe(loan$loan_amnt) #Ergenis: mit
                                             #"n"(680) die Anzahl der Beobachtungen,
                                             #"mean"(17608.6) der Mittelwert,
                                             #"sd"(8660.21) die Standardabweichung,
                                             #"median"(16000) der Median,
                                             #"trimmed"(17178.03) der getrimmte Mittelwert,
                                             #"mad"(8006.04) die mittlere absolute Abweichung,
                                             #"min"(1500) das Minimum,
                                             #"max"(35000) das Maximum,
                                             #"range"(33500) die Spannweite,
                                             #"skew"(0.39) die Schiefe,
                                             #"kurtosis"(-0.57) das Kurtosis,
                                             #"se"(332.1) der Standardfehler des Mittelwerts.
        #Die Funktion desc() liefert noch eine erweitere textuelle und auch eine visuelle Beschreibung, mit der man herausfinden kann, ob in der betreffenden Variablen noch fehlende Werte vorhanden sind.
          install.packages("DescTools")
          library(DescTools)
          Desc(loan$loan_amnt) #Ergebnis: Zusätzlich zu den Parametern, die bereits mit Hilfe der vorherigen Funktionen beschrieben wurden, kommen hier folgende Parameter hinzu:
                                  #"unique“ (175), die Anzahl einmalig vorkommender Ausprägungen
                                  #"lowest" und "highest", die Anzahl der niedrigsten und höchsten Werten am Ende.
        
      #Spalte "installment"
        #Struktur Überprüfung
          str(loan$installment) #Ergebnis: numerischer Wert
        #Überblick descriptive Werten
          Desc(loan$installment)
       
      #Spalte "int_rate"
        #Struktur Überprüfung
          str(loan$int_rate) #Ergebnis: character Wert faktorisiert
        #Überblick descriptive Werten
          Desc(loan$int_rate)
          
      #Spalte "funded_amnt"
        #Struktur Überprüfung
          str(loan$funded_amnt) #Ergebnis: numerischer Wert
        #Überblick descriptive Werten
          Desc(loan$funded_amnt)
          
      #Spalte "annual_inc"
        #Struktur Überprüfung
          str(loan$annual_inc) #Ergebnis: numerischer Wert 
        #Überblick descriptive Werten
          Desc(loan$annual_inc)
          
      #Spalte "grade"
        #Struktur Überprüfung
          str(loan$grade) #Ergebnis: character Wert faktorisiert 
        #Überblick descriptive Werten
          Desc(loan$grade)  
          
      #Spalte "sub_grade"
        #Struktur Überprüfung
          str(loan$sub_grade) #Ergebnis: character Wert faktorisiert 
        #Überblick descriptive Werten
          Desc(loan$sub_grade)  
          
    #Korrelation mit anderen numerischen Variablen NB: je vais un peu sortir un Kreuztabelle ici avant de calculer pour voir.......
      cor(loan$loan_amnt, loan$annual_inc, use="pairwise.complete.obs", method = c("pearson", "kendall", "spearman")) #Ergebnis: Die Variable „loan_amnt“ korreliert gerade 36,4% mit dem Attribut „annual_inc“.
      cor(loan$loan_amnt, loan$installment, use="pairwise.complete.obs", method = c("pearson", "kendall", "spearman")) #Ergebnis: Die Variable „loan_amnt“ ist in mit einem Prozentsatz von 95,43 stark mit dem Attribut „installment“ korreliert.
      cor(loan$loan_amnt, loan$funded_amnt, use="pairwise.complete.obs", method = c("pearson", "kendall", "spearman")) #Ergebnis: Die Variable „loan_amnt“ ist mit einem Prozentsatz von 96,73 ebenfalls stark mit dem Attribut „funded_amnt“ korreliert.
      # Ergebnis: Anhand der Ergebnisse lässt sich erkennen, dass die Variable „loan_amnt“ eine sehr starke Korrelation mit den Variablen „installment“ und „funded_amnt“ aufweist.    
          
  #Erstellen Sie danach eine Visualisierung, um zu zeigen für welchen „grade“ die Wahrscheinlichkeit am größten ist, dass der „loan_status“= „Fully Paid“ ist. 
   ...
    
  #Erstellen Sie danach eine Visualisierung, um zu zeigen für welchen „prupose“ die Wahrscheinlichkeit am größten ist, dass der „loan_status“= „Charged Off“ ist. 
    ...    
    
#Teilaufgabe 1c: Modellbildung     
  #...    
      
      
      
      
      
      
      