rm(list=ls())
#install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(ggplot2)
library(rts)
library(viridis)
library(dplyr)
library(ggpubr)
library(broman)
library(data.table)
library(plyr)
require(foreign)
require(MASS)
require(Hmisc)
require(reshape2)
require(foreach)
library(dplyr)  # Data frame manipulation
library(readr)  # Read CSVs nicely
library(broom)  # Convert models to data frames
library(pastecs)
library(car)
library(multcomp)
library(idefix)
library(mlogit)
library(Formula)
library(readxl)
library(jtools)
library(ggstance)
library(broom.mixed)
library(coefplot)
library(data.table)
library(gmnl) 
library(mlogit)
library(RColorBrewer)
library(stargazer)
library(sf)
library(rgdal)
library(CARBayesdata)
library(CARBayes)
library(RColorBrewer)
library(spdep)
library(coda)
library(tidyr)
#library(xlsx)
library(plotrix)


options(scipen=999)
rm(list=ls())

#-------------------------------------------------------------------------------
# NOTE ON LICENCE
# This code is made available under an MIT license. 
# The code is free to use, but the author of the code will have to be cited for
# every form of publication that is created with the code.
#-------------------------------------------------------------------------------






#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Automatic process of generating an orthogonal array
# Use this code to create an orthogonal array with input from the "intake qualtrics"
# which can be found here: https://erasmusuniversity.eu.qualtrics.com/jfe/form/SV_3UxuyC9IwywzA8e 
# The output of this code can be used to create a Qualtrics DCE survey through the Python code 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Here load the data from the Intake Form from Qualtrics. Ideally, separate the different intakes from the different applications. 
# This means there is only 1 row in the excel sheet, for the particular application you want to create the form for. 
intake.data <- read_excel("[yourpath]/.xlsx") #fill in the path to the excel export from the QUaltrics intake survey
intake.data = intake.data[-1,]
intake.data = intake.data[which(as.numeric(intake.data$aantal_meedoen) > 99),]
intake.data = intake.data[which(!is.na(intake.data$type)),]



#This code restructures the database and creates new variables names 
newdata = data.frame(
  "name"             = intake.data$Naam,
  "member_nr"        = as.numeric(intake.data$aantal_leden),
  "member_part"      = as.numeric(intake.data$aantal_meedoen),
  "type_name"        = ifelse(intake.data$type == "Energie", "energy",
                              ifelse(intake.data$type == "Voedsel", "food",
                                     ifelse(intake.data$type == "Zorg", "care", "other"))),
  "type_number"      = ifelse(intake.data$type == "Energie", 1,
                              ifelse(intake.data$type == "Voedsel", 2,
                                     ifelse(intake.data$type == "Zorg", 3, 4))),
  "attributes_set"   = ifelse(intake.data$type == "Energie", 5,
                              ifelse(intake.data$type == "Voedsel", 3,
                                     ifelse(intake.data$type == "Zorg", 4, 3))),
  "attributes_extra" = ifelse(intake.data$type == "Energie", (lengths(gregexpr(",", intake.data$energie_attributen)) + 1),
                              ifelse(intake.data$type == "Voedsel", (lengths(gregexpr(",", intake.data$voedsel_attributen)) + 1),
                                     ifelse(intake.data$type == "Zorg", (lengths(gregexpr(",", intake.data$zorg_attributen)) + 1), 
                                            (lengths(gregexpr(",", intake.data$anders_attributen)) + 1)))),
  "survey_questions"=  ifelse(intake.data$type == "Energie", intake.data$survey_energie,
                              ifelse(intake.data$type == "Voedsel", intake.data$survey_voedsel,
                                     ifelse(intake.data$type == "Zorg", intake.data$survey_zorg, intake.data$survey_anders))),
  "att1"            = 2, #this is the number of options; by putting this here we know that an attribute is included (not 0) and how many attribute levels there are 
  "att2"            = 2, #for att1, att2 and att3 we know they are always in here (they are not optional in the intake), so they are standard 2
  "att3"            = 2,  
  "att4"            = ifelse(grepl("Impact", intake.data$energie_attributen) |
                               grepl("Impact", intake.data$voedsel_attributen) |
                               grepl("Impact", intake.data$zorg_attributen)    |
                               grepl("Impact", intake.data$anders_attributen), 2, 0 ), #att4 is not standard selected, so if it is 2 we know it was selected and that it has 2 attribute levels
  "att5"            = ifelse(intake.data$type == "Energie", 4, 0),
  "att6"            = ifelse(intake.data$type == "Energie", 4, 0),
  "att7"            = ifelse(grepl("Winst", intake.data$energie_attributen) |
                               grepl("Winst", intake.data$voedsel_attributen) |
                               grepl("winst", intake.data$zorg_attributen)    |
                               grepl("Winst", intake.data$anders_attributen), 3, 0),
  "att8"            = ifelse(grepl("Hoeveelheid", intake.data$energie_attributen) |
                               grepl("Hoeveelheid", intake.data$voedsel_attributen) |
                               grepl("Hoeveelheid", intake.data$zorg_attributen)    |
                               grepl("Hoeveelheid", intake.data$anders_attributen), 2, 0),  
  "att9"            = ifelse(grepl("Afstand", intake.data$energie_attributen) |
                               grepl("Afstand", intake.data$voedsel_attributen) |
                               grepl("Afstand", intake.data$zorg_attributen)    |
                               grepl("Afstand", intake.data$anders_attributen), 3, 0),   
  "att10"            = ifelse(grepl("Transparantie", intake.data$energie_attributen) |
                                grepl("Transparantie", intake.data$voedsel_attributen) |
                                grepl("Transparantie", intake.data$zorg_attributen)    |
                                grepl("Transparantie", intake.data$anders_attributen), 2, 0),   
  "att11"            = ifelse(grepl("Verplichte", intake.data$energie_attributen) |
                                grepl("Verplichte", intake.data$voedsel_attributen) |
                                grepl("Verplichte", intake.data$zorg_attributen)    |
                                grepl("Verplichte", intake.data$anders_attributen), 2, 0),     
  "att12"            = ifelse(grepl("Aantal", intake.data$energie_attributen) |
                                grepl("Aantal", intake.data$voedsel_attributen) |
                                grepl("Aantal", intake.data$zorg_attributen)    |
                                grepl("Aantal", intake.data$anders_attributen), 3, 0),     
  "att13"            = ifelse(grepl("Diversificatie", intake.data$energie_attributen) |
                                grepl("Diversificatie", intake.data$voedsel_attributen) |
                                grepl("Diversificatie", intake.data$zorg_attributen)    |
                                grepl("Diversificatie", intake.data$anders_attributen), 3, 0),      
  "att14"            = ifelse(grepl("Energie levering", intake.data$energie_attributen) |
                                grepl("Energie levering", intake.data$voedsel_attributen) |
                                grepl("Energie levering", intake.data$zorg_attributen)    |
                                grepl("Energie levering", intake.data$anders_attributen), 2, 0), 
  "att15"            = ifelse(grepl("Advies", intake.data$energie_attributen) |
                                grepl("Advies", intake.data$voedsel_attributen) |
                                grepl("Advies", intake.data$zorg_attributen)    |
                                grepl("Advies", intake.data$anders_attributen), 2, 0),
  #  "att16"           = ifelse(intake.data$type == "Voedsel", 3, 0),   #used to be weekkosten 
  "att17"            = ifelse(grepl("Manier", intake.data$energie_attributen) |
                                grepl("Manier", intake.data$voedsel_attributen) |
                                grepl("Manier", intake.data$zorg_attributen)    |
                                grepl("Manier", intake.data$anders_attributen), 2, 0),
  "att18"            = ifelse(grepl("Wat ", intake.data$energie_attributen) |
                                grepl("Wat ", intake.data$voedsel_attributen) |
                                grepl("Wat ", intake.data$zorg_attributen)    |
                                grepl("Wat ", intake.data$anders_attributen), 2, 0),  
  "att19"            = ifelse(grepl("Uniformiteit", intake.data$energie_attributen) |
                                grepl("Uniformiteit", intake.data$voedsel_attributen) |
                                grepl("Uniformiteit", intake.data$zorg_attributen)    |
                                grepl("Uniformiteit", intake.data$anders_attributen), 2, 0),   
  "att20"            = ifelse(grepl("Type", intake.data$energie_attributen) |
                                grepl("Type", intake.data$voedsel_attributen) |
                                grepl("Type", intake.data$zorg_attributen)    |
                                grepl("Type", intake.data$anders_attributen), 3, 0),    
  "att21"            = ifelse(grepl("Voedsel", intake.data$energie_attributen) |
                                grepl("Voedsel", intake.data$voedsel_attributen) |
                                grepl("Voedsel", intake.data$zorg_attributen)    |
                                grepl("Voedsel", intake.data$anders_attributen), 2, 0),  
  "att22"            = ifelse(grepl("Onderhoud", intake.data$energie_attributen) |
                                grepl("Onderhoud", intake.data$voedsel_attributen) |
                                grepl("Onderhoud", intake.data$zorg_attributen)    |
                                grepl("Onderhoud", intake.data$anders_attributen), 2, 0),   
  "att23"            = ifelse(grepl("Lengte", intake.data$energie_attributen) |
                                grepl("Lengte", intake.data$voedsel_attributen) |
                                grepl("Lengte", intake.data$zorg_attributen)    |
                                grepl("Lengte", intake.data$anders_attributen), 2, 0),  
  "att24"            = ifelse(grepl("Biologisch", intake.data$energie_attributen) |
                                grepl("Biologisch", intake.data$voedsel_attributen) |
                                grepl("Biologisch", intake.data$zorg_attributen)    |
                                grepl("Biologisch", intake.data$anders_attributen), 2, 0),  
  "att25"           = ifelse(intake.data$type == "Zorg", 3, 0),
  "att26"            = ifelse(grepl("Doelgroep", intake.data$energie_attributen) |
                                grepl("Doelgroep", intake.data$voedsel_attributen) |
                                grepl("Doelgroep", intake.data$zorg_attributen)    |
                                grepl("Doelgroep", intake.data$anders_attributen), 3, 0),   
  "att27"            = ifelse(grepl("Condities", intake.data$energie_attributen) |
                                grepl("Condities", intake.data$voedsel_attributen) |
                                grepl("Condities", intake.data$zorg_attributen)    |
                                grepl("Condities", intake.data$anders_attributen), 3, 0),   
  "att28"            = ifelse(grepl("Zorgtaken", intake.data$energie_attributen) |
                                grepl("Zorgtaken", intake.data$voedsel_attributen) |
                                grepl("Zorgtaken", intake.data$zorg_attributen)    |
                                grepl("Zorgtaken", intake.data$anders_attributen), 2, 0),   
  "att29"            = ifelse(grepl("Lidmaatschap", intake.data$energie_attributen) |
                                grepl("Lidmaatschap", intake.data$voedsel_attributen) |
                                grepl("Lidmaatschap", intake.data$zorg_attributen)    |
                                grepl("Lidmaatschap", intake.data$anders_attributen), 2, 0)  
  
)

#here we create all attribute levels for all possible included attributes 
att1 = c("mogelijk", "niet mogelijk")
att2 = c("een lid een stem", "in verhouding tot aantal aandelen in bedrijf")
att3 = c("klein en lokaal", "uitbreiding en groei")
att4 = c("bedrijfsfocus op maatschappelijke impact en leden", "focus alleen op leden")
att5 = c("0%", "33.3%", "66.6%", "100%")
att6 = c("20", "25", "30", "35")
att7 = c("verdelen als dividend", "investeren in de organisatie", "investeren in maatschappelijke projecten")
att8 = c("gelimiteerd", "ongelimiteerd")
att9 = c("lokaal (0-10 km)", "regionaal (10-30 km)", "supra-regionaal (>30 km)")
att10 = c("volledige transparantie", "wettelijk verplichte transparantie")
att11 = c("verplichte kosten om lid te worden", "geen verplichte kosten om lid te worden")
att12 = c("geen", "maandelijks", "jaarlijks")
att13 = c("geen diversificatie", "enige diversificatie", "hoge diversificatie")
att14 = c("via netbeheerder","via zelflevering")
att15 = c("algemeen advies","individueel advies")
#att16 = c("0-10 euro", "10-30 euro", "30-50 euro") #used to be weekkosten 
att17 = c("bezorgservice of zelf ophalen", "alleen zelf ophalen")
att18 = c("externe inkoop", "geen externe inkoop")
att19 = c("alle filialen hebben hetzelfde organisatiemodel", "filialen hebben autonomie over de organisatorische inrichting")
att20 = c("alleen producent", "alleen consument", "prosumer (producent en consument)")
att21 = c("het hele jaar rond", "enkele maanden per jaar")
att22 = c("verplichte participatie van leden", "vrijwillige participatie van leden")
att23 = c("korte keten met lokale producten", "langere keten met niet altijd lokale producten")
att24 = c("biologisch voedsel" , "geen biologisch voedsel")
att25 = c("zorgprofessionals in vaste dienst van de organisatie", 
          "zorgprofessionals in vaste dienst en taken uitgevoerd door leden",
          "taken uitgevoerd door leden")
att26 = c("ouderen", "mensen met een beperking", "geen specifieke doelgroep")
att27 = c("bepaald aantal jaren lidmaatschap", "éénmalige vergoeding om hulp te ontvangen", "geen specifieke eisen")
att28 = c("worden uitbesteed aan externe zorg providers", "worden gedaan door de organisatie zelf")
att29 = c("iedereen kan lid worden", "selectieve procedure om lid te worden")

#Attribute names 
att1_name = "Participatie van leden in besluitvorming"
att2_name = "Stemrecht van leden in de besluitvorming"
att3_name = "Evolutie en groei"
att4_name = "Impact"
att5_name = "Percentage groene energie"
att6_name = "Prijs per kWh (in centen)"
att7_name = "Winst"
att8_name = "Hoeveelheid leden"
att9_name = "Afstand tot hoofdkwartier van het burgercollectief"
att10_name = "Transparantie van prijsvaststelling"
att11_name = "Verplichte kosten bij lidmaatschap"
att12_name = "Aantal sociale events voor leden (naast ALV)"
att13_name = "Diversificatie van producten naast elektriciteit"
att14_name = "Energie levering"
att15_name = "Advies vanuit de organisatie over installaties en energiezuinig wonen"
#att16_name = "Wekelijke kosten"  #this one is out now 
att17_name = "Manier waarop voedsel verkregen wordt"
att18_name = "Wat gebeurt er bij een tekort"
att19_name = "Uniformiteit van filialen"
att20_name = "Type lidmaatschap"
att21_name = "Voedsel beschikbaarheid"
att22_name = "Onderhoud en productie"
att23_name = "Lengte voedselketen"
att24_name = "Biologisch voedsel"
att25_name = "De uitvoering van zorg"
att26_name = "Doelgroep"
att27_name = "Condities voor het ontvangen van hulp"
att28_name = "Zorgtaken"
att29_name = "Lidmaatschap voorwaarden"

#total number of attributes is the standard set attributes (varies per type) + the selected attributes
newdata$total_attributes = newdata$attributes_set + newdata$attributes_extra


#we select the attributes that were chosen + set; and this is a vector of levels (as we indicated above)  
row = 1 #here, select the row of the application you want to create the orthogonal array for. If there is only 1 row (recommended) then just leave this.

#identify the column numbers/places for the attribute variables 
x = match("att1",names(newdata)) #what is the place of att1
y = x + 26 #plus the 26 other att variables that are in there --> change this if the number of total possible attributes changes!

levels =  as.numeric(as.vector(newdata[row,x:y])) #select the first row, and the attributes
levels = levels[which(levels!=0)]


#which survey questions are in there? 
survey_questions = data.table(question = c("Wat is uw leeftijd?", 
                                           "Wat is uw gender?", 
                                           "Wat is het gecombineerde jaarlijkse netto inkomen van uw huishouden?", 
                                           "Wat is uw hoogst afgeronde opleiding?",
                                           "In welk jaar werd u lid van dit burgercollectief?"),
                              label = c("age", "gender", "income", "education", "yearmember" ))
survey_questions_extra = as.data.table(unlist(strsplit(newdata[row,]$survey_questions,",")))
colnames(survey_questions_extra) = c("question")
survey_questions_extra$label = with(survey_questions_extra, 
                                    ifelse(question == "Wat is uw leeftijd?", "age", 
                                           ifelse(question == "In welke gemeente woont u?", "municipality",
                                                  ifelse(question == "In wat voor een woongebied woont u?", "urban",
                                                         ifelse(question == "In welk type woning woont u?", "housetype",
                                                                ifelse(question == "Hoeveel leden telt uw huishouden (volwassenen en kinderen)?", "nrfamily",
                                                                       ifelse(question == "Hoeveel kiloWattuur gebruikt u in totaal per jaar?", "energyuse",
                                                                              ifelse(question == "Gebruikt u elektriciteit voor koken?", "cooking",
                                                                                     ifelse(question == "Heeft u een elektrische auto", "car",
                                                                                            ifelse(question == "Heeft u een (hybride) warmtepomp", "heatpump",
                                                                                                   ifelse(question == "Heeft u zonnepanelen?", "solarpanels",
                                                                                                          ifelse(question == "Ontvangt u op dit moment zorg vanuit het burgercollectief?", "care", 
                                                                                                                 ifelse(question == "Hoe tevreden bent u met uw lidmaatschap van dit burgercollectief?", "satisfaction",
                                                                                                                        ifelse(question == "Hoe belangrijk is de autonomie die een burgercollectief biedt ten opzichte van het systeem voor u?", "autonomy", NA))))))))))))))
survey_questions = rbind(survey_questions, survey_questions_extra)


survey_questions$option1 = with(survey_questions, 
                                ifelse(label == "age", "", 
                                       ifelse(label == "gender", "man", 
                                              ifelse(label == "income", "0 - 19.999", 
                                                     ifelse(label == "education", "lager onderwijs", 
                                                            ifelse(label == "yearmember", "", 
                                                                   ifelse(label == "municipality", "", 
                                                                          ifelse(label == "urban", "grote stad", 
                                                                                 ifelse(label == "housetype", "appartement", 
                                                                                        ifelse(label == "nrfamily", "", 
                                                                                               ifelse(label == "energyuse", "minder dan 1500 kWh", 
                                                                                                      ifelse(label == "cooking", "ja", 
                                                                                                             ifelse(label == "car", "ja", 
                                                                                                                    ifelse(label == "heatpump", "ja", 
                                                                                                                           ifelse(label == "solarpanels", "ja", 
                                                                                                                                  ifelse(label == "care", "ja", 
                                                                                                                                         ifelse(label == "satisfaction", "erg ontevreden", 
                                                                                                                                                ifelse(label == "autonomy", "erg onbelangrijk", ""))))))))))))))))))
survey_questions$option2 = with(survey_questions, 
                                ifelse(label == "gender", "vrouw", 
                                       ifelse(label == "income", "20.000 - 39.999", 
                                              ifelse(label == "education", "middelbaar onderwijs", 
                                                     ifelse(label == "urban", "buitenwijk van grote stad", 
                                                            ifelse(label == "housetype", "rijtjeshuis", 
                                                                   ifelse(label == "energyuse", "1500 - 1999 kWh", 
                                                                          ifelse(label == "cooking", "nee", 
                                                                                 ifelse(label == "car", "nee", 
                                                                                        ifelse(label == "heatpump", "ja hybride", 
                                                                                               ifelse(label == "solarpanels", "nee", 
                                                                                                      ifelse(label == "care", "nee", 
                                                                                                             ifelse(label == "satisfaction", "ontevreden", 
                                                                                                                    ifelse(label == "autonomy", "onbelangrijk", ""))))))))))))))
survey_questions$option3 = with(survey_questions, 
                                ifelse(label == "gender", "anders", 
                                       ifelse(label == "income", "40.000 - 59.999", 
                                              ifelse(label == "education", "hogeschool", 
                                                     ifelse(label == "urban", "dorp of kleine stad", 
                                                            ifelse(label == "housetype", "halfopen bebouwing", 
                                                                   ifelse(label == "energyuse", "2000 - 2499 kWh", 
                                                                          ifelse(label == "cooking", "zeg ik liever niet", 
                                                                                 ifelse(label == "car", "zeg ik liever niet", 
                                                                                        ifelse(label == "heatpump", "nee", 
                                                                                               ifelse(label == "solarpanels", "zeg ik liever niet", 
                                                                                                      ifelse(label == "care", "zeg ik liever niet", 
                                                                                                             ifelse(label == "satisfaction", "neutraal", 
                                                                                                                    ifelse(label == "autonomy", "neutraal", ""))))))))))))))
survey_questions$option4 = with(survey_questions, 
                                ifelse(label == "gender", "zeg ik liever niet", 
                                       ifelse(label == "income", "60.000 - 79.999", 
                                              ifelse(label == "education", "universiteit: bachelor", 
                                                     ifelse(label == "urban", "plattelandsdorp", 
                                                            ifelse(label == "housetype", "open bebouwing", 
                                                                   ifelse(label == "energyuse", "2500 - 2999 kWh", 
                                                                          ifelse(label == "heatpump", "zeg ik liever niet", 
                                                                                 ifelse(label == "care", "zeg ik liever niet", 
                                                                                        ifelse(label == "satisfaction", "tevreden", 
                                                                                               ifelse(label == "autonomy", "belangrijk", "")))))))))))
survey_questions$option5 = with(survey_questions, 
                                ifelse(label == "income", "80.000 - 99.999", 
                                       ifelse(label == "education", "universiteit: master", 
                                              ifelse(label == "urban", "huis op platteland", 
                                                     ifelse(label == "housetype", "zeg ik liever niet", 
                                                            ifelse(label == "energyuse", "3000 - 3499 kWh", 
                                                                   ifelse(label == "satisfaction", "erg tevreden", 
                                                                          ifelse(label == "autonomy", "erg belangrijk", ""))))))))
survey_questions$option6 = with(survey_questions, 
                                ifelse(label == "income", "80.000 - 99.999", 
                                       ifelse(label == "education", "universiteit: master", 
                                              ifelse(label == "urban", "huis op platteland", 
                                                     ifelse(label == "energyuse", "3000 - 3499 kWh", "")))))
survey_questions$option6 = with(survey_questions, 
                                ifelse(label == "income", "100.000 of meer", 
                                       ifelse(label == "education", "postgraduaat", 
                                              ifelse(label == "urban", "zeg ik liever niet", 
                                                     ifelse(label == "energyuse", "3500 - 3999 kWh", "")))))
survey_questions$option7 = with(survey_questions, 
                                ifelse(label == "income", "zeg ik liever niet", 
                                       ifelse(label == "education", "PhD", 
                                              ifelse(label == "energyuse", "4000 kWh of meer", ""))))
survey_questions$option8 = with(survey_questions, 
                                ifelse(label == "education", "zeg ik liever niet", 
                                       ifelse(label == "energyuse", "zeg ik liever niet", "")))

#save and export survey questions as csv file 
write.csv(survey_questions, "[yourpath]/survey_questions.csv", row.names = FALSE)


#make a table with attributes that are always in the orthogonal array 
att_table = data.table(attribuut = "Participatie van leden in besluitvorming", levels = "1 = mogelijk, 0 = niet Mogelijk", variabele = "participatie")
tab_att2 = data.table(attribuut = "Stemrecht van leden in de besluitvorming", levels = "1 = één lid één stem, 0 = in verhouding tot aantal aandelen in bedrijf", variabele = "democratisch.stemrecht")
tab_att3 = data.table(attribuut = "Evolutie en groei", levels = "1 = focus op uitbreiding en groei, 0 = focus op klein en lokaal", variabele = "evolutie")

att_table = rbind(att_table, tab_att2, tab_att3)

# add the optional attributes that were selected in the intake 
if(newdata[row,]$att4 > 0){
  tab_att4 = data.table(attribuut = "Impact", levels = "1 = bedrijfsfocus op maatschappelijke impact en leden, 0 = focus alleen op leden" , variabele = "impact")
  att_table = rbind(att_table, tab_att4)
}
if(newdata[row,]$att5 > 0){
  tab_att5 = data.table(attribuut = "Percentage groene energie", levels = "1 = 1 = 0%, 2 = 33.3%, 3 = 66.6%, 4 = 100%", variabele = "groene.energie0/33/66/100")
  att_table = rbind(att_table, tab_att5)
}
if(newdata[row,]$att6 > 0){
  tab_att6 = data.table(attribuut = "Prijs per kWh", levels = "1 = 20 cent, 2 = 25 cent, 3 = 30 cent, 4 = 35 cent" , variabele = "kwhprijs20/25/30/35")
  att_table = rbind(att_table, tab_att6)
}
if(newdata[row,]$att7 > 0){
  tab_att7 = data.table(attribuut = "Winst", levels = "1 = verdelen als dividend onder leden, 2 = investeren in organisatie, 3 = investeren in maatschappelijke projecten", variabele = "winst.naarleden/naarorganisatie/naarmaatschappij")
  att_table = rbind(att_table, tab_att7)
}
if(newdata[row,]$att8 > 0){
  tab_att8 = data.table(attribuut = "Hoeveelheid leden", levels = "1 = gelimiteerd, 0 = ongelimiteerd" , variabele = "limiet.leden")
  att_table = rbind(att_table, tab_att8)
}
if(newdata[row,]$att9 > 0){
  tab_att9 = data.table(attribuut = "Afstand tot hoofdkwartier van het burgercollectief", levels = "1 = lokaal (0-10 km), 2 = regionaal (10-30 km), 3 = supraregionaal (>30 km)", variabele = "afstand.hoofdkwartier.lokaal/regionaal/supraregionaal")
  att_table = rbind(att_table, tab_att9)
}
if(newdata[row,]$att10 > 0){
  tab_att10 = data.table(attribuut = "Transparantie van prijsvaststelling", levels = "1 = volledige transparantie, 0 = wettelijke transparantie", variabele = "transparantie.prijs")
  att_table = rbind(att_table, tab_att10)
}
if(newdata[row,]$att11 > 0){
  tab_att11 = data.table(attribuut = "Verplichte kosten bij lidmaatschap", levels = "1 = verplichte kosten om lid te worden, 0 = geen verplichte kosten", variabele = "verplichte.kosten")
  att_table = rbind(att_table, tab_att11)
}
if(newdata[row,]$att12 > 0){
  tab_att12 = data.table(attribuut = "Aantal sociale events voor leden (naast ALV)", levels = "1 = geen, 2 = maandelijks, 3 = jaarlijks" , variabele = "sociale.eventsgeen/maandelijks/jaarlijks")
  att_table = rbind(att_table, tab_att12)
}
if(newdata[row,]$att13 > 0){
  tab_att13 = data.table(attribuut = "Diversificatie van producten naast elektriciteit", levels = "1 = geen diversificatie, 2 = enige diversificatie, 3 = veel diversificatie" , variabele = "diversificatie.geen/enige/veel")
  att_table = rbind(att_table, tab_att13)
}
if(newdata[row,]$att14 > 0){
  tab_att14 = data.table(attribuut = "Energie levering", levels = "1 = eigen energielevering, 0 = via netbeheerder", variabele = "eigen.energielevering")
  att_table = rbind(att_table, tab_att14)
}
if(newdata[row,]$att15 > 0){
  tab_att15 = data.table(attribuut = "Advies vanuit de organisatie over installaties en energiezuinig wonen", levels = "1 = individueel advies, 0 = alleen algemeen advies", variabele = "individueel.advies")
  att_table = rbind(att_table, tab_att15)
}
#if(newdata[row,]$att16 > 0){
#tab_att16 = data.table(attribuut = "Wekelijke kosten", levels = "1 = 0-10 euro, 2 = 10-30 euro, 3 = 30-50 euro", variabele = "weekkosten.010/1030/3050")
#att_table = rbind(att_table, tab_att16)
#}
if(newdata[row,]$att17 > 0){
  tab_att17 = data.table(attribuut = "Manier waarop voedsel verkregen wordt", levels = "1 = bezorgservice of zelf ophalen, 0 = alleen zelf ophalen", variabele = "voedsel.bezorgen")
  att_table = rbind(att_table, tab_att17)
}
if(newdata[row,]$att18 > 0){
  tab_att18 = data.table(attribuut = "Wat gebeurt er bij een tekort", levels = "1 = externe inkoop, 0 = geen externe inkoop", variabele = "externe.inkoop")
  att_table = rbind(att_table, tab_att18)
}
if(newdata[row,]$att19 > 0){
  tab_att19 = data.table(attribuut = "Uniformiteit van filialen", levels = "1 = alle filialen hebben hetzelfde organisatiemodel, 0 = filialen hebben autonomie over de organisatorische inrichting", variabele = "uniformiteit")
  att_table = rbind(att_table, tab_att19)
}
if(newdata[row,]$att20 > 0){
  tab_att20 = data.table(attribuut = "Type lidmaatschap", levels = "1 = alleen producent, 2 = alleen consument, 3 = prosumer (producent en consument)", variabele = "lidmaatschap.consument/producent/prosumer")
  att_table = rbind(att_table, tab_att20)
}
if(newdata[row,]$att21 > 0){
  tab_att21 = data.table(attribuut = "Voedsel beschikbaarheid", levels = "1 = het hele jaar rond, 0 = in bepaalde delen van het jaar", variabele = "beschikbaarheid")
  att_table = rbind(att_table, tab_att21)
}
if(newdata[row,]$att22 > 0){
  tab_att22 = data.table(attribuut = "Onderhoud en productie", levels = "1 = verplichte participatie van leden, 0 = vrijwillige participatie van leden", variabele = "bijdrage.onderhoudproductie")
  att_table = rbind(att_table, tab_att22)
}
if(newdata[row,]$att23 > 0){
  tab_att23 = data.table(attribuut = "Lengte voedselketen", levels = "1 = korte keten met lokale producten, 0 = langere keten, niet altijd lokale producten", variabele = "lengte.voedselketen")
  att_table = rbind(att_table, tab_att23)
}
if(newdata[row,]$att24 > 0){
  tab_att24 = data.table(attribuut = "Biologisch voedsel", levels = "1 = biologisch voedsel, 0 = geen biologisch voedsel", variabele = "biologisch.voedsel")
  att_table = rbind(att_table, tab_att24)
}
if(newdata[row,]$att25 > 0){
  tab_att25 = data.table(attribuut = "De uitvoering van zorgg", levels = "1 = zorgprofessionals in vaste dienst van de organisatie, 2 = zorgprofessionals in vaste dienst en taken uitgevoerd door leden, 3 = alleen uitgevoerd door leden", variabele = "uitvoeringprofessionals/professionals.leden/leden")
  att_table = rbind(att_table, tab_att25)
}
if(newdata[row,]$att26 > 0){
  tab_att26 = data.table(attribuut = "Doelgroep", levels = "1 = ouderen, 2 = mensen met een beperking, 3 = geen specifieke doelgroep" , variabele = "doelgroepouderen/handicap/geen")
  att_table = rbind(att_table, tab_att26)
}
if(newdata[row,]$att27 > 0){
  tab_att27 = data.table(attribuut = "Condities voor het ontvangen van hulp", levels = "1 = geen specifieke eisen, 2 = éénmalige vergoeding om hulp te ontvangen, 3 = bepaald aantal jaren lidmaatschap" , variabele = "conditiesgeen/vergoeding/jarenlid")
  att_table = rbind(att_table, tab_att27)
}
if(newdata[row,]$att28 > 0){
  tab_att28 = data.table(attribuut = "Zorgtaken", levels = "1 = worden uitbesteed aan zorg providers, 0 = worden gedaan door de organisatie zelf" , variabele = "zorgtaken.zelf")
  att_table = rbind(att_table, tab_att28)
}
if(newdata[row,]$att29 > 0){
  tab_att29 = data.table(attribuut = "Lidmaatschap voorwaarden", levels = "1 = selectieve procedure om lid te worden, 0 = iedereen kan lid worden", variabele = "selectieve,voorwaarden")
  att_table = rbind(att_table, tab_att29)
}


#coding --> put everything to D for dummy, as there are no real continuous vars except for price per kwh
x = rep("D", times=length(levels))

#means --> put to 0 as we have no priors. length is sum of levels -1
y = rep(0, times = sum(levels-1))

#Create orthogonal array 
set.seed(123)
lvls = levels
coding = x
m = y
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 500, mu = m, Sigma = v)
DKE_design <- CEA(lvls = lvls, coding = coding, n.alts = 2, #the number of alternatives is always 2 in our design
                  n.sets = 14, par.draws = ps,   #the number of choice sets is always 14 in our design 
                  best = TRUE)



#make list of attributes that need to be represented
lvls_name = list()
namelist = list()
if (newdata[row,]$att1 >0) {
  lvls_name = append(lvls_name, list(att1))
  namelist = append(namelist, att1_name)
}
if (newdata[row,]$att2 >0) {
  lvls_name = append(lvls_name, list(att2))
  namelist = append(namelist, att2_name)
}
if (newdata[row,]$att3 >0) {
  lvls_name = append(lvls_name, list(att3))
  namelist = append(namelist, att3_name)
}
if (newdata[row,]$att4 >0) {
  lvls_name = append(lvls_name, list(att4))
  namelist = append(namelist, att4_name)
}
if (newdata[row,]$att5 >0) {
  lvls_name = append(lvls_name, list(att5))
  namelist = append(namelist, att5_name)
}
if (newdata[row,]$att6 >0) {
  lvls_name = append(lvls_name, list(att6))
  namelist = append(namelist, att6_name)
}
if (newdata[row,]$att7 >0) {
  lvls_name = append(lvls_name, list(att7))
  namelist = append(namelist, att7_name)
}
if (newdata[row,]$att8 >0) {
  lvls_name = append(lvls_name, list(att8))
  namelist = append(namelist, att8_name)
}
if (newdata[row,]$att9 >0) {
  lvls_name = append(lvls_name, list(att9))
  namelist = append(namelist, att9_name)
}
if (newdata[row,]$att10 >0) {
  lvls_name = append(lvls_name, list(att10))
  namelist = append(namelist, att10_name)
}
if (newdata[row,]$att11 >0) {
  lvls_name = append(lvls_name, list(att11))
  namelist = append(namelist, att11_name)
}
if (newdata[row,]$att12 >0) {
  lvls_name = append(lvls_name, list(att12))
  namelist = append(namelist, att12_name)
}
if (newdata[row,]$att13 >0) {
  lvls_name = append(lvls_name, list(att13))
  namelist = append(namelist, att13_name)
}
if (newdata[row,]$att14 >0) {
  lvls_name = append(lvls_name, list(att14))
  namelist = append(namelist, att14_name)
}
if (newdata[row,]$att15 >0) {
  lvls_name = append(lvls_name, list(att15))
  namelist = append(namelist, att15_name)
}
#if (newdata[row,]$att16 >0) {
#  lvls_name = append(lvls_name, list(att16))
#  namelist = append(namelist, att16_name)
#}
if (newdata[row,]$att17 >0) {
  lvls_name = append(lvls_name, list(att17))
  namelist = append(namelist, att17_name)
}
if (newdata[row,]$att18 >0) {
  lvls_name = append(lvls_name, list(att18))
  namelist = append(namelist, att18_name)
}
if (newdata[row,]$att19 >0) {
  lvls_name = append(lvls_name, list(att19))
  namelist = append(namelist, att19_name)
}
if (newdata[row,]$att20 >0) {
  lvls_name = append(lvls_name, list(att20))
  namelist = append(namelist, att20_name)
}
if (newdata[row,]$att21 >0) {
  lvls_name = append(lvls_name, list(att21))
  namelist = append(namelist, att21_name)
}
if (newdata[row,]$att22 >0) {
  lvls_name = append(lvls_name, list(att22))
  namelist = append(namelist, att22_name)
}
if (newdata[row,]$att23 >0) {
  lvls_name = append(lvls_name, list(att23))
  namelist = append(namelist, att23_name)
}
if (newdata[row,]$att24 >0) {
  lvls_name = append(lvls_name, list(att24))
  namelist = append(namelist, att24_name)
}
if (newdata[row,]$att25 >0) {
  lvls_name = append(lvls_name, list(att25))
  namelist = append(namelist, att25_name)
}
if (newdata[row,]$att26 >0) {
  lvls_name = append(lvls_name, list(att26))
  namelist = append(namelist, att26_name)
}
if (newdata[row,]$att27 >0) {
  lvls_name = append(lvls_name, list(att27))
  namelist = append(namelist, att27_name)
}
if (newdata[row,]$att28 >0) {
  lvls_name = append(lvls_name, list(att28))
  namelist = append(namelist, att28_name)
}
if (newdata[row,]$att29 >0) {
  lvls_name = append(lvls_name, list(att29))
  namelist = append(namelist, att29_name)
}


#Decode 
DKE_design.oa <- Decode(des = DKE_design$design, lvl.names = lvls_name, n.alts = 2,
                        coding = coding)
DKE_design.oa$design
colnames(DKE_design.oa$design) = namelist


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#export fully named OA as CSV. This is the file that the Python code needs to run 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
write.csv(DKE_design.oa$design, "[yourpath]/dce_questions.csv")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# For the analysis later it is important that you have a nicer file with the orthogonal
# array. You will make that by running the code below
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#import csv file with orthogonal array that you created above 
oa_csv <- read.csv("[yourpath]/dce_questions.csv")

#set simpler names
colnames(oa_csv) = ifelse(colnames(oa_csv) == "Participatie.van.leden.in.besluitvorming", "participatie", 
                          ifelse(colnames(oa_csv) == "Stemrecht.van.leden.in.de.besluitvorming", "democratisch.stemrecht", 
                          ifelse(colnames(oa_csv) == "Evolutie.en.groei", "evolutie", 
                          ifelse(colnames(oa_csv) == "Impact", "impact", 
                          ifelse(colnames(oa_csv) == "Percentage.groene.energie", "groene.energie", 
                          ifelse(colnames(oa_csv) == "Prijs.per.kWh..in.centen.", "kwhprijs.lin", 
                          ifelse(colnames(oa_csv) == "Winst", "winst.doel",
                          ifelse(colnames(oa_csv) == "Aantal.sociale.events.voor.leden..naast.ALV.", "sociale.events",
                          ifelse(colnames(oa_csv) == "Hoeveelheid.leden", "limiet.leden", 
                          ifelse(colnames(oa_csv) == "Afstand.tot.hoofdkwartier.van.het.burgercollectief", "afstand.hoofdkwartier",
                          ifelse(colnames(oa_csv) == "Transparantie.van.prijsvaststelling", "transparantie.prijs", 
                          ifelse(colnames(oa_csv) == "Verplichte.kosten.bij.lidmaatschap", "verplichte.kosten", 
                          ifelse(colnames(oa_csv) == "Diversificatie.van.producten.naast.elektriciteit", "diversificatie",
                          ifelse(colnames(oa_csv) == "Energie.levering", "eigen.energielevering", 
                          ifelse(colnames(oa_csv) == "Advies.vanuit.de.organisatie.over.installaties.en.energiezuinig.wonen", "individueel.advies",
                          #ifelse(colnames(oa_csv) == "Wekelijke.kosten", "weekkosten",
                          ifelse(colnames(oa_csv) == "Manier.waarop.voedsel.verkregen.wordt", "voedsel.bezorgen", 
                          ifelse(colnames(oa_csv) == "Wat.gebeurt.er.bij.een.tekort", "externe.inkoop", 
                          ifelse(colnames(oa_csv) == "Uniformiteit.van.filialen", "uniformiteit", 
                          ifelse(colnames(oa_csv) == "Type.lidmaatschap", "lidmaatschap", 
                          ifelse(colnames(oa_csv) == "Voedsel.beschikbaarheid", "beschikbaarheid", 
                          ifelse(colnames(oa_csv) == "Onderhoud.en.productie", "bijdrage.onderhoudproductie", 
                          ifelse(colnames(oa_csv) == "De.uitvoering.van.zorg", "uitvoering", 
                          ifelse(colnames(oa_csv) == "Doelgroep", "doelgroep", 
                          ifelse(colnames(oa_csv) == "Condities.voor.het.ontvangen.van.hulp", "condities", 
                          ifelse(colnames(oa_csv) == "Zorgtaken", "zorgtaken.zelf", 
                          ifelse(colnames(oa_csv) == "Lidmaatschap.voorwaarden", "selectieve.voorwaarden", "X" ))))))))))))))))))))))))))

#set values to 1's and 0's 
#standard included
oa_csv$participatie = ifelse(oa_csv$participatie == "Mogelijk", 1,0)
oa_csv$democratisch.stemrecht = ifelse(oa_csv$democratisch.stemrecht == "Een lid een stem", 1, 0)
oa_csv$evolutie = ifelse(oa_csv$evolutie == "Uitbreiding en groei", 1, 0)


if ("impact" %in%   colnames(oa_csv))
{oa_csv$impact                        = ifelse(oa_csv$impact == "Bedrijfsfocus op maatschappelijke impact en leden", 1, 0)}
if ("groene.energie" %in%   colnames(oa_csv))
{oa_csv$groene.energie                = ifelse(oa_csv$groene.energie == "0%", 1, 
                                               ifelse(oa_csv$groene.energie == "33.3%", 2, 
                                                      ifelse(oa_csv$groene.energie == "66.6%", 3, 
                                                             ifelse(oa_csv$groene.energie == "100%", 4, NA))))

oa_csv$groene.energie0 = ifelse(oa_csv$groene.energie == 1, 1, 0)
oa_csv$groene.energie33 = ifelse(oa_csv$groene.energie == 2, 1, 0)
oa_csv$groene.energie66 = ifelse(oa_csv$groene.energie == 3, 1, 0)
oa_csv$groene.energie100 = ifelse(oa_csv$groene.energie == 4, 1, 0)
}
if ("winst.doel" %in%   colnames(oa_csv))
{oa_csv$winst.doel               = ifelse(oa_csv$winst.doel == "verdelen als dividend", 1,
                                          ifelse(oa_csv$winst.doel == "investeren in de organisatie", 2, 3))

oa_csv$winst.naarleden = ifelse(oa_csv$winst.doel == 1, 1, 0)
oa_csv$winst.naarorganisatie= ifelse(oa_csv$winst.doel == 2, 1, 0)
oa_csv$winst.naarmaatschappij = ifelse(oa_csv$winst.doel == 3, 1, 0)
}

if ("sociale.events" %in%   colnames(oa_csv))
{oa_csv$sociale.events                = ifelse(oa_csv$sociale.events == "geen", 1, 
                                               ifelse(oa_csv$sociale.events == "maandelijks", 2, 3))
oa_csv$sociale.eventsgeen             = ifelse(oa_csv$sociale.events == 1, 1, 0)
oa_csv$sociale.eventsmaandelijks      = ifelse(oa_csv$sociale.events == 2, 1, 0)
oa_csv$sociale.eventsjaarlijks        = ifelse(oa_csv$sociale.events == 3, 1, 0)
}
if ("limiet.leden" %in%   colnames(oa_csv))
{oa_csv$limiet.leden                  = ifelse(oa_csv$limiet.leden == "gelimiteerd",1, 0)}
if ("afstand.hoofdkwartier" %in%   colnames(oa_csv))
{oa_csv$afstand.hoofdkwartier         = ifelse(oa_csv$afstand.hoofdkwartier == "lokaal (0-10 km)", 1, 
                                               ifelse(oa_csv$afstand.hoofdkwartier == "regionaal (10-30 km)", 2, 3))

oa_csv$afstand.hoofdkwartier.lokaal   = ifelse(oa_csv$afstand.hoofdkwartier == 1, 1, 0)
oa_csv$afstand.hoofdkwartier.regionaal  = ifelse(oa_csv$afstand.hoofdkwartier == 2, 1, 0)
oa_csv$afstand.hoofdkwartier.supraregio = ifelse(oa_csv$afstand.hoofdkwartier == 3, 1, 0)
}
if ("transparantie.prijs" %in%   colnames(oa_csv))
{oa_csv$transparantie.prijs           = ifelse(oa_csv$transparantie.prijs == "volledige transparantie", 1,0)}
if ("verplichte.kosten" %in%   colnames(oa_csv))
{oa_csv$verplichte.kosten             = ifelse(oa_csv$verplichte.kosten == "verplichte kosten om lid te worden", 1, 0)}
if ("diversificatie" %in%   colnames(oa_csv))
{oa_csv$diversificatie                = ifelse(oa_csv$diversificatie == "Geen diversificatie", 1,
                                               ifelse(oa_csv$diversificatie == "Enige diversificatie", 2, 3))
oa_csv$diversificatie.geen            = ifelse(oa_csv$diversificatie == 1, 1, 0)
oa_csv$diversificatie.enige           = ifelse(oa_csv$diversificatie == 2, 1, 0)
oa_csv$diversificatie.veel            = ifelse(oa_csv$diversificatie == 3, 1, 0)
}
if ("eigen.energielevering" %in%   colnames(oa_csv))
{oa_csv$eigen.energielevering         = ifelse(oa_csv$eigen.energielevering == "via netbeheerder", 0, 1)}
if ("individueel.advies" %in%   colnames(oa_csv))
{oa_csv$individueel.advies            = ifelse(oa_csv$individueel.advies == "individueel advies", 1, 0)}
#if ("weekkosten" %in%   colnames(oa_csv))
#{oa_csv$weekkosten                    = ifelse(oa_csv$weekkosten == "0-10 euro", 1, 
#                                               ifelse(oa_csv$weekkosten == "10-30 euro", 2, 3))
#oa_csv$weekkosten.010                 = ifelse(oa_csv$weekkosten == 1, 1, 0)
#oa_csv$weekkosten.1030                = ifelse(oa_csv$weekkosten == 2, 1, 0)
#oa_csv$weekkosten.3050                = ifelse(oa_csv$weekkosten == 3, 1, 0)
#}
if ("voedsel.bezorgen" %in%   colnames(oa_csv))
{oa_csv$voedsel.bezorgen              = ifelse(oa_csv$voedsel.bezorgen == "bezorgservice of zelf ophalen", 1, 0)}
if ("externe.inkoop" %in%   colnames(oa_csv))
{oa_csv$externe.inkoop                = ifelse(oa_csv$externe.inkoop == "externe inkoop",1, 0)}
if ("uniformiteit" %in%   colnames(oa_csv))
{oa_csv$uniformiteit                  = ifelse(oa_csv$uniformiteit == "alle filialen hebben hetzelfde organisatiemodel", 1, 0)}
if ("lidmaatschap" %in%   colnames(oa_csv))
{oa_csv$lidmaatschap                  = ifelse(oa_csv$lidmaatschap == "alleen producent", 1, 
                                               ifelse(oa_csv$lidmaatschap == "alleen consument", 2, 3))
oa_csv$lidmaatschapproducent          = ifelse(oa_csv$lidmaatschap == 1, 1, 0)
oa_csv$lidmaatschapconsument          = ifelse(oa_csv$lidmaatschap == 2, 1, 0)
oa_csv$lidmaatschapprosumer           = ifelse(oa_csv$lidmaatschap == 3, 1, 0)
}
if ("beschikbaarheid" %in%   colnames(oa_csv))
{oa_csv$beschikbaarheid               = ifelse(oa_csv$beschikbaarheid == "het hele jaar rond", 1, 0)}
if ("bijdrage.onderhoudproductie" %in%   colnames(oa_csv))
{oa_csv$bijdrage.onderhoudproductie   = ifelse(oa_csv$bijdrage.onderhoudproductie == "verplichte participatie van leden",1, 0)}
if ("uitvoering" %in%   colnames(oa_csv))
{oa_csv$uitvoering                    = ifelse(oa_csv$uitvoering == "zorgprofessionals in vaste dienst van de organisatie", 1,
                                               ifelse(oa_csv$uitvoering == "zorgprofessionals in vaste dienst en taken uitgevoerd door leden", 2, 3))
oa_csv$uitvoeringprofessionals        = ifelse(oa_csv$uitvoering == 1, 1, 0)
oa_csv$uitvoeringprofessionals.leden  = ifelse(oa_csv$uitvoering == 2, 1, 0)
oa_csv$uitvoeringleden                = ifelse(oa_csv$uitvoering == 3, 1, 0)
}
if ("doelgroep" %in%   colnames(oa_csv))
{oa_csv$doelgroep                     = ifelse(oa_csv$doelgroep == "ouderen", 1, 
                                               ifelse(oa_csv$doelgroep == "mensen met een beperking", 2, 3))
oa_csv$doelgroepouderen               = ifelse(oa_csv$doelgroep == 1, 1, 0)
oa_csv$doelgroephandicap              = ifelse(oa_csv$doelgroep == 2, 1, 0)
oa_csv$doelgroepgeen                  = ifelse(oa_csv$doelgroep == 3, 1, 0)
}
if ("condities" %in%   colnames(oa_csv))
{oa_csv$condities                     = ifelse(oa_csv$condities == "geen specifieke eisen", 1, 
                                               ifelse(oa_csv$condities == "éénmalige vergoeding om hulp te ontvangen", 2, 3))
oa_csv$conditiesgeen                  = ifelse(oa_csv$condities == 1, 1, 0)
oa_csv$conditiesvergoeding            = ifelse(oa_csv$condities == 2, 1, 0)
oa_csv$conditiesjarenlid              = ifelse(oa_csv$condities == 3, 1, 0)
}
if ("zorgtaken.zelf" %in%   colnames(oa_csv))
{oa_csv$zorgtaken.zelf                     = ifelse(oa_csv$zorgtaken.zelf == "worden gedaan door de organisatie zelf", 0, 1)}
if ("selectieve.voorwaarden" %in%   colnames(oa_csv))
{oa_csv$selectieve.voorwaarden        = ifelse(oa_csv$selectieve.voorwaarden == "selectieve procedure om lid te worden", 1, 0)}


#make separate variables for the categorical (>2 categories) and money (in cents) variables 
if ("kwhprijs.lin" %in%   colnames(oa_csv)){
  oa_csv$kwhprijs20 = ifelse(oa_csv$kwhprijs.lin == 20, 1, 0)
  oa_csv$kwhprijs25 = ifelse(oa_csv$kwhprijs.lin == 25, 1, 0)
  oa_csv$kwhprijs30 = ifelse(oa_csv$kwhprijs.lin == 30, 1, 0)
  oa_csv$kwhprijs35 = ifelse(oa_csv$kwhprijs.lin == 35, 1, 0)
}


#add set number 
oa_csv$set = ifelse(grepl("set1.alt", oa_csv$X), 1, 
                    ifelse(grepl("set2.alt", oa_csv$X), 2,
                           ifelse(grepl("set3.alt", oa_csv$X), 3, 
                                  ifelse(grepl("set4.alt", oa_csv$X), 4, 
                                         ifelse(grepl("set5.alt", oa_csv$X), 5, 
                                                ifelse(grepl("set6.alt", oa_csv$X), 6, 
                                                       ifelse(grepl("set7.alt", oa_csv$X),7,
                                                              ifelse(grepl("set8.alt", oa_csv$X), 8, 
                                                                     ifelse(grepl("set9.alt", oa_csv$X), 9,
                                                                            ifelse(grepl("set10.alt", oa_csv$X), 10, 
                                                                                   ifelse(grepl("set11.alt", oa_csv$X), 11,
                                                                                          ifelse(grepl("set12.alt", oa_csv$X), 12, 
                                                                                                 ifelse(grepl("set13.alt", oa_csv$X), 13, 14)))))))))))))
#alternative number
oa_csv$alt = ifelse(grepl("alt1", oa_csv$X),"A", "B")

#remove X var
oa_csv = subset(oa_csv, select= -c(X))

#reshape
oa_reshape = reshape(oa_csv, idvar = "set", timevar = "alt", direction = "wide")

#rename and save as csv
ort_array  = oa_reshape
colnames(ort_array) = gsub(".A", "_A", colnames(ort_array) )
colnames(ort_array) = gsub(".B", "_B", colnames(ort_array) )

write.xlsx(ort_array, "[yourpath]/ort_array.xlsx")



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Now go to the Python code designed for creating a Qualtrics DCE survey
# using the csv file that you just created 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------