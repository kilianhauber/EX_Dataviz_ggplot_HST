## ----setup, include=FALSE----------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(fontawesome)
library(glue)

library(ggplot2)
library(janitor)
library(forcats)
library(dplyr)
library(RColorBrewer)
library(svglite)
library(patchwork)
library(ggthemes)


# Assignment ggplot-Intro

# A1 -----

## Gruppe ----

group <- "HST"  #(Anfangsbuchstaben Nachnamen)

## Names of Members -----

#Kilian Hauber
#Danis Sisic
#Tim Trefzer

## Date -----
#Bearbeitungsdatum angeben
#30.07.2024 




# A2 -----
## Define in-file ----
my_in_file <- 'workshop_data.xlsx'
## Data import -----
tbl_wshop <- read_excel(xfun::from_root('data',my_in_file))


## c) Fehlende Werte ----
tbl_wshop <- tbl_wshop %>%
  mutate_all(list(~ ifelse(. == 'NA', NA, .)))



## glimpse ----
glimpse(tbl_wshop)

# A3 ------
#Define ISBA Colors 
ISBAblu <- "#232461"
ISBAred <- "#D84117"


# A4 ------

## A4 a) --------
### Create plot-Object ----
p <- mtcars %>% 
              ggplot()
### Print plot -----
p

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4a_emptyplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



## A4b -------
### create and print plot ----

p4b <- tbl_wshop %>% 
  ggplot(aes(x = workshop)) +
  geom_bar() +
  theme_grey() 

p4b


### save plot as pdf ----
date <- Sys.Date()
fig_name <- glue("A4b_barplot_{group}_{date}.pdf")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")




## A4c -------
### create and print plot ----

p4c <- tbl_wshop %>% 
  ggplot(aes(y = workshop)) +
  geom_bar() +
  theme_grey()

p4c

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4c_barplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



## A4d -------
### create and print plot ----
p4d <- tbl_wshop %>% 
  count(workshop) %>%
  mutate(workshop = fct_reorder(workshop, n, .desc = FALSE)) %>%. #reorder
  ggplot(aes(x = n, y = workshop)) +
  geom_bar(stat = "identity") +
  labs(x = "count", y = "workshop") +
  theme_grey()

p4d

### save plot as png ----

date <- Sys.Date()
fig_name <- glue("A4d_barplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")




## A4e -------
### create and print plot ----
p4e <- tbl_wshop %>% 
  count(workshop) %>%
  mutate(workshop = fct_reorder(workshop, n, .desc = FALSE)) %>%
  ggplot(aes(x = n, y = workshop)) +
  geom_bar(stat = "identity", fill = ISBAblu, color = ISBAred) +
  labs(x = "count", y = "workshop") +
  theme_grey()

p4e

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4e_barplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")




## A4f -------
### create and print plot ----
p4f <- tbl_wshop %>% 
  count(workshop) %>%
  mutate(workshop = fct_reorder(workshop, n, .desc = FALSE)) %>%
  ggplot(aes(x = n, y = workshop)) +
  geom_bar(stat = "identity", fill = ISBAblu, color = ISBAred) +
  labs(x = "absolute Häufigkeit", y = "Workshop") +
  theme_grey()

p4f

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4f_barplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")


## A4g -------
### create and print plot ----
p4g <- tbl_wshop %>% 
  count(workshop) %>%
  mutate(workshop = fct_reorder(workshop, n, .desc = FALSE)) %>%
  ggplot(aes(x = n, y = workshop)) +
  geom_bar(stat = "identity", fill = ISBAblu) +
  labs(x = "absolute Häufigkeit", y = "Workshop", title = "Beliebte Software Workshops", caption='Datenquelle: Frei erfunden.') +
  theme_light() +
  theme(plot.caption = element_text(hjust = 0)) # caption linksbündig

p4g

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4g_barplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")




# A5 -----
## create plot -----
p5 <- tbl_wshop %>% 
  ggplot(aes(x = workshop, fill = workshop)) +
  geom_bar()

p5

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A5_coloredbars_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A6 
## create plot -----

# Häufigkeit der Workshop-Kategorien berechnen
workshop_counts <- tbl_wshop %>%
  count(workshop)

# Gestapeltes Balkendiagramm erstellen
p6 <- ggplot(workshop_counts, aes(x = "", y = n, fill = workshop)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "relative Häufigkeit", fill = "Workshop") +
  theme_minimal()

p6

### save plot as pdf ----
date <- Sys.Date()
fig_name <- glue("A6_stackedbar_{group}_{date}.pdf")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")


# A7 ----
RColorBrewer::display.brewer.all()
#TODO: Webseite: www.colorbrewer2.org

## plot erzeugen -----

p7 <- ggplot(workshop_counts, aes(x = "", y = n, fill = workshop)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "relative Häufigkeit", fill = "Workshop") +
  scale_fill_brewer(type = "qual") +
  theme_minimal()

p7

 
### save plot as svg ----
date <- Sys.Date()
fig_name <- glue("A7_stackedbar_{group}_{date}.svg")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A8-----
## create plot -----

### Mit NA
p8 <- tbl_wshop %>% 
   janitor::tabyl(workshop) %>% 
      ggplot(aes(x=fct_rev(fct_infreq(workshop)),y=n)) +
  geom_segment(aes(x = workshop, xend = workshop, y = 0, yend = n), color = "black") +
  geom_point(size = 2, color = "black") +
  labs(x = "Workshop", y = "absolute Häufigkeit")

### Ohne NA
p8 <- tbl_wshop %>% 
  filter(!is.na(workshop)) %>% 
  janitor::tabyl(workshop) %>% 
  ggplot(aes(x=fct_rev(fct_infreq(workshop)),y=n)) +
  geom_segment(aes(x = workshop, xend = workshop, y = 0, yend = n), color = "black") +
  geom_point(size = 2, color = "black") +
  labs(x = "Workshop", y = "absolute Häufigkeit")

### print plot -----
p8

## save plot as pdf ----
date <- Sys.Date()
fig_name <- glue("A8_lollipop_{group}_{date}.pdf")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A9 -----

## Plot erzeugen -----

# NA-Werte herausfiltern und Häufigkeit der Geschlechter berechnen
gender_counts <- tbl_wshop %>% 
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Farben festlegen
colors <- c("Female" = ISBAred, "Male" = ISBAblu)

# Kreisdiagramm erstellen
p9 <- ggplot(gender_counts, aes(x = "", y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = colors, labels = c("weiblich", "männlich")) +
  labs(fill = "Geschlecht") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white")

p9


## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A9_piechart_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A10 -----

## Plot 1 -----
p10_1 <- tbl_wshop %>% 
        ggplot(aes(posttest)) +
        geom_histogram(aes(y=after_stat(density)), fill = ISBAblu, bins = 21) +
        geom_density(adjust=0.5, color = ISBAred) +
        labs(y="Dichte",x="") +
        theme_light()

## Plot 2 -----
p10_2 <- tbl_wshop %>% 
        ggplot(aes(posttest)) +
        geom_histogram(aes(y=after_stat(density)), fill = ISBAblu, bins = 11) +
        geom_density(adjust=1, color = ISBAred) +
        labs(y="Dichte",x="Testergebnis (Punkte)") +
        theme_light()


## Plot 3 -----
p10_3 <- tbl_wshop %>% 
        ggplot(aes(posttest)) +
        geom_histogram(aes(y=after_stat(density)), fill = ISBAblu, bins = 7) +
        geom_density(adjust=2.5, color = ISBAred) +
        labs(y="Dichte",x="") +
        theme_light()

p10_1 + p10_2 + p10_3


## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A10_histdens_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A 11------
## create plot -----
p11 <- tbl_wshop %>% 
       ggplot(aes(x="",y=posttest)) + 
       geom_boxplot() +
       labs(y = "Testergebnis (Punkte)", x = "") +
       theme_light()

p11

## save plot as svg ----
date <- Sys.Date()
fig_name <- glue("A11_boxplot_{group}_{date}.svg")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")


# A12 ----
## select and transform data ----
tbl_long <- tbl_wshop %>% 
                select(pretest,posttest) %>% 
                pivot_longer(everything(), names_to = "var") %>%
                mutate(var = recode(var, pretest = "vor Workshop", posttest = "nach Workshop"))

## create plot -----
p12 <- tbl_long %>% 
  ggplot(aes(x=var,y=value)) + 
  geom_boxplot() +
  labs(y = "Testergebnis (Punkte)", x = "Zeitpunkt des Tests") +
  theme_economist()

### print plot -----
p12

## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A12_boxplot_{group}_{date}.svg")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



