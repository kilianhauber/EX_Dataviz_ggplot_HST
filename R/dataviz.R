## ----setup, include=FALSE----------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(fontawesome)
library(glue)


# Assignment ggplot-Intro

# A1 -----

## Gruppe ----

group <- "XY"  # XYZ ANPASSEN (Anfangsbuchstaben Nachnamen)

## Names of Members -----

#NAME PERSON 1  # ANPASSEN 
#NAME PERSON 2  # ggf. ANPASSEN
#NAME PERSON 3  # ggf. ANPASSEN

## Date -----
#Bearbeitungsdatum angeben
#20.06.2023  #ANPASSEN!




# A2 -----
## Define in-file ----
my_in_file <- 'data_dummy.xlsx'
## Data import -----
tbl_wshop <- read_excel(xfun::from_root('data',my_in_file))


## c) Fehlende Werte ----
tbl_wshop <- tbl_wshop %>% 
                mutate_all(list(~ ifelse(. == 'NA', NA, .)))



## glimpse ----
glimpse(tbl_wshop)



# A3 ------
#Define ISBA Colors 
ISBAblu <- "#485"
ISBAred <- "#788"



# A4 ------

## A4 a) --------
### Create plot-Object ----
p <- mtcars %>% 
              ggplot()
### Print plot -----
p
### save plot as pdf ----
date <- Sys.Date()
fig_name <- glue("A4a_emptyplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



## A4b -------
### create and print plot ----

### save plot as pdf ----
date <- Sys.Date()
fig_name <- glue("A4b_barplot_{group}_{date}.pdf")




## A4c -------
### create and print plot ----

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4c_barplot_{group}_{date}.png")



## A4d -------
### create and print plot ----

### save plot as png ----




## A4e -------
### create and print plot ----

### save plot as png ----




## A4f -------
### create and print plot ----


### save plot as png ----



## A4g -------
### create and print plot ----
p + geom_bar(aes(y=hp)) +
  labs(title = "Beliebte Software Workshops", caption='Datenquelle: Frei erfunden.') +
  theme_light() +
  theme(plot.caption = element_text())
  

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A4g_barplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")




# A5 -----
## create plot -----
iris %>% 
  ggplot(aes(Species, fill = Species) ) + 
    geom_bar()

### save plot as png ----
date <- Sys.Date()
fig_name <- glue("A5_coloredbars_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A6 
## A6 a)
### create plot -----
 
### save plot as png ----
 


# A7 ----
## plot erzeugen -----
 
### save plot as png ----
 


# A8-----
## create plot -----
tbl_wshop %>% 
   janitor::tabyl(workshop) %>% 
      ggplot(aes(x=fct_rev(fct_infreq(workshop)),y=n)) +
        geom_point()


## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A8_lollipop_{group}_{date}.pdf")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")




# A9 -----

## Plot erzeugen -----
tbl_wshop %>%
   filter(!is.na(gender)) %>%
     ggplot(aes(x = factor(""), fill = gender)) +
     geom_bar() +
     geom_text(aes(label = paste0(round((after_stat(count)) / sum(after_stat(count)) * 100, 1), "%")),
             stat = "count", position = position_stack(vjust = 0.5),color='white') 
     
  
## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A9_piechart_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A10 -----

## Plot 1 -----
p1 <- tbl_wshop %>% 
       ggplot(aes(posttest)) +
       geom_histogram(aes(y=after_stat(density))) + 
       geom_density(adjust=.5) +
       labs(y="Dichte",x="") +
       theme_light()

## Plot 2 -----
p2 <- tbl_wshop %>% 
  ggplot(aes(posttest)) +
  geom_density(adjust=.5) +
  labs(y="Dichte",x="") +
  theme_light()


## Plot 3 -----
p3 <- tbl_wshop %>% 
        ggplot(aes(posttest)) +
        geom_histogram(aes(y=after_stat(density))) + 
        labs(y="Dichte",x="") +
        theme_light()




library(patchwork)
p1+p2+p3
## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A10_histdens_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")



# A 11------
## create plot -----
tbl_wshop %>% 
  ggplot(aes(x="",y=posttest)) + 
  geom_boxplot() 

## save plot as png ----
date <- Sys.Date()
fig_name <- glue("A11_boxplot_{group}_{date}.png")
ggsave(xfun::from_root("figs",fig_name),
       width = 18,  height = 10,  units = "cm")


# A12 ----
## select and transform data ----
tbl_long <- tbl_wshop %>% 
                select(pretest,posttest) %>% 
                pivot_longer(everything(), names_to = "var")
## create plot -----


## save plot as png ----


