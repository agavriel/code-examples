library(ggplot2)
library(XML)
library(data.table)
library(rvest)
library(reshape)
library(plyr)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(stringr)
library(magrittr)

#Scrape
ohseven <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2008_skaters.html", header = T, which = 1)
oheight <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2009_skaters.html", header = T, which = 1)
ohnine <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2010_skaters.html", header = T, which = 1)
ten <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2011_skaters.html", header = T, which = 1)
eleven <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2012_skaters.html", header = T, which = 1)
twelve <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2013_skaters.html", header = T, which = 1)
thirteen <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2014_skaters.html", header = T, which = 1)
fourteen <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2015_skaters.html", header = T, which = 1)
fifteen <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2016_skaters.html", header = T, which = 1)

#Add Season Variable
ohseven$Season <- 20072008
oheight$Season <- 20082009
ohnine$Season <- 20092010
ten$Season <- 20102011
eleven$Season <- 20112012
twelve$Season <- 20122013
thirteen$Season <- 20132014
fourteen$Season <- 20142015
fifteen$Season <- 20152016


#Pull Vars
vars <- c("Player", "Season", "Age")

#Subset
ohseven2 <- ohseven[vars]
oheight2 <- oheight[vars]
ohnine2 <- ohnine[vars]
ten2 <- ten[vars]
eleven2 <- eleven[vars]
twelve2 <- twelve[vars]
thirteen2 <- thirteen[vars]
fourteen2 <- fourteen[vars]
fifteen2 <- fifteen[vars]

#Bind

fullup <- dplyr::bind_rows(ohseven2, oheight2)
fullup <- dplyr::bind_rows(fullup, ohnine2)
fullup <- dplyr::bind_rows(fullup, ten2)
fullup <- dplyr::bind_rows(fullup, eleven2)
fullup <- dplyr::bind_rows(fullup, twelve2)
fullup <- dplyr::bind_rows(fullup, thirteen2)
fullup <- dplyr::bind_rows(fullup, fourteen2)
fullup <- dplyr::bind_rows(fullup, fifteen2)

#Clean
fullup <- subset(fullup, Age != "Age")