library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library("writexl")
library(shiny)

vstup <- c()
vyhodnoceni_ipa <- c()
vyhodnoceni <- c()
vector_ipa <- c("a" = "a", "á" = "aː", "b" = "b", "c" = "t͡s", "č" = "t͡ʃ", "d" = "d", "ď" = "ɟ", "e" = "ɛ", "é" = "ɛː", "ě" = "ě", "f" = "f", "g"= "ɡ", "h" = "ɦ", "ch" = "x", "i" = "ɪ", "í" = "iː", "j" = "j", "k" = "k", "l" = "l", "m" = "m", "n" = "n", "ň" = "ň", "o" = "o", "ó" = "oː", "p" = "p", "q" = "k", "r" = "r", "s" = "s", "š" = "ʃ", "t" = "t", "ť" = "c", "u" = "u", "ú" = "uː", "ů" = "uː", "v" = "v", "w" = "w", "x" = "ks", "y" = "ɪ", "ý" = "iː", "z" = "z", "ž" = "ʒ", "di" = "ɟɪ", "dí" = "ɟiː", "dě" = "ɟɛ", "ti" = "cɪ", "tí" = "ciː", "tě" = "cɛ", "ni" = "ɲɪ", "ní" = "ɲiː", "ně" = "ɲɛ", "mě" = "mɲɛ", "bě" = "bjɛ", "pě" = "pjɛ", "vě" = "vjɛ", "ts" = "t͡s", "dz" = "d͡z", "ie" = "ɪjɛ", "ia" = "ɪja", "io" = "ɪjo", "ř" = "r̝")


vector_temp <- c("a" = "a", "á" = "á", "b" = "b", "c" = "c", "č" = "č", "d" = "d", "ď" = "ď", "e" = "e", "é" = "é", "ě" = "ě", "f" = "f", "g"= "g", "h" = "h", "ch" = "ch", "i" = "i", "í" = "í", "j" = "j", "k" = "k", "l" = "l", "m" = "m", "n" = "n", "ň" = "ň", "o" = "o", "ó" = "ó", "p" = "p", "q" = "q", "r" = "r", "ř" = "ř", "s" = "s", "š" = "š", "t" = "t", "ť" = "ť", "u" = "u", "ú" = "ú", "ů" = "ů", "v" = "v", "w" = "w", "x" = "x", "y" = "y", "ý" = "ý", "z" = "z", "ž" = "ž", "di" = "di", "dí" = "dí", "dě" = "dě", "ti" = "ti", "tí" = "tí", "tě" = "tě", "ni" = "ni", "ní" = "ní", "ně" = "ně", "mě" = "mě", "bě" = "bě", "pě" = "pě", "vě" = "vě", "dz" = "dz", "ts" = "ts", "ie" = "ie", "ia" = "ia", "io" = "io")
vector_paired_consonants <- c("b" = "p", "d" = "t", "ď" = "ť", "g" = "k", "v" = "f", "z" = "s", "ž" = "š", "ch" = "h", "dz" = "c", "dž" = "č", "p" = "b", "t" = "d", "ť" = "ď", "k" = "g", "f" = "v", "s" = "z", "š" = "ž", "h" = "ch", "c" = "dz", "č" = "dž")
vector_paired_unvoiced_consonants <- c("p" = "p", "t" = "t", "ť" = "ť", "k" = "k", "f" = "f", "s" = "s", "š" = "š", "ch" = "ch",
                                       "c" = "c", "č" = "č")
vector_paired_voiced_consonants <- c("b" = "b", "d" = "d", "ď" = "ď", "g" = "g", "v" = "v", "z" = "z", "ž" = "ž", "dz" = "dz", "dž" = "dž")
vector_dtn <- c("d" = "d", "t" = "t", "n" = "n")
vector_dtn_vocal <- c("í" = "í", "i" = "i", "ě" = "ě")
vector_mbpv <- c("m" = "m", "b" = "b", "p" = "p", "v" = "v")
vector_mbpv_vocal <- c("ě" = "ě")
vector_ch_first <- c("c" = "c")
vector_ch_second <- c("h" = "h")
vector_ts_first <- c("t" = "t")
vector_ts_second <- c("s" = "s")
vector_dz_first <-c("d" = "d")
vector_dz_second <-c("z" = "z")
vector_ieiaio_first <- c("i" = "i")
vector_ieiaio_second <- c("e" = "e", "a" = "a", "o" = "o")


shinyServer(function(input, output) {
  
  output$vyhodnoceni_ipa <- renderUI({

    vstup <- input$vstup
    vstup <- tolower(vstup)
    vstup_split <- split(vstup, unique(vstup))
    vstup_split <- unlist(strsplit(vstup, ""))

    #for removing previous searches
    vyhodnoceni <- c()
    vyhodnoceni_ipa <- c()
    # adding each letter as an element of vector vyhodnoceni
    for (i in seq_along(vstup_split)) {
      x <- vstup_split[i]
      vyhodnoceni <- append(vyhodnoceni, vector_temp[x])
    }
    
    #loating i followed by e, a, o // e.g. filosofie, nokia, rio 
    location <- which(vyhodnoceni %in% vector_ieiaio_first)
    if (length(location) > 0) {
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_ieiaio_second[z]) == FALSE){
          vyhodnoceni[x] <- paste(vyhodnoceni[x], vyhodnoceni[y], sep = "", collapse = "")
          vyhodnoceni[y] <- NA
        }
      }
    }
    #locating d followed by z for d͡z // e.g. odzbrojit
    location <- which(vyhodnoceni %in% vector_dz_first)
    if (length(location) > 0) {
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_dz_second[z]) == FALSE){
          vyhodnoceni[x] <- paste(vyhodnoceni[x], vyhodnoceni[y], sep = "", collapse = "")
          vyhodnoceni[y] <- NA
        }
      }
    }
    # locating t followed by s for t͡s // e.g. tsar
    location <- which(vyhodnoceni %in% vector_ts_first)
    if (length(location) > 0) {
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_ts_second[z]) == FALSE){
          vyhodnoceni[x] <- paste(vyhodnoceni[x], vyhodnoceni[y], sep = "", collapse = "")
          vyhodnoceni[y] <- NA
        }
      }
    }
    
    # locating unvoiced, checking if next to it is voiced
    location <- which(vyhodnoceni %in% vector_paired_unvoiced_consonants)
    if (length(location) > 0) {
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_paired_voiced_consonants[z]) == FALSE){
          w <- vyhodnoceni[x]
          vyhodnoceni[x] <- vector_paired_consonants[w]
        }
      }
    }

    # locating voiced, checking if next to it is unvoiced
    location <- which(vyhodnoceni %in% vector_paired_voiced_consonants)
    if (length(location) > 0){
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_paired_unvoiced_consonants[z]) == FALSE){
          w <- vyhodnoceni[x]
          vyhodnoceni[x] <- vector_paired_consonants[w]
        }
      }
    }
    #locating ch
    location <- which(vyhodnoceni %in% vector_ch_first)
    if (length(location) > 0 ){
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_ch_second[z]) == FALSE){
          vyhodnoceni[x] <- paste(vyhodnoceni[x], vyhodnoceni[y], sep = "", collapse = "")
          vyhodnoceni[y] <- NA
        }
      }
    }
    
    # locating dtn, checking if next to it is specific vocal 
    location <- which(vyhodnoceni %in% vector_dtn)
    if (length(location) > 0 ){
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_dtn_vocal[z]) == FALSE){
        vyhodnoceni[x] <- paste(vyhodnoceni[x], vyhodnoceni[y], sep = "", collapse = "")
        vyhodnoceni[y] <- NA
        }
      }
    }

    # locating mbpv, checking i next to it is specific vocal
    location <- which(vyhodnoceni %in% vector_mbpv)
    if (length(location) > 0 ){
      for (i in seq_along(location)) {
        x <- location[i]
        y <- x+1
        z <- vyhodnoceni[y]
        if (is.na(vector_mbpv_vocal[z]) == FALSE){
          vyhodnoceni[x] <- paste(vyhodnoceni[x], vyhodnoceni[y], sep = "", collapse = "")
          vyhodnoceni[y] <- NA
        }
      }
    }


    y <- length(vyhodnoceni)
    z <- vyhodnoceni[y]
    if (is.na(vector_paired_voiced_consonants[z]) == FALSE){
      vyhodnoceni[y] <- vector_paired_consonants[z]
    }

    vyhodnoceni <- as.vector(na.omit(vyhodnoceni))
    ### IPA convert
    for (i in seq_along(vyhodnoceni)) {
      x <- vyhodnoceni[i]
      vyhodnoceni_ipa <- append(vyhodnoceni_ipa, vector_temp[x])
    }
    vyhodnoceni_ipa

    for (i in seq_along(vyhodnoceni_ipa)) {
      x <- vyhodnoceni_ipa[i]
      vyhodnoceni_ipa[i] <- vector_ipa[x]
    }
    vyhodnoceni_ipa

    vyhodnoceni_ipa <- paste(vyhodnoceni_ipa, sep = "", collapse = "")

    Encoding(vyhodnoceni_ipa)
    return(vyhodnoceni_ipa)
  })
  
})
##################################



