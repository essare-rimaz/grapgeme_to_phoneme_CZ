library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Transkripce českých slov"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("vstup", "Vaše slovo:", value = "příklad"),
      submitButton("Odeslat")
    ),
    

  mainPanel(
    tabsetPanel(
      tabPanel("Vaše transkripce",
               htmlOutput("vyhodnoceni_ipa")  # text poslaný ze server.R
      ),
      tabPanel("Dodatečné informace",
               p("Tato aplikace transkribuje (nebo-li přepisuje) česká slova do mezinárodní fonetické abecedy IPA. Aby aplikace správně fungovala, je potřeba abyste do ní vložili vždy právě jedno slovo."),
               p("Zároveň je dobré mít na paměti, že některé jevy jako jsou například diftongy, není v možnostech této aplikace věrně zachytit. Diftongy tedy v této aplikaci nejsou značeny."),

               
      )
    )
  )
)))