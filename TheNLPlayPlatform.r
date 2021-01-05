########################################## NLPlay APP ########################################################

########################## Remember to run the Setup Script before you press "Run App" #######################


# --------------------------- INITIAL SETUP ---------------------------------------- #

# Enable upload of larger files
options(shiny.maxRequestSize = 30*1024^2)

## --------------------------- FUNCTIONS -------------------------------- ##
stopwords_remove <- function(input){
  stopwords_regex = paste(stopwords('da'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  input <- stringr::str_replace_all(input, stopwords_regex, '')
  return(input)
}

words_remove <- function(corpus, input_list){
  words_regex = paste(input_list, collapse = '\\b|\\b')
  words_regex = paste0('\\b', words_regex, '\\b')
  corpus <- stringr::str_replace_all(corpus, words_regex, '')
  return(corpus)
}

# User Exercises 
render_exercise <- function(q, hh1, hh2, hh3 = NULL) {
  fluidRow(
    box(title = strong(q), width = 12, height = 40, background = "yellow")
  )
  fluidRow(
    box(title = strong("Hint nr. 1"), collapsible = T, collapsed = T,
        h5(hh1)
    ),
    box(title = strong("Hint nr. 2"), collapsible = T, collapsed = T,
        h5(hh2)
    ),
    box(title = strong("Hint nr. 3"), collapsible = T, collapsed = T,
        h5(hh3))
  )
  br()
}

## ----------------------------------------- DEFINE UI ------------------------------------------------------ ##
header <- dashboardHeader(title = img(src = "NLPlayLogo.png", height = "50%", width = "50%"), titleWidth = 250)


sidebar <- dashboardSidebar(width = 250,
  sidebarMenu(
    menuItem("Velkommen", tabName = "welcome", icon = icon("child")),
    menuItem("Introduktion og Overblik", tabName = "introduction", icon = icon("book-reader")),
    menuItem("DAGW-Modellen", tabName = "semantics", icon = icon("project-diagram")),
    menuItem("Word Cloud", tabName = "wc", icon = icon("cloud")),
    menuItem("Sentiment Analysis", tabName = "sa", icon = icon("theater-masks")),
    menuItem("Topic Modeling", tabName = "tm", icon = icon("buffer")),
    menuItem("Øvelser", tabName = "practice_tasks", icon = icon("user"))
    )
)

# Define loading spinner to indicate that the website is sometimes busy
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 1)

body <- dashboardBody(
  
tabItems(
  
    # Welcome Page
    tabItem(tabName = "welcome", background = "orange",
            br(),
            br(),
            br(),
            h1(img(src = "NLPlayLogo2.png", height = "50%", width = "50%"), align = "center"),
            h4("Et dansk, computerbaseret tekstanalyseredskab", align = "center"),
            img(src = "logo.png"), align = "center",
            br(),
            br(),
            fluidRow(column(width = 12, offset = 2,
              infoBoxOutput("Sofie"),
              infoBoxOutput("Astrid")
            ))),
    
    # Introduction Page
    tabItem(tabName = "introduction",
            br(),
            br(),
            h1(strong("Velkommen til"), img(src = "NLPlayLogo2.png", height = "20%", width = "20%"), align = "center"),
            br(),
            fluidRow(
              box(width = 12, background = "yellow",
                  h4("Denne platform er designet som et redskab til computerbaseret tekstanalyse. Alle funktioner er baseret på Natural Language Processing (NLP) eller som det på dansk hedder Sprogteknologi. Denne tværfaglige disciplin handler om at få computere til at forstå sprog med det formål at trække meningsfuld information ud af store mængder tekst. I en digitaliseret verden, hvor store mængder tekst er frit tilgængeligt kan værktøjer, som denne platform tilbyder, muliggøre tekstanalyse med en kvantitativ tilgang fremfor det traditionelle, kvalitative arbejde med tekster, som vi er vant til. På denne platform får du mulighed for at arbejde med forskellige NLP-værktøjer: Topic Modeling, Sentiment Analysis, Word Clouds, og en semantisk model trænet på den hidtil største samling af dansk sprogbrug i alle former. Vi anbefaler, at du ser vores introduktionsvideo, som demonstrerer, hvordan platformen kan bruges. Denne video er tilgængelig her: https://github.com/TheNLPlayPlatform/NLPlay")
                )
            ),
            h2(strong("Overblik"), align = "center"),
            br(),
            
            # Tool 1
            fluidRow(
              infoBoxOutput("DAGW_Model"),
              tags$style(
                type = 'text/css',
                'i.fa.fa-minus {
                font-weight: 5000;
                color: grey;
                }'
              ),
              box(title = "Læs om DAGW-modellen", width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("DAGW-modellen er en semantisk model, som har sit navn fra det korpus den er baseret: The Danish Gigaword Corpus (DAGW). Dette korpus består af over 1 milliard danske ord fra vidt forskellige genrer, tidsperioder, sammenhænge samt steder, hvilket gør dette korpus repræsentativt for det danske sprog. En semantisk model af denne type repræsenterer ord som vektorer (altså en række tal) for at kunne placere dem i et multidimensionelt rum i relation til hinanden. Det vil sige, at ord, som bruges i de samme sproglige sammenhænge får lignende vektorer og af denne grund optræder de tættere på hinanden i dette multidimensionelle rum. Således kan man bruge den numerale afstand mellem ord til at sige noget om semantiske relationer. På denne platform har du mulighed for at anvende denne model til at undersøge semantiske sammenhænge efter eget valg. Måske kunne du være interesseret i at finde ord, der ligger tæt på ”kvinde”, eller finde den semantiske afstand mellem ordet ”kvinde” og ”statsminister”, eller endda sammenligne denne afstand med afstanden mellem ordet ”mand” og ”statsminister”."))),
            br(),
            
            # Tool 2
            fluidRow(
              infoBoxOutput("Topic_Modeling"),
              box(title = "Læs om Topic Modeling | Emnemodellering",width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("Topic Modeling, også kaldet Emnemodellering, drejer sig om at finde emner på tværs af tekster. Af denne grund anvendes Topic Modeling primært når man har at gøre med store samlinger af tekster.  En topic model forstår tekst som en samling af emner, og emner som en samling af ord. Med topic modelling kan du finde information om det overordnede indhold af en tekstsamling og de enkelte tekster i den. En topic model er ofte god til at finde latente emner, som man ellers ville overse som læser, og kan således være en indgang til nye muligheder for uudforsket fortolkning, som ikke er farvet af eksisterende kendskab til teksten. Måske kan du opdage nye temaer i Thit Jensens samlede værker, som hidtil har været overskygget af det kendte narrativ om hendes forfatterskab. Eller måske arbejder du med nyhedsartikler om dansk politik og ønsker at kategorisere dem på baggrund af de emner, som behandles i dem."))),
            br(),
            
            # Tool 3
            fluidRow(
              infoBoxOutput("WordCloud"),
              box(title = "Læs om Word Clouds | Ordskyer", width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("En word cloud er en visuel repræsentation af indholdet af en tekst baseret på ords hyppighed. Ord, som anvendes hyppigt, optræder således både større og mere centralt i ordskyen sammenlignet med mindre hyppige ord. En word cloud kan være et godt sted at starte en computerbaseret tekstanalyse, idet den giver et god overblik over tekstens indhold. En ordsky har mange anvendelsesmuligheder. Eksempelvis kunne man forestille sig at lave en ordsky baseret på historiske tekster såsom vidneberetninger fra 2. verdenskrig med det formål at få et indledende overblik af teksten indhold, som kan bruges som springbræt til en dybdegående analyse."))),
            br(),
            
            # Tool 4
            fluidRow(
              infoBoxOutput("SentimentAnalysis"),
              box(title = "Læs om Sentiment Analysis | Stemningsanalyse", width = 8, background = "yellow", collapsible = T, collapsed = F,
                  h5("Sentiment analysis, eller stemningsanalyse, er en metode til at kvantificere den generelle stemning i en tekst baseret på de positivt- og negativt ladede ord, som optræder i teksten. Med sentiment analysis kan du således få et overblik over en hel teksts stemning, men du også bruge sentiment analysis på enkeltstående ord eller sætninger. Måske er du interesseret i at finde ud af, hvordan forskellige danske aviser omtaler et givent parti, emne, eller person, hvortil sentiment analysis kan være behjælpelig.")))
    ),

    # Sentiment Analysis Tab
    tabItem(tabName = "sa",
            fluidRow(
              box(title = strong("Sentiment Analysis | Stemningsanalyse"), width = 12, background = "yellow",
                  h4("Herinde kan man få et overblik over stemningen i en given tekst. Man får både den totale stemningsværdi, hvilket er mest relevant for enkle ord eller sætninger, samt den gennemsnitlige stemningsværdi, hvilket er mere relevant for hele tekster. De beregnede værdier er baseret på SENTIDA, som er et dansk sentiment analysis leksikon udviklet specifikt til dette formål.")),
            ),
            fluidRow(
              box(title = span(strong("Påbegynd din Stemningsanalyse"), style = "color:orange",
                               bsButton("info14_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info14_sa", title = "",
                                         content = paste0("Hvis du vælger at uploade din tekst som en fil er det vigtigt, at denne er en txt-fil."),
                                         trigger = "hover",
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 12,
              radioButtons(
                inputId = "sa_source",
                label = "Text Input",
                choices = c(
                  "Indsæt tekst(er) manuelt" = "sa_own",
                  "Upload en tekstfil" = "sa_file"
                )),
                conditionalPanel(
                  condition = "input.sa_source == 'sa_own'",
                  textAreaInput(inputId = "sa_text", "Indsæt tekst her", rows = 7)
                ),
                conditionalPanel(
                  condition = "input.sa_source == 'sa_file'",
                  fileInput("sa_file", "Vælg din tekstfil")
                )
              )
            ),
            fluidRow(
              box(title = span(strong("Samlede Sentimentværdi"), style = "color:orange",
                               bsButton("info1_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info1_sa", title = "",
                                         content = paste0("Den samlede sentimentværdi er summen af hvert ords sentimentværdi. Der tages højde for de omkringstående ord, så et negativt ord bliver f.eks. yderligere negativt ladet hvis der står *meget* foran det."),
                                         trigger = "hover",
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 6,
                  withSpinner(verbatimTextOutput("sentiment_score_total"), type = 1)),
              box(title = span(strong("Gennemsnitlig Sentimentværdi"), style = "color:orange",
                               bsButton("info2_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info2_sa", title = "",
                                         content = paste0("Den gennemsnitlige sentimentværdi er beregnet som den samlede sentimentværdi divideret med antallet af ord. Den gennemsnitlige sentimentværdi kan ligge mellem -5 (meget negativ) og +5 (meget positiv), hvor 0 angiver en neutral tone."),
                                         trigger = "hover",
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 6,
                  withSpinner(verbatimTextOutput("sentiment_score_mean"), type = 1))
            ),
            fluidRow(
              box(title = span(strong("Polaritetsplot"), style = "color:orange",
                               bsButton("info3_sa", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info3_sa", title = "",
                                         content = paste0("Polaritetsplottet viser dig fordelingen af positive og negative ord i din tekst. Neutrale ord er ekskluderede, da nogle ord ikke eksisterer i det anvendte leksikon, og dette ville således give et forkert billede af fordelingen."),
                                         placement = "top", 
                                         trigger = "hover", 
                                         options = list(container = "body")
                               )), width = 12,
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
                  ),
                  withSpinner(highchartOutput("sentiment_polarity_plot", height = 210), type = 1)
                  )
            )
        ),
    
    # DAGW Model Tab 
    tabItem(tabName = "semantics",
            fluidRow(
              box(title = strong("DAGW-Modellen"), width = 12, background = "yellow",
                  h4("Herinde kan du udforske semantiske relationer mellem ord baseret på DAGW-modellen, som er trænet på The Danish Gigaword Corpus (DAGW) med over 1 milliard danske ord."))
              ),
            fluidRow(
              box(title = strong("Semantiske Relationer"), width = 12, background = "yellow", collapsible = T, collapsed = F,
                  h4("Nedenfor kan du udforske ords semantiske relationer med andre ord. Måske er du interesseret i at finde ud af, hvilke ord, der ligger tættest på 'hest' eller 'ost'? I så fald, kan du indtaste det ord, og se hvilke ord, der har den højeste semantiske lighed. Til højre vil de semantiske sammenhænge visualiseres på forskellig vis."))
                  ),
            fluidRow(
              box(width = 6, height = 600,
                  textInput(inputId = "sm_text1",
                                label = h4(span(strong("Indtast et ord"), style = "color:orange"),
                                           bsButton("model_info1", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                           bsPopover(id = "model_info1", title = "",
                                                     content = paste0("Indtast et hvilket som helst ord og find ud af hvilke ord, der er relateret til dette ord. Semantisk lighed er et mål for, hvor ens de to ord er. Dette tal ligger mellem 0 og 1, hvor 0 er helt uens og 1 er helt ens."),
                                                     placement = "right", 
                                                     trigger = "hover", 
                                                     options = list(container = "body")
                                                     )),
                            placeholder = "Indtast et ord her, f.eks. 'ost'"),
                  h6("*Vær opmærksom på, at modellen er meget stor, så det kan tage noget tid at loade resultaterne*", style = "color:grey"),
                  hr(),
                  withSpinner(tableOutput("table_similar"), type = 1)),
                  box(width = 6, height = 600,
                  tabBox(
                    side = "right", height = "550px", width = 12,
                    title = tagList(shiny::icon("hubspot"), "Plots"),
                    selected = "Plot 1",
                    tabPanel("Plot 3", withSpinner(plotOutput("semantics_plot3"), type = 1)),
                    tabPanel("Plot 2", withSpinner(plotOutput("semantics_plot2"), type = 1)),
                    tabPanel("Plot 1", withSpinner(plotOutput("semantics_plot1"), type = 1))
                  ))
              ),
            fluidRow(
              box(title = strong("Semantisk Lighed"), width = 12, background = "yellow", collapsible = T, collapsed = F,
                  h4("Nedenfor kan du finde den semantiske lighed mellem to ord. Den semantiske lighed kan ligge mellem 0 og 1 afhængigt af ligheden mellem de to ord. Jo tættere på 0 den semantiske lighed er, desto mere ulig er ordene. Jo tættere på 1 den semantiske lighed er, desto mere ens er ordene mht. semantik."))
            ),
            fluidRow(
              box(title = span(strong("Find den semantiske lighed mellem to ord"), style = "color:orange",
                               bsButton("info_nocapitalletters", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info_nocapitalletters", title = "",
                                         content = paste0("Ordene skal stå med små bogstaver"),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )), width = 6, height = 250,
                  textInput(inputId = "semanticdistance_word1",
                            label = "Indtast det første ord her",
                            placeholder = "Indtast et ord her, f.eks. 'hval'"),
                  textInput(inputId = "semanticdistance_word2",
                            label = "Indtast det næste ord her",
                            placeholder = "Indtast et ord her, f.eks. 'løve'")
              ),
              box(width = 6, height = 250,
                     bsButton("model_info2", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                     bsPopover(id = "model_info2", title = "",
                               content = paste0("Den semantiske lighed kan ligge mellem 0 og 1 afhængigt af ligheden mellem de to ord. Jo tættere på 0 den semantiske lighed er, desto mere ulig er ordene. Jo tættere på 1 den semantiske lighed er, desto mere ens er ordene mht. semantik. Hvis du får resultatet NA, betyder det højst sandsynligt, at ordet ikke eksiterer i modellen og den semantiske lighed kan derfor ikke udregnes."),
                               placement = "left",
                               trigger = "hover", 
                               options = list(container = "body")),
                  h1(textOutput("semanticdistance_output"), style = "font-size:150px;", style = "color:orange;", align = "center"),
            )),
            fluidRow(
              box(title = strong("Beregninger med Ordvektorer"), width = 12, background = "yellow", collapsible = T, collapsed = F,
                  h4("Nedenfor kan man trække ordvektorer fra hinanden eller lægge dem sammen, og se hvilke ord det resulterer i. Med ordvektorer skal forstås en samling af de egenskaber, der til sammen skaber et ords betydning. Eksempelvis, kan man trække vektoren for 'mand' fra 'konge' og plusse 'kvinde' og få 'dronning'. Altså at trække egenskaben 'mand' ud af begrebet 'konge' og lægge egenskaben 'kvinde' til giver blandt andet 'dronning'."))
            ),
            fluidRow(
              box(title = span(strong("Beregninger"), style = "color:orange",
                               bsButton("model_info3", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "model_info3", title = "",
                                         content = paste0("Vælg den form for beregning du vil lave, og indtast de ord, du vil lave beregninger. Ét ord per boks."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )), width = 6,
                  
                  radioButtons(
                    inputId = "calc_source",
                    label = "Hvilken form for beregning vil du lave?",
                    choices = c(
                      "Ord 1 + Ord 2" = "calc_option1",
                      "Ord 1 - Ord 2" = "calc_option2",
                      "Ord 1 - Ord 2 + Ord 3" = "calc_option3",
                      "Ord 1 + Ord 2 + Ord 3" = "calc_option4",
                      "Ord 1 + Ord 2 - Ord 3" = "calc_option5",
                      "Ord 1 - Ord 2 - Ord 3" = "calc_option6"
                    )),
                  tags$hr(),
                  textInput(inputId = "word1_sm",
                            label = "Ord 1",
                            placeholder = "Indtast det første ord her, f.eks. 'konge'"),
                  textInput(inputId = "word2_sm",
                            label = "Ord 2",
                            placeholder = "Indtast det andet ord her, f.eks. 'mand'"),
                  textInput(inputId = "word3_sm",
                            label = "Ord 3",
                            placeholder = "Indtast det tredje ord her, f.eks. 'kvinde'"),
                  tags$hr(),
                  radioButtons(
                    inputId = "calc_n", 
                    label = "Antal resultatord der vises", 
                    choices = c("5" = 5, "10" = 10, "20" = 20), 
                    selected = "5", 
                    inline = TRUE),
                  tags$hr(),
                  column(12,
                         actionButton("calc_action", "Udregn", style = "color: #fff; background-color: orange; border-color: orange"),
                         align = "center"
                  ),
                  tags$hr()
              ),
              box(title = span(strong("Resultater"), style = "color:orange"), width = 6,
                  withSpinner(div(tableOutput("new_word_vector"), style = "font-size:130% ; height:150%"), type = 1), align = "center")
            ),
            fluidRow(
              box(width = 12, background = "yellow",
                         h4(strong("Vil du have fingrene i DAGW-Modellen til dine egne analyser?"), align = "center"),
                         h4("Modellen er tilgængelig som på vores GitHub: https://github.com/TheNLPlayPlatform/NLPlay"), align = "center"
                    )
            )
    ),
    # Word Cloud Tab
    tabItem(tabName = "wc",
            fluidRow(
              box(title = strong("Word Cloud | Ordsky"), width = 12, background = "yellow",
                  h4("Herinde kan man generere en word cloud, som er en visuel repræsentation af ords hyppighed i en given tekst: jo større font, jo flere gange optræder et ord. Når man genererer en word cloud fra en tekst, vil teksten automatisk gennemgå nogle basale NLP-trin: danske stopord, tal og tegnsætning fjernes, og store bogstaver gøres små. Dertil er det muligt, at ordene i ordskyen farves alt afhængigt af deres ladning. Altså, negativt ladede ord vil være røde, mens positivt ladede ord vil fremstå grønne. Word clouds er et godt udgangspunkt for en mere dybdegående tekstanalyse.")),
            ),
            fluidRow(
              box(title = span(strong("Lav din Word Cloud"), style = "color:orange",
                               bsButton("info11_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info11_wc", title = "",
                                         content = paste0("Vælger du at uploade din tekst som en fil, skal dette være en txt-fil eller en CSV-fil."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )), width = 12,
                  radioButtons(
                    inputId = "source",
                    label = "Tekstinput",
                    choices = c(
                      "Indsæt din tekst manuelt" = "own",
                      "Upload en tekstfil" = "file"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.source == 'own'",
                    textAreaInput("text", "Indsæt din tekst her", rows = 7)
                  ),
                  conditionalPanel(
                    condition = "input.source == 'file'",
                    fileInput("file", "Vælg din tekstfil")
                  ),
                  # checkboxInput(inputId = "stem_wc", label = span("Stem dine ord",
                  #                                              bsButton("info41_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                  #                                              bsPopover(id = "info41_wc", title = "",
                  #                                                        content = paste0("At ’stemme’ ord betyder at reducere dem til deres stamme. F.eks. bliver ordet ’gyngede’ til ’gyng’. Dette er en god idé, hvis der det samme ord i din word cloud med forskellige bøjninger. Ved at stemme ordene, vil du kun få stammen af hvert ord, så der ikke forekommer gentagelser af det samme ord."),
                  #                                                        trigger = "hover", 
                  #                                                        placement = "bottom",
                  #                                                        options = list(container = "body")
                  #                                              ))),
                  checkboxInput(inputId = "sentiment_wc", span("Farv negative og positive ord",
                                                     bsButton("info1_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                                     bsPopover(id = "info1_wc", title = "",
                                                               content = paste0("Hvis du vælger, at ordene i din ordsky skal farves efter deres ladning, vil positive ord fremstå grønne, mens negative ord vil fremstå røde. Neutrale ord, eller ord, som ikke eksisterer i leksikonnet vil fremstå grå."),
                                                               trigger = "hover", 
                                                               placement = "right",
                                                               options = list(container = "body")
                                                     )), value = FALSE
                                ),
                  conditionalPanel(
                    condition = "input.sentiment_wc == 1"),
                  checkboxInput(inputId = "wc_remove_words", label = span("Fjern andre ord",
                                                                      bsButton("info2_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                                                      bsPopover(id = "info2_wc", title = "",
                                                                                content = paste0("Hvis du lægger mærke til ord i din word cloud, som du ikke er interesseret i, kan du fjerne dem her. Ordet skal *ikke* stå med stort begyndelsesbogstav."),
                                                                                trigger = "hover", 
                                                                                placement = "right",
                                                                                options = list(container = "body")
                                                                      )), value = FALSE
                                ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1",
                textAreaInput("wc_words_to_remove1", "Indtast de ord, du ønsker at fjerne. Ét ord per boks", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove1.length > 0",
                textAreaInput("wc_words_to_remove2", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove2.length > 0",
                textAreaInput("wc_words_to_remove3", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove3.length > 0",
                textAreaInput("wc_words_to_remove4", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove4.length > 0",
                textAreaInput("wc_words_to_remove5", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove5.length > 0",
                textAreaInput("wc_words_to_remove6", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove6.length > 0",
                textAreaInput("wc_words_to_remove7", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove7.length > 0",
                textAreaInput("wc_words_to_remove8", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove8.length > 0",
                textAreaInput("wc_words_to_remove9", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove9.length > 0",
                textAreaInput("wc_words_to_remove10", "", rows = 1)
              ),
              conditionalPanel(
                condition = "input.wc_remove_words == 1 && input.wc_words_to_remove10.length > 0",
                textAreaInput("wc_words_to_remove11", "", rows = 1)
              ),
              colourInput(inputId = "col", "Baggrundsfarve", value = "white"),
              numericInput("num", "Antal ord i din word cloud",
                           value = 100, min = 5),
              )
            ),
            fluidRow(
              box(title = span(strong("Word Cloud"), style = "color:orange",
                               bsButton("info12_wc", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info12_wc", title = "",
                                         content = paste0("Ord som hyppigt optræder i din tekst fremstår større og mere centraliseret. Hvis du vælger at farve ordene efter deres ladning, vil positive ord være grønne, mens negative ord vil være røde. Vær opmærksom på, at grå ord kan både være neutrale ord, men det kan også være ord, som ikke optræder i leksikonnet. Når du holder musen henover et ord, viser den ordets hyppighed."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )),
                  withSpinner(wordcloud2Output("cloud"), type = 1), width = 12)
            ),
            fluidRow(
              box(title = span(strong("De Mest Hyppige Ord"), style = "color:orange"), 
                  withSpinner(plotlyOutput("wc_word_frequency_plot"), type = 1), width = 12
                  )
            )
      ),

    # Topic Modeling Tab
    tabItem(tabName = "tm",
            fluidRow(
              box(title = strong("Topic Modeling | Emnemodellering"), width = 12, background = "yellow",
                  h4("Topic modeling, eller emnemodellering, er en machine-learning metode, som identificerer emner (topics) på tværs af tekster baseret på ordene i dem. Topic modeling kan levere meningsfuld information om det overordnede indhold af en tekstsamling og de individuelle tekster deri. Topic modeling er således et brugbart værktøj til at klassificere, organisere og opsummere indholdet af tekster. Ofte anvendes topic modeling til at identificere latente emner i tekster, som ellers ville blive overset ved traditionel, kvalitativ tekstanalyse. At lave en god topic model er en process præget af mange gentagelser, hvilket betyder, at du sandsynligvis ikke vil ramme rigtigt i første forsøg. Topic modeling skal derfor anses som en process, hvor man prøver sig frem med forskellige parametre for at finde frem til et antal emner som giver mening for en given samling af tekster. Du kan derfor forvente at vende tilbage til tidligere trin længere henne i processen.")),
            ),
            fluidRow(
              box(title = strong("Trin 1: Data"), width = 12, height = 40, background = "yellow")
              ),
            fluidRow(
              box(title = span(strong("Instruktioner"), style = "color:orange"), width = 12, collapsible = T, collapsed = F,
                  h4("Du kan vælge mellem at indsætte dine tekster manuelt, eller uploade en CSV-fil (comma-separated-values file). En sådan fil kan du lave i Excel, og gemme den som en CSV-fil. Hvis du vælger at indsætte dine tekster manuelt, skal du indsætte overskriften for hver tekst samt indholdet af hver tekst i de respektive felter. Vær opmærksom på, at du kun kan have en samling af i alt fem tekster, hvis du vælger at indsætte teksterne manuelt. Måske arbejder du med en samling af eventyr, og i så fald skal du indtaste titlen på hvert eventyr, f.eks. 'Klods Hans', og selve eventyret nedenunder. Hvis du vælger at uploade en CSV-fil, er det meget vigtigt, at filen har det korrekte format. Se infoboksen for mere information herom."))
            ),
            fluidRow(
              box(title = span(strong("Vælg din data"), style = "color:orange",
                               bsButton("info1_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info1_tm", title = "",
                                         content = paste0("Hvis du vælger at uploade en CSV-fil, skal du sikre dig, at den har det korrekte format. Titlen på hver tekst skal forekomme i den første kolonne, mens selve teksten skal forekomme i kolonnen derefter (se billedet nedenfor)", img(src = "CSV.png")),
                                         trigger = "hover", 
                                         placement = "bottom",
                                         options = list(container = "body")
                               )), width = 12,
                  radioButtons(
                    inputId = "tm_source",
                    label = "Vælg dit tekstinput",
                    choices = c(
                      "Upload en CSV-fil" = "tm_uploadfile",
                      "Indsæt din tekst manuelt" = "tm_own"
                    )
                  ),
                  hr(),
                  conditionalPanel(
                    condition = "input.tm_source == 'tm_own'",
                    textInput(inputId = "tm_title", "Titel 1"),
                    textAreaInput(inputId = "tmtext", "Tekst 1"),
                    textInput(inputId = "tm_title2", "Titel 2"),
                    textAreaInput(inputId = "tmtext2", "Tekst 2"),
                    textInput(inputId = "tm_title3", "Titel 3"),
                    textAreaInput(inputId = "tmtext3", "Tekst 3"),
                    textInput(inputId = "tm_title4", "Titel 4"),
                    textAreaInput(inputId = "tmtext4", "Tekst 4"),
                    textInput(inputId = "tm_title5", "Titel 5"),
                    textAreaInput(inputId = "tmtext5", "Tekst 5")
                  ),
                  conditionalPanel(
                    condition = "input.tm_source == 'tm_uploadfile'",
                    fileInput(inputId = "tm_file", "Vælg fil",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    h5(span(strong("Indstillinger For Filformat")),
                       bsButton("info32_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                       bsPopover(id = "info32_tm", title = "",
                                 content = paste0("Hvis du har fulgt instruktionerne om filformat, skal du ikke ændre indstillingerne nedenfor. Tryk bare på Upload Fil."),
                                 trigger = "hover",
                                 placement = "bottom",
                                 options = list(container = "body")
                       )),
                    checkboxInput("header", "Header", FALSE),
                    radioButtons("sep", "Separator", choices = c(Komma = ",", Semikolon = ";", Tab = "\t"), selected = ";", inline = TRUE),
                    radioButtons("quote", "Quote", choices = c(Ingen = "", "Dobbelt citationstegn" = '"', "Enkelt citationstegn" = "'"), selected = '"', inline = TRUE),
                    tags$hr(),
                    column(12,
                           actionButton("submitDataForUpload", "Upload Fil", style = "color: #fff; background-color: orange; border-color: orange"),
                           align = "center",
                           textOutput("upload_complete")
                    ),
                    tags$hr()
                  )
              ),
            ),
            
            fluidRow(
              box(title = strong("Trin 2: Forberedende Databehandling"), width = 12, height = 40, background = "yellow")
              ),
            fluidRow(
              box(title = span(strong("Instruktioner"), style = "color:orange"), width = 12, collapsible = T, collapsed = F,
                  h4("Nu har du uploadet dine tekster og er klar til at foretage noget forberedende databehandling, før din topic model kan laves. Selv hvis du ikke ønsker at foretage dig noget, skal du klikke på kanppen 'Gem databehandling' inden du kan gå videre til trin 3. Nedenfor har du mulighed for at 'stemme' ordende i dit corpus, hvilket betyder at reducere alle ord til deres stamme, hvilket typisk gøres når man har at gøre med et stort korpus. Du kan også fjerne standard stopord, hvilket er ord, som optræder ofte, men som ikke er særlig betydningsbærende (f.eks. 'og', 'men', 'et'). Hvis der er ord, som ofte bliver brugt i dine tekster, men som ikke er yderligere interessante at have med i din topic model, så kan du fjerne dem. Hvis der f.eks, er et ord, du ved optræder i alle dine tekster, kan det være en god idé at fjerne det. Måske analyserer du manuskriptet på Aftenshowet, og i så fald, kunne det give mening at fjerne ord som 'god', 'aften', og 'Aftenshowet'. Dertil skal du også bestemme antallet af dokumenter et ord minimum skal optræde i for at blive inkluderet. Du kan altid vende tilbage hertil og justere dette antal. I plottet til højre får du et overblik over de mest hyppige ord i din samling af tekster. Det kan du bruge til at identificerer ord, som kan give mening at fjerne. I TF-IDF visualiseringen får du et overblik over, hvor vigtige bestemte ord er for dine tekster. Check infoboksene for yderligere information."))
            ),
            fluidRow(
              box(title = span(strong("Indstillinger"), style = "color:orange"),
                  checkboxInput(inputId = "stem", label = span("Stem dine ord",
                                bsButton("info22_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                bsPopover(id = "info22_tm", title = "",
                                          content = paste0("At ’stemme’ ord betyder at reducere dem til deres stamme. F.eks. bliver ordet ’gyngede’ til ’gyng’"),
                                          trigger = "hover", 
                                          placement = "bottom",
                                          options = list(container = "body")
                                )), value = FALSE),
                  
                  checkboxInput(inputId = "stops", label = span("Fjern stopord",
                                                                bsButton("info2_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                                                bsPopover(id = "info2_tm", title = "",
                                                                          content = paste0("Stopord er ord som ’af’, ’at’, ’og’ som brugest hyppigt, men som ikke er særligt betydningsbærende. Af denne grund anbefales det at fjerne dem, medmindre du er specifik interesseret i brugen af dem."),
                                                                          trigger = "hover", 
                                                                          placement = "bottom",
                                                                          options = list(container = "body")
                                                                )), value = TRUE),
                  
                  checkboxInput(inputId = "remove_words2", label = span("Fjern andre ord",
                                                                        bsButton("info3_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                                                                        bsPopover(id = "info3_tm", title = "",
                                                                                  content = paste0("Hvis du lægger mærke til ord, som forekommer hyppigt, men som ikke er særlig interessante at tage med i din topic model, kan du fjerne dem."),
                                                                                  trigger = "hover", 
                                                                                  placement = "right",
                                                                                  options = list(container = "body")
                                                                        )), value = FALSE),
                  conditionalPanel(
                    condition = "input.remove_words2 == 1",
                    textInput(inputId = "words_to_remove12", label = "Indtast de ord, du ønsker at fjerne. Adskil dem med komma og mellemrum", placeholder = "f.eks. plattenslager, så, tænke, sagde")
                  ),
                  hr(),
                  sliderInput("minDoc",
                              "Minimum antal tekster et ord skal optræde i for at blive inkluderet:", 
                              min = 0,  max = 50,  value = 1, step = 1),
                  bsTooltip("minDoc", "Træk for at justere. Dette antal afhænger naturligvis af, hvor mange tekster du har.",
                            "left", options = list(container = "body"),
                            ),
                  tags$hr(),
                  
                  actionButton(inputId = "dfm_button", label = "Gem databehandling", icon = icon("bullseye"), style="color: #fff; background-color: orange; border-color: orange"), 

                  textOutput("dfm_verbatim"),
              ),
              box(title = span(strong("Ordoptælling"), style = "color:orange"),
                  withSpinner(DTOutput("tm_wordcount"), type = 1))
            ),
            
            fluidRow(
              box(title = span(strong("TF-IDF Visualisering"), 
                               bsButton("info4_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info4_tm", title = "",
                                         content = paste0("TF-IDF står for ’Term Frequency Inverse Document Frequency’ og er et mål for hvor vigtig et ord er for en tekst i en samling af tekster, men det er altså *ikke* et udtryk for emner - dem er vi ikke kommet til endnu. Nedenfor kan du altså se vigtigheden af bestemte ord som optræder i dine tekster. TF-IDF værdien er angivet på x-aksen."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               ), style = "color:orange"), width = 12, collapsible = T, collapsed = F,
                  sliderInput("height_tfidf_plot",
                              "Justér højden af dine plots, hvis de ikke er læsbare", 
                              min = 100,  max = 20000,  value = 1000, step = 10),
                  bsTooltip("height_tfidf_plot", "Træk for at justere. Hvis du har mange tekster, kan det være nødvendigt at gøre højden større. Hvis du har få tekster, skal højden gøres mindre.",
                            "left", options = list(container = "body"),
                  ),
                  withSpinner(plotlyOutput("tfidf_plot", height = "auto"), type = 1)
              )
            ),
            
            fluidRow(
              box(title = strong("Trin 3: Topic Model"), width = 12, height = 40, background = "yellow")
            ),
            fluidRow(
              box(title = span(strong("Instruktioner"), style = "color:orange"), width = 12, collapsible = T, collapsed = F,
                  h4("Nu er du klar til at køre din topic model og finde fremtrædende emner i dine tekster. Først skal du dog bestemme, hvor mange emner du vil have. At lave en god topic model er som sagt en process præget af mange gentagelser, hvilket betyder, at du sandsynligvis ikke vil ramme rigtigt i første forsøg. Altså, prøv dig ad med forskellige antal emner, og se hvad der giver mest mening for dine tekster. Når du har kørt din model vil du kunne se to visualiseringer; betaværdier og gammaværdier. Betaværdier angiver hvor meget et bestemt ord bidrager til et bestemt emne. Altså, betaværdier angiver hvor vigtige bestemte ord er i et bestemt emne. Gammaværdier er per-tekst-per-emne sandsynlighedsværdier, hvilket betyder at gammaværdier angiver hvor meget hver tekst er knyttet til et bestemt emne. Altså, gammaværdier angiver hvor stor en del af en tekst, der består af ord fra et bestemt emne. Hvis du har mange tekster, vil grafen være mest læsbar, hvis den er organiseret efter emne. Hvis du har mange emner, vil grafen være mest læsbar, hvis den er organiseret efter tekst."))
              ),
            fluidRow(
              box(title = span(strong("Modelparametre"), style = "color:orange"),
                  width = 12,
                  column(1.5,
                         sliderInput("num.topics",
                                     "Hvor mange emner vil du have?", 
                                     min = 0,  max = 50,  value = 10, step = 1), align = "left",
                         bsTooltip("num.topics", "Træk for at justere",
                                   "left", options = list(container = "body"))),
                  column(1.5,
                         sliderInput("iter",
                                     "Antal Iterationer: Hvor mange gange skal din model tilpasse sig dataen? (Det er sjældent nødvendigt at ændre denne indstilling)",
                                     min = 20,  max = 200,  value = 100, step = 20), align = "left",
                         bsTooltip("iter", "Jo flere gange din model forsøger at tilpasse sig dataen, desto mere præcis bliver den, men det kan potentielt tage meget lang tid.",
                                   "left", options = list(container = "body"))),
                  hr(),
                  actionButton("topic_button", "Kør Model", icon = icon("running"), style = "color: #fff; background-color: orange; border-color: orange"),
                  h6("*Tryk kun én gang*", style = "color:grey"),
                  textOutput("model_running")
                  )
              ),
              fluidRow(
                box(title = span(strong("Betaværdier: Relationen Mellem Ord og Emner"), style = "color:orange",
                               bsButton("info6_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info6_tm", title = "",
                                         content = paste0("Betaværdier angiver, hvor meget et bestemt ord bidrager til et bestemt emne. Et ord med en høj betaværdi bidrager meget til et bestemt emne. Betaværdierne er angivet på x-aksen."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )),
                    sliderInput("height_betaplot",
                                "Justér højden af dine plots", 
                                min = 100,  max = 10000,  value = 1000, step = 1),
                    bsTooltip("height_betaplot", "Træk for at justere.",
                              "left", options = list(container = "body"),
                    ),
                  withSpinner(plotlyOutput("beta_plot", height = "auto"), type = 1), width = 12, 
                  )
              ),
            fluidRow(
              box(title = span(strong("Gammaværdier: Relationen Mellem Emner og Tekster"), style = "color:orange", width = 12,
                               bsButton("info7_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                               bsPopover(id = "info7_tm", title = "",
                                         content = paste0("Gammaværdier er per-tekst-per-emne sandsynlighedsværdier, hvilket betyder at gammaværdier angiver, hvor meget hver tekst er knyttet til et bestemt emne. Altså, gammaværdier angiver hvor stor en del af en tekst, der består af ord fra et bestemt emne. Hvis du har mange tekster, vil grafen være mest læsbar, hvis den er organiseret efter emne. Hvis du har mange emner, vil grafen være mest læsbar, hvis den er organiseret efter tekst."),
                                         trigger = "hover", 
                                         placement = "right",
                                         options = list(container = "body")
                               )),
                  
                  h5(strong("Gammaværdier organiseret efter tekster")),
                  h5("Når gammaværdierne er organiseret efter tekster, vises der et plot for hver af dine tekster, hvor emner er angivet på x-aksen, mens gammaværdierne er angivet på y-aksen. Ud fra disse plots kan du dermed aflæse, hvilket emne, der mest sandsynligt tilhører dine tekster. Et emne med en høj gammeværdi for en bestemt tekst betyder, at dette emne højst sandsynligt tilhører denne tekst."),
                  h5(strong("Gammaværdier organiseret efter emner")),
                  h5("Når gammaværdierne er organiseret efter emner, vises der et plot for hvert emne, hvor gammaværdierne er angivet på x-aksen, mens antallet af tekster er angivet på y-aksen. Ud fra disse plots kan du dermed aflæse, hvor mange tekster, der knytter sig til et bestemt emne, og hvor stor sandsynligheden er for, at dette antal tekster tilhører dette emne. En høj gammaværdi betyder altså, at dette antal dokumenter højst sandsynligt tilhører det specifikke emne."),
                  hr(),
                  h4(span(strong("Grafparametre"), style = "color:orange"), align = "left",
                     bsButton("info5_tm", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small"),
                     bsPopover(id = "info5_tm", title = "",
                               content = paste0("Hvis du har mange tekster, vil grafen være mest læsbar, når den er organiseret efter emne. Hvis du derimod har mange emner, vil grafen være mest læsbar, når den er organiseret efter tekst. Gammaværdier er per-tekst-per-emne sandsynlighedsværdier, hvilket betyder, at gammaværdier angiver, hvor meget hver tekst er knyttet til et bestemt emne. Det vil sige, at en tekst med en høj gammaværdi for et bestemt emne betyder, at den tekst knytter sig meget til det bestemte emne."),
                               trigger = "hover", 
                               placement = "right",
                               options = list(container = "body")
                     )),
                  radioButtons(inputId = "tm_plot_source", 
                               label = "Organisér gammaværdier efter:",
                               choices = c("Tekster" = "rb_docs", 
                                           "Emner" = "rb_tops")
                  ), align = "left",
                  hr(),
          
                  sliderInput("height_gammaplot",
                              "Justér højden af dine plots", 
                              min = 100,  max = 20000,  value = 1000, step = 1),
                  bsTooltip("height_gammaplot", "Træk for at justere.",
                            "left", options = list(container = "body"),
                  ),
                  withSpinner(plotlyOutput("gamma_plot", height = "auto"), type = 1), width = 12,
              )
            )
    ),
    
    tabItem(tabName = "practice_tasks",
            fluidRow(
              box(title = strong("Træningsopgaver"), width = 12, background = "yellow",
                  h4("Herunder er stillet en række opgaver, som du kan give dig i kast med, for at få en fornemmelse for hvordan NLPlay kan bruges og hvilke sprøgsmål den bl.a. kan anvendes til at besvare. Hvis du kommer i tvivl om hvordan en opgave kan løses, kan du gøre brug af hints, som kan hjælpe dig i den rigtige retning. Tryk på + ud for hintet for at se det. Som det er med tekstanalyse er der ofte ikke ét rigtigt svar, for mange løsninger vil afhænge af hvordan netop du griber opgaven an, hvilke ord du f.eks. er interesseret i, og hvilke du filtrerer fra. Af samme grund tilbydes ingen løsninger i forbindelse med opgaverne. Så: prøv dig frem, udforsk, og vurdér dine resultater. Husk at NLPlay kun kan regne på tekster - den kan ikke vurdere om resultaterne er meningsfyldte.")),
            ),
            fluidRow(
              box(title = span(strong("Filer"), style = "color:orange"), width = 12,
                  column(width = 6,
                         br(),
                         h5("Til de fleste opgaver skal du bruge denne fil, som indeholder samtlige af Dronning Margrethes nytårstaler fra 1972 til 2019. Hvis du uploader den til topic modeling skal du ikke ændre på indstillingerne for filformat."),
                         br(),
                         tags$hr(),
                         downloadButton("downloadData_corpus", label = "Download alle nytårstaler", style="color: #fff; background-color: orange; border-color: orange"), 
                         tags$hr(),
                         align = "center"
                  ),
                  column(width = 6,
                         br(),
                         h5("I opgave 10 skal du kun bruge en enkelt tale, nemlig nytårstalen fra 1999."),
                         br(),
                         br(),
                         tags$hr(),
                         downloadButton("downloadData_1999", label = "Download talen fra 1999",  style="color: #fff; background-color: orange; border-color: orange"),
                         tags$hr(),
                         align = "center")
              )
            ),
            
                    # Task 1
                     fluidRow(
                       box(title = strong("1. Hvilke ord optræder oftest i Dronningens nytårstaler?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Prøv at læse om NLPlays fire værktøjer på siden 'Introduktion og Overblik'."))),
                     fluidRow(
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("En word cloud er et godt sted at starte."))),
                     
                     # Task 2
                     fluidRow(
                       box(title = strong("2. Hvilke negativt ladede ord optræder færrest (eller kun få) gange på tværs af talerne?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Har du set at man kan farve ordene i word cloud generatoren?")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Hvis du holder musen over et ord i din word cloud, så kan du se hvor mange gange det ord optræder i din tekstsamling."))),
                     fluidRow(
                       box(title = strong("Hint nr. 3"), width = 12, collapsible = T, collapsed = T,
                           h5("Hvis du ikke kan finde ret mange negative ord, kan du prøve at se hvad der sker hvis du øger antallet af ord, som din word cloud skal vise."))
                     ),
                     
                     # Task 3
                     fluidRow(
                       box(title = strong("3. Hvor ofte optræder ordet “hjælpsomhed” over alle talerne?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Prøv at finde et sted hvor du kan søge i ordoptælling.")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Som en del af databehandling til Topic modeling får du en lille tabel, som viser hvor hyppigt ord optræder i din tekstsamling."))
                     ),
                     
                     # Task 4
                     fluidRow(
                       box(title = strong("4. Hvad betyder TF-IDF?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Det er noget man kan bruge når man vil lave en topic model.")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Når man kommer til den forberedende databehandling er der masser af hjælp at hente, både i instruktionsboksen og vha. 'i'-ikonerne."))
                     ),
                     
                     # Task 5
                     fluidRow(
                       box(title = strong("5. Hvordan adskiller Dronningens nytårstale i 1979 og 1989?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Her tænkes også i relation til de resterende år.")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Kan du huske hvad TF-IDF var for noget?"))),
                     fluidRow(
                       box(title = strong("Hint nr. 3"), width = 12, collapsible = T, collapsed = T,
                           h5("Hvis nogle af ordene ikke giver mening for dig kan du overveje at fjerne dem."))
                     ),
                     
                     # Task 6
                     fluidRow(
                       box(title = strong("6. Hvilke ord er mest betydningsfulde for Dronningens tale i år 2001, i relation til andre år?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("TF-IDF værdien er et udtryk for hvordan ordene i en tekst adskiller sig fra ord i de andre tekster.")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Synes du alle de definerende ord fra 2001 er meningsfulde?"))
                     ),
                     
                     # Task 7
                     fluidRow(
                       box(title = strong("7. Hvilket år taler Dronningen om klimatopmødet?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Her drejer det sig bare om at scrolle gennem visualiseringen"))
                     ),
                     
                     # Task 8
                     fluidRow(
                       box(title = strong("8. Kan du finde nogle meningsfulde gennemgående temaer/emner i Dronningens samlede taler?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Her skal du igennem alle funktionerne på topic modeling-siden. Vær opmærksom på at du kan folde TF-IDF visualiseringen sammen. Så er det nemmere at overskue siden.")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Læs instruktionerne til trin 3 grundigt. Og husk at gemme din databehandling inden."))),
                     fluidRow(
                       box(title = strong("Hint nr. 3"), width = 12, collapsible = T, collapsed = T,
                           h5("Når du har kørt en model, så se på beta-værdierne og overvej: Giver de enkelte emner mening? Adskiller de sig fra hinanden? Fortæller de dig noget, du kan bruge?")),
                       box(title = strong("Hint nr. 4"), width = 12, collapsible = T, collapsed = T,
                           h5("Du kan prøve med forskellige antal emner eller at udelukke flere ord. Det vil give dig andre resultater, men hvad du vil opleve er, at de ikke er særlig meningsfyldte. Det sker fordi selvom filen indeholder 48 taler, så er den faktisk for lille til at lave en god emnemodel af. Når du selv skal i gang er det derfor en god idé at prøve med større tekster - måske indtil flere romaner på én gang."))
                     ),
                     
                     # Task 9
                     fluidRow(
                       box(title = strong("9. Hvordan bruger Dronningen sproglig ladning i sine taler?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(
                       box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                           h5("Sproglig ladning kaldes også 'sentiment'.")),
                       box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                           h5("Når du ved at filen indeholder 48 taler kan du regne ud hvad den gennemsnitlige sentimentværdi er per tale ud fra den samlede værdi. Du kan også se på den gennemsnitlige sentimentværdi per ord."))
                     ),
                     
                     # Task 10
                     fluidRow(
                       box(title = strong("10. Hvor positivt/negativt ladet var Dronningens tale i 1999 set i forhold til den generelle tendens i talerne?"), width = 12, height = 40, background = "yellow")),
                     fluidRow(height = 200,
                              box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                                  h5("Det er her du skal have fat i den anden fil (1999).")),
                              box(title = strong("Hint nr. 2"), width = 12, collapsible = T, collapsed = T,
                                  h5("Prøv at sammenligne med dine resultater fra opgave 9."))),
                              
                    # Task 11
                    fluidRow(
                      box(title = strong("11. Prøv DAGW modellen af. Hvad kan den? Vælg et par interessante begreber fra Dronningens taler og udforsk deres betydning, associationer og forhold til hinanden."), width= 12, height = 60, background = "yellow")),
                    fluidRow(
                      box(title = strong("Hint nr. 1"), width = 12, collapsible = T, collapsed = T,
                          h5("Her er ingen hjælp at hente - bare fri leg!"))
            )
    )
  )
)


ui <- dashboardPage(title = "NLPlay", header, sidebar, body, skin = "yellow")

## ------------------------------------------------ DEFINE SERVER ------------------------------------------ ##
server <- function(input, output, session) {
  
  # WELCOME PAGE
  output$Sofie <- renderInfoBox({
      infoBox(
        "Applikationsudvikler", "SOFIE DITMER", "Aarhus Universitet | Cognitive Science", icon = icon("user-graduate", lib = "font-awesome"),
        color = "yellow", fill = TRUE
      )
    })
  
  output$Astrid <- renderInfoBox({
    infoBox(
      "Applikationsudvikler", "ASTRID NØRGAARD FONAGER", "Aarhus Universitet | Cognitive Science", icon = icon("user-graduate", lib = "font-awesome"),
      color = "yellow", fill = TRUE
    )
  })
  
  # INTRODUCTION PAGE
  output$DAGW_Model <- renderInfoBox({
    infoBox(
      "Værktøj 1", "DAGW-MODELLEN", "Udforsk semantiske sammenhænge", icon = icon("project-diagram", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
  output$Topic_Modeling <- renderInfoBox({
    infoBox(
      "Værktøj 2", "TOPIC MODELING", "Find emner i tekster", icon = icon("buffer", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
  output$WordCloud <- renderInfoBox({
    infoBox(
        "Værktøj 3", "WORD CLOUD GENERATOR", "Visualisér din tekst i en sky", icon = icon("cloud", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
  output$SentimentAnalysis <- renderInfoBox({
    infoBox(
      "Værktøj 4", "SENTIMENT ANALYSIS", "Udforsk stemningen i tekster", icon = icon("theater-masks", lib = "font-awesome"),
      color = "yellow", fill = TRUE,
    )
  })
  
# SENTIMENT ANALYSIS # 
  sa_data <- reactive({
    if (input$sa_source == "sa_own") {
      data <- input$sa_text
    } else if (input$sa_source == "sa_file") {
      if (is.null(input$sa_file)) {
        return("")
      }
      data <- readLines(input$sa_file$datapath, encoding = "UTF-8")
    }
    
    data <- as.data.frame(data)
    return(data)
    
  })
  
  output$sentiment_score_total <- renderText({
    if (input$sa_source == "sa_own"){
    total_sentiment <- sentida(req(input$sa_text), output = "total")
    return(total_sentiment)
    }
    
    if (input$sa_source == "sa_file"){
      sa_data_string <- readLines(req(input$sa_file$datapath), encoding = "UTF-8")
      sa_data_string <- as.String(sa_data_string)
      total_sentiment <- sentida(sa_data_string, output = "total")
      return(total_sentiment)
    }
  }) 
  
  output$sentiment_score_mean <- renderText({
    if (input$sa_source == "sa_own"){
      mean_sentiment <- sentida(req(input$sa_text), output = "mean")
      return(mean_sentiment)
    }
    if (input$sa_source == "sa_file"){
      sa_data_string <- readLines(req(input$sa_file$datapath), encoding = "UTF-8")
      sa_data_string <- as.String(sa_data_string)
      mean_sentiment <- sentida(sa_data_string, output = "mean")
      return(mean_sentiment)
    }
  })

  # Create a list of words and add column with sentiment
  sa_df <- reactive({
    x <- as.data.frame(sa_data())
    x$data <- tolower(x$data) 
    x$sentiment <- NA
    x <- x %>% unnest_tokens(word, data)
    
    for (row in 1:nrow(x)){
      x$sentiment[row] <- sentida(x$word[row])
    }
    
    x$category_senti <- ifelse(x$sentiment < 0, "Negativ", ifelse(x$sentiment > 0, "Positiv", "Neutral"))
    return(x)
  
  })
  
  senti_df <- reactive({
    senti_df <- sa_df() %>% 
      group_by(category_senti) %>% 
      summarise(score = n()) %>% 
      mutate(score_pct = score/sum(score)*100, coloract = c("#d35400", "#2ecc71", "#48a60d")) %>% 
      filter(category_senti != "Neutral")
  })
  
  # Sentiment Polarity Plot
  output$sentiment_polarity_plot <- renderHighchart({
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = senti_df()$category_senti,
               labels = list(style = list(fontSize= '12px'))
      )    %>%
      hc_colors(colors = senti_df()$coloract) %>% 
      hc_add_series(name="Sentiment", data = senti_df()$score_pct, colorByPoint = TRUE, 
                    type ="column",
                    color = "#4472c4", showInLegend= F) %>% 
      hc_yAxis(labels=list(format = '{value}%'), min=0,
               max=100, showFirstLabel = TRUE, showLastLabel=TRUE)
    return(hc)
  })
  
# THE DAGW MODEL #
  react_closest_words <- reactiveValues(closest_words = NULL)

  # Table Output: Similar or dissimilar words
  output$table_similar <- renderTable({
    
    closest_words <- predict(DAGW_model, newdata = req(input$sm_text1), type = "nearest")
    closest_words <- as.data.frame(closest_words)
    names(closest_words)[1] <- "Inputord"
    names(closest_words)[2] <- "Ord tættest på inputord"
    names(closest_words)[3] <- "Semantisk Lighed"
    names(closest_words)[4] <- "Rangering"
    react_closest_words$closest_words <- closest_words
    return(closest_words)
    
  })
  
  # Plot 1
  output$semantics_plot1 <- renderPlot({
      x <- predict(DAGW_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      y <- predict(DAGW_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      df <- word2vec_similarity(x, y)
      df <- as.data.frame(df)
      df2 <- data.matrix(df)
      dd <- dist(scale(df2), method = "euclidian")
      hc <- hclust(dd, method = "ward.D2")
      plot(as.phylo(hc), type = "unrooted", main = "Semantisk Netværk")
  })

  # Plot 2
  output$semantics_plot2 <- renderPlot({
      x <- predict(DAGW_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      y <- predict(DAGW_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      df <- word2vec_similarity(x, y)
      df <- as.data.frame(df)
      df2 <- data.matrix(df)
      dd <- dist(scale(df2), method = "euclidian")
      hc <- hclust(dd, method = "ward.D2")
      plot(as.phylo(hc), type = "fan", main = "Semantisk Netværk")
  })

  # Plot 3
  output$semantics_plot3 <- renderPlot({
      x <- predict(DAGW_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      y <- predict(DAGW_model, req(c(react_closest_words$closest_words[1,1], react_closest_words$closest_words[,2])), type = "embedding")
      df <- word2vec_similarity(x, y)
      df <- as.data.frame(df)
      df2 <- data.matrix(df)
      dd <- dist(scale(df2), method = "euclidian")
      hc <- hclust(dd, method = "ward.D2")
      hcd <- as.dendrogram(hc)
      plot(hcd, type = "triangle", axes = F, main = "Semantisk Netværk")
  })
  
  # Output: Distance between two words
  output$semanticdistance_output <- renderText({
    x <- predict(DAGW_model, req(input$semanticdistance_word1), type = "embedding")
    y <- predict(DAGW_model, req(input$semanticdistance_word2), type = "embedding")
    z <- word2vec_similarity(x, y)
    round(z[1,1], 2)
  })
  
  # Word calculation output
  observeEvent(input$calc_action, {
    vector_word1 <- predict(DAGW_model, newdata = req(input$word1_sm), type = "embedding")
    vector_word2 <- predict(DAGW_model, newdata = req(input$word2_sm), type = "embedding")
    vector_word3 <- predict(DAGW_model, newdata = input$word3_sm, type = "embedding")
    
    if (input$calc_source == "calc_option1"){
      new_vector <- vector_word1[,] + vector_word2[,]
    }
    
    if (input$calc_source == "calc_option2"){
      new_vector <- vector_word1[,] - vector_word2[,]
    }
    
    if (input$calc_source == "calc_option3"){
      new_vector <- vector_word1[,] - vector_word2[,] + vector_word3[,]
    }
    
    if (input$calc_source == "calc_option4"){
      new_vector <- vector_word1[,] + vector_word2[,] + vector_word3[,]
    }
    
    if (input$calc_source == "calc_option5"){
      new_vector <- vector_word1[,] + vector_word2[,] - vector_word3[,]
    }
    
    if (input$calc_source == "calc_option6"){
      new_vector <- vector_word1[,] - vector_word2[,] - vector_word3[,]
    }
    
    n <- as.numeric(input$calc_n) + 3
    predicted_word <- predict(DAGW_model, newdata = new_vector, type = "nearest", top_n = n)
    predicted_word <- as.data.frame(predicted_word)
    names(predicted_word)[1] <- "Ord"
    names(predicted_word)[2] <- "Semantisk Lighed"
    names(predicted_word)[3] <- "Rangering"
    
    df <- predicted_word %>% 
      filter(Ord != req(input$word1_sm), Ord != req(input$word2_sm), Ord != input$word3_sm) %>% 
      head(input$calc_n)
    df$Rangering <- c(1:input$calc_n)
    
    output$new_word_vector <- renderTable({df})
    
  })
  

# WORD CLOUD # 
  data_source <- reactive({
    if (input$source == "own") {
      wc_data <- input$text
    } else if (input$source == "file") {
      wc_data <- input_file()
    }
    return(wc_data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath, encoding = "UTF-8")
  })
  
  reactive_wc_data <- reactiveValues(data = NULL)
  
  create_wordcloud <- function(wc_data, num_words = 500, background = "white") {

    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(wc_data)) {
      corpus <- Corpus(VectorSource(wc_data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("Danish")))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove10))
      corpus <- tm_map(corpus, removeWords, c(input$wc_words_to_remove11))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      wc_data <- sort(rowSums(tdm), decreasing = TRUE)
      wc_data <- data.frame(word = names(wc_data), freq = as.numeric(wc_data))
    }
    
    # Create reactive variable for later use
    reactive_wc_data$data <- wc_data
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    wc_data <- head(wc_data, n = num_words)
    if (nrow(wc_data) == 0) {
      return(NULL)
    }

    # If the user chooses to color by sentiment
    if (input$sentiment_wc == TRUE){
      words_as_list <- list(as.character(wc_data[,1])) %>% 
        unlist()
      wc_data$sentiment <- NA
      for (i in 1:length(words_as_list)) {
        wc_data$sentiment[i] <- sentida(words_as_list[i], output = "total")
        words_as_list_reactive <- reactive({
          words_as_list
        })
      }
      
      # Color by sentiment
      # Credit: https://www.displayr.com/sentiment-word-clouds-r/ 
      n = nrow(wc_data)
      colors = rep("grey", n)
      colors[wc_data$sentiment < 0 & wc_data$sentiment > -1] = "#FFCACA"
      colors[wc_data$sentiment < -1 & wc_data$sentiment > -2] = "#FF9696"
      colors[wc_data$sentiment < -2 & wc_data$sentiment > -3] = "#FF5D5D"
      colors[wc_data$sentiment < -3 & wc_data$sentiment > -4] = "#FF2D2D"
      colors[wc_data$sentiment < -4 & wc_data$sentiment > -5] = "#FF0000"
      colors[wc_data$sentiment > 0 & wc_data$sentiment < 1] =  "#C9FFCB"
      colors[wc_data$sentiment > 1 & wc_data$sentiment < 2] =  "#9EFFA1"
      colors[wc_data$sentiment > 2 & wc_data$sentiment < 3] =  "#76FF7B"
      colors[wc_data$sentiment > 3 & wc_data$sentiment < 4] = "#3BFF41"
      colors[wc_data$sentiment > 4 & wc_data$sentiment < 5] =  "#00FF08"
      
      wordcloud2(wc_data, backgroundColor = background, color = colors)
      
    } else if (input$sentiment_wc == FALSE){
      wordcloud2(wc_data, backgroundColor = background, color = "orange")
    }
}
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
  
  output$wc_word_frequency_plot <- renderPlotly({
    frequency_ggpplot <- reactive_wc_data$data %>%
      filter(freq > 1) %>%
      top_n(10, freq) %>%
      arrange(desc(freq)) %>% 
      ggplot(aes(word, freq)) +
      geom_col(alpha = 1, show.legend = FALSE) +
      theme(text = element_text(size = 12)) +
      coord_flip() +
      scale_x_reordered() +
      labs(x = " ", y = "Hyppighed")
    
    ggplotly <- ggplotly(frequency_ggpplot)
    return(ggplotly)
  })
  

# TOPIC MODELING # 
  
  # Determining whether the input is manually inserted text or an uploaded file
  tm_data <- reactive({
    if (input$tm_source == "tm_own") {
      data <- tm_manual()
    } else if (input$tm_source == "tm_uploadfile") {
      data <- z$tm_input_file
    }
    
    data$text <- tolower(data$text)
    data <- data %>%
      unnest_tokens(word, text)
    
    # Remove stopwords
    if(input$stops == TRUE){
      data <- filter(data,
                     (!data$word %in%
                        list_of_stopwords))
    }
    
    # Remove unwanted words selected by the user
    if(input$remove_words2 == TRUE){
      user_stopwords <- strsplit(input$words_to_remove12, ", ") %>% unlist()
      data <- filter(data, 
                     (!data$word %in% 
                        user_stopwords))
    }
    
    # Stem words
    if(input$stem == TRUE){ # If the user chooses to stem the words
      data$word <- tm::stemDocument(as.character(data$word),"danish")
    }
    
    return(data)
  })
  
  # Submit uploaded file
  observeEvent(input$submitDataForUpload, {
    if (is.null(input$tm_file)) {
      return("")
    }
    tm_data <- read.csv(input$tm_file$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
    
    validate(need(ncol(tm_data) == 2, "Error reading the file"))
    
    tm_data[,2] <- gsub("[[:punct:]]", "", tm_data[,2]) 
    tm_data[,2] <- gsub("[[:digit:]]", "", tm_data[,2])
    colnames(tm_data) <- c("title", "text")
    
    z$tm_input_file <- tm_data
    
    output$upload_complete <- renderText({ "Din fil er nu uploadet!" })
  })
  
  # Manually inputed texts 
  tm_manual <- reactive({
    if (is.null(input$tmtext)) {
      return("")
    }
    
    tm_data1 <- data.frame(title = input$tm_title, text = input$tmtext)
    tm_data2 <- data.frame(title = input$tm_title2, text = input$tmtext2)
    tm_data3 <- data.frame(title = input$tm_title3, text = input$tmtext3)
    
    if(input$stem == TRUE) {
      tm_data1$text <- tm::stemDocument(as.character(tm_data1$text),"danish")
      tm_data2$text <- tm::stemDocument(as.character(tm_data2$text),"danish")
      tm_data3$text <- tm::stemDocument(as.character(tm_data3$text),"danish")
    }
    
    tm_data <- rbind(tm_data1,tm_data2, tm_data3)
    
    output$upload_complete <- renderText({ "Din fil er nu uploadet!" })
    
  })
  
  # Make input into data frame
  data_tf_idf <- reactive({
    data_tf_idf <- tm_data() %>%
      count(title, word, sort = TRUE) %>% 
      bind_tf_idf(word, title, n) %>% 
      arrange(-tf_idf) %>%
      group_by(title) %>%
      top_n(10) %>%
      ungroup()
    data_tf_idf <- as.data.frame(data_tf_idf)
  })
  
  # Data preview
  # output$tm_df <- renderTable({ 
  #   req(isTruthy(tm_data()))
  #   tm_data() %>% group_by(title) %>% 
  #   head(n = 10)
  # })
  
  # TF-IDF plot
  output$tfidf_plot <- renderPlotly({
    data_tf_idf <- data_tf_idf() %>%
      mutate(word = reorder_within(word, tf_idf, title))
    data_tf_idf2 <- data_tf_idf %>%
      group_by(title) %>%
      top_n(10, word)
    
    ggplot <- ggplot(data_tf_idf2, aes(x = word, y = tf_idf, fill = title)) +
      geom_col(alpha = 0.8, show.legend = F) +
      theme(axis.text.y = element_text(size = 12)) +
      facet_rep_wrap(~title, scales = "free", ncol = 1, repeat.tick.labels = TRUE) +
      coord_capped_cart(bottom = 'both', left = 'both') +
      labs(x = "", y = "TF-IDF") +
      scale_x_reordered() +
      coord_flip() +
      theme(strip.text = element_text(size = 12), legend.position = 'none')
    
    ggplotly <- ggplotly(ggplot, height = length(title)*input$height_tfidf_plot)
    return(ggplotly)
  })
  
  # Beta plot
  output$beta_plot <- renderPlotly({
    td_beta <- tidy(req(z$topic_model))
    
    ggplot_beta <- td_beta %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      mutate(topic = paste0("Emne ", topic),
             term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = as.factor(topic))) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      facet_rep_wrap(~ topic, scales = "free_y", ncol = 1, repeat.tick.labels = TRUE) +
      coord_capped_cart(bottom = 'both', left = 'both') +
      coord_flip() +
      theme(strip.text = element_text(size = 10), legend.position = 'none') +
      scale_x_reordered() +
      labs(x = "", y = "Betaværdi")
    
    ggplotly_beta <- ggplotly(ggplot_beta, height = length(title)*input$height_betaplot)
    ggplotly_beta
  })
  
  # Gamma plot
  output$gamma_plot <- renderPlotly({
    td_gamma <- tidy(req(z$topic_model), matrix = "gamma",                    
                     document_names = rownames(z$dfm))
    
    # Organiseret efter emner
    if (input$tm_plot_source == "rb_tops") {
      ggplot_gamma_tekster <- ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) + # ændr så der står "emne x"
        geom_histogram(alpha = 0.8, show.legend = FALSE) +
        facet_rep_wrap(~ topic, ncol = 1, repeat.tick.labels = TRUE) +
        coord_capped_cart(bottom = 'both', left = 'both') +
        labs(y = "Antal Tekster", x = "Gammaværdi") +
        theme(strip.text = element_text(size = 10), legend.position = 'none')
      
      ggplotly_gamma <- ggplotly(ggplot_gamma_tekster, height = length(title)*input$height_gammaplot)
      ggplotly_gamma
      } 
    
    # Organiseret efter teskter
    else {
      ggplot_gamma_emner <- ggplot(td_gamma, aes(x = as.factor(topic), y = gamma, fill = as.factor(document))) + 
        geom_col(alpha = 0.8, show.legend = FALSE) +
        facet_rep_wrap(~ document, ncol = 1, repeat.tick.labels = TRUE) +
        coord_capped_cart(bottom = 'both', left = 'both') +
        scale_x_reordered() +
        labs(y = "Gammaværdi", x = "Emne") +
        theme(strip.text = element_text(size = 10), legend.position = 'none')

      ggplotly_gamma <- ggplotly(ggplot_gamma_emner, height = length(title)*input$height_gammaplot)
      ggplotly_gamma
    }
  })
  
  # Word count
  output$tm_wordcount <- renderDT(
    DT::datatable(tm_WC(), 
                  options = list(pageLength = 5, searching = T))
  )
  
  tm_WC <- reactive({
    wc <- tm_data() %>% 
      count(word, sort = TRUE)
    names(wc)[1] <- "Ord"
    names(wc)[2] <- "Hyppighed"
    return(wc)
  })
  
  output$tf_idf <- renderTable({
    data_tf_idf()
  })
  
  # Create DFM
  z <- reactiveValues(dfm = NULL, topic_model = NULL, tm_input_file = NULL, tm_WC = NULL, sa_df = NULL)
  
  observeEvent(input$dfm_button, {
    data_dfm <- tm_data() %>%
      count(title, word, sort = TRUE) %>%
      cast_dfm(title, word, n)
    
    z$dfm <- dfm_trim(data_dfm, min_docfreq = input$minDoc)
    output$dfm_verbatim <- renderText({ "         Din databehandling er gemt!" })
  })
  
  observeEvent(input$topic_button, {
    req(isTruthy(z$dfm))
    k = input$num.topics
    dfm <- z$dfm
    
    topic_model <- stm(dfm, K = k, max.em.its = input$iter, init.type = "Spectral", verbose = TRUE)
    z$topic_model <- topic_model
    
    output$model_running <- renderText({"Modellen er kørt"})
  })
  
  # observeEvent(input$topic_button, {
  #   req(isTruthy(z$dfm))
  #   while (is.null(z$topic_model)){
  #     output$model_running <- renderText({"Vent mens modellen kører..."})    
  #   }
  #   
  #   k = input$num.topics
  #   dfm <- z$dfm
  #   
  #   topic_model <- stm(dfm, K = k, max.em.its = input$iter, init.type = "Spectral", verbose = TRUE)
  #   z$topic_model <- topic_model
  #   
  #   output$model_running <- renderText({"Modellen er kørt!"})
  # })
  
  ### Exercise ###
  # Downloadable csv of selected dataset ----
  output$downloadData_corpus <- downloadHandler(
    filename = function() {
      paste("Nytårstaler", ".csv", sep = "")
    },
    content = function(file) {
      write.csv("Nytårstaler-korpus.csv", file, row.names = FALSE)
    }
  )
  
  output$downloadData_1999 <- downloadHandler(
    filename = function() {
      paste("Nytårstale1999", ".txt", sep = "")
    },
    content = function(file) {
      myfile <- paste0('/NLPlay/www/', "1999.txt", collapse = NULL)
      file.copy(myfile, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
