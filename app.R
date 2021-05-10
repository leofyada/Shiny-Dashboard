#----------------

# Setup inicial -

#----------------
#### Importando as bibliotecas que serão utilizadas na análise ####
library(readxl) # leitura de bases de dados
library(data.table) # leitura de bases de dados
library(dplyr) # limpeza, manipulação e visualização de dados
library(ggplot2) # visualização de dados
library(plotly) # visualização interativa de dados
library(shiny) # criação de aplicação
library(shinydashboard) # layout de dashboard
#### Importação das bases de dados que serão utilizadas ####
inse_escolas <- read_excel("INSE_2019_ESCOLAS.xlsx")
#--------------------------------

# Tratamento das bases de dados -

#--------------------------------
#### Categorizando a localização das escolas ####
inse_escolas$TP_LOCALIZACAO <- factor(x = inse_escolas$TP_LOCALIZACAO, 
                                      levels = c(1, 2), 
                                      labels = c("Urbana", "Rural"))
#### Categorizando a dependência administrativa ####
inse_escolas$TP_DEPENDENCIA <- factor(x = inse_escolas$TP_DEPENDENCIA, 
                                      levels = c(1, 2, 3, 4),
                                      labels = c("Federal",
                                                 "Estadual",
                                                 "Municipal",
                                                 "Privada"))
#### Agregando dados de INSE por unidade federativa ####
inse_uf <- inse_escolas %>%
    group_by(CO_UF, NOME_UF) %>%
    summarise(MEDIA_INSE = mean(INSE_VALOR_ABSOLUTO), 
              SOMA_ALUNOS_INSE = sum(QTD_ALUNOS_INSE),
              .groups = "drop_last")
#### Criando uma lista com as unidades federativas ####
uf_list <- inse_escolas %>%
    select(NOME_UF) %>%
    arrange(NOME_UF) %>%
    unique()
#--------------------------

# Construção do dashboard -

#--------------------------
#### User Interface (UI) ####
ui <- dashboardPage(
    dashboardHeader(title = "Painel de Dados"),
    dashboardSidebar(
        selectInput(
            inputId = "uf",
            label = "Unidade Federativa:",
            choices = uf_list,
            selected = "São Paulo",
            selectize = FALSE
        )
    ),
    dashboardBody(
        fluidRow(
            valueBoxOutput("mean"),
            valueBoxOutput("sd"),
            valueBoxOutput("count")
        )
        ,
        fluidRow(
            shinydashboard::box(plotlyOutput("plot1", height = 300)),
            shinydashboard::box(plotlyOutput("plot2", height = 300))
        ),
        fluidRow(
            shinydashboard::box(plotlyOutput("plot3", height = 300)),
            shinydashboard::box(plotlyOutput("plot4", height = 300))
        )
    )
)

#### Server Side ####
server <- function(input, output) {
    
    data <- reactive({inse_escolas %>%
            dplyr::filter(NOME_UF == input$uf) 
    }) %>%
        bindCache(input$uf)
    
    output$mean <- renderValueBox({
        
        valueBox(round(mean(data()$INSE_VALOR_ABSOLUTO), digits = 2), 
                 "Média do INSE",  
                 icon = icon("balance-scale"),
                 color = "purple")
    }) 
    
    output$sd <- renderValueBox({
        
        valueBox(round(sd(data()$INSE_VALOR_ABSOLUTO), digits = 2), 
                 "Desvio Padrão do INSE", 
                 icon = icon("balance-scale-right"),
                 color = "green")
    }) 
    
    output$count <- renderValueBox({
        
        valueBox(count(data()), 
                 "Quantidade de escolas", 
                 icon = icon("calculator"),
                 color = "yellow")
    }) 
    
    output$plot1 <- renderPlotly({
        h <- ggplot(data(), aes(x = INSE_VALOR_ABSOLUTO))
        plot_1 <- h + geom_histogram(bins = 30, 
                                     col = "dark blue", 
                                     fill = "blue") +
            ggtitle("Distribuição de INSE") +
            xlab("INSE (valor absoluto)") +
            ylab("Frequência") + 
            theme(plot.title = element_text(size = 20))
        
        ggplotly(plot_1)
        
    }) %>%
        bindCache(data())
    
    output$plot2 <- renderPlotly({
        p <- ggplot(data(), aes(x = INSE_VALOR_ABSOLUTO,
                                y = QTD_ALUNOS_INSE)) +
            ggtitle("Relação entre INSE e qtd de alunos") +
            xlab("INSE (valor absoluto)") +
            ylab("Quantidade de alunos") + 
            theme(plot.title = element_text(size = 20))
        plot_2 <- p + geom_jitter(col = "red")
        
        ggplotly(plot_2)

    }) %>%
        bindCache(data())
    
    output$plot3 <- renderPlotly({
        f <- ggplot(data(), aes(x = TP_LOCALIZACAO,
                                y = INSE_VALOR_ABSOLUTO,
                                group = TP_LOCALIZACAO))
        plot_3 <- f + geom_boxplot(fill = "slateblue") + 
            ggtitle("Boxplot - INSE (por localização)") + 
            xlab("Localização") +
            ylab("INSE (valor absoluto)") +
            theme(plot.title = element_text(size = 20))
        
        ggplotly(plot_3)
        
    }) %>%
        bindCache(data())
    
    output$plot4 <- renderPlotly({
        g <- ggplot(data(), aes(x = TP_DEPENDENCIA,
                              y = INSE_VALOR_ABSOLUTO,
                              group = TP_DEPENDENCIA))
        plot_4 <- g + geom_boxplot(fill = "slateblue") + 
            ggtitle("Boxplot - INSE (por dep. adm.)") + 
            xlab("Dependência administrativa") +
            ylab("INSE (valor absoluto)") +
            theme(plot.title = element_text(size = 20))
        
        ggplotly(plot_4)
        
    }) %>%
        bindCache(data())
}

#### App ####
shinyApp(ui, server)