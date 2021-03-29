### NASH Treatment benefit ###


library(shinydashboard)
library(tidyverse)
library(patchwork)


#############  Data prep  ########

textLRM <- 
    tibble(
        time = c(0.3, 0.3),
        est = c(21, 25),
        RxIncidence = c(20, 25),
        label = c("New treatment", "Current lifestyle intervention")
)

textNonLRM <- 
    tibble(
        time = c(0.3, 0.3),
        est = c(21, 25),
        RxIncidence = c(20, 25),
        label = c("New treatment", "Current lifestyle intervention")
    )

layout <- 
    c(area(1, 1, 12, 12), area(1, 2, 7, 12))

# MORTALITY
## read CmpRsk data to plot
F2plot <- read_csv("F2mortality.csv")

F3plot <- read_csv("F3mortality.csv") 

F4plot <- read_csv("F4mortality.csv")


F2tableData <- read_csv("F2tableData.csv")

F3tableData <- read_csv("F3tableData.csv")

F4tableData <- read_csv("F4tableData.csv")



# MORBIDITY
## read CmpRsk data to plot

F2morbid <- read_csv("F2morbidity.csv")

F3morbid <- read_csv("F3morbidity.csv") 

F4morbid <- read_csv("F4morbidity.csv")


###############
# Formatting

## Theme set

appTheme <- 
    theme_classic() +
    theme(text = element_text(face = "bold"))

theme_set(appTheme)

# App

ui <- dashboardPage(
    
    dashboardHeader(disable = TRUE,
                    title = "Estimating the benefits of treatment in NASH"
                    ),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
        
        tags$style(
            type = 'text/css', 
            '.btn-box-tool {color: #FFFFFF!important; }'
        ),
        
        fluidRow(
            column(width = 3,
                   box(title = strong("Estimating benefits of treatment in NASH"),
                       solidHeader = TRUE,
                       background = "light-blue",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       "Individuals with NASH are at risk of death from both liver disease, and more commonly heart disease.
                       Treatments in development for NASH may improve outcomes through reductions in liver related mortality and / or 
                       cardiovascular disease related deaths.
                       This tool illustrates the impact of drug treatment on outcomes in patients with NASH.",
                       br(),
                       br(),
                       "Outcomes illustrated are for patients at 55 years of age starting treatment at fibrosis stage 2, 3, or 4.", 
                       width = NULL
                    ),
                   
                   fluidRow(
                       box(title = strong("Mortality risk"),
                           solidHeader = TRUE,
                           background = "blue",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           "The risk that a patient will die from liver disease. 
                           The baseline value is the current best estimate - see background information for further details.",
                           width = 6
                        ),
                       
                       box(title = strong("Age"),
                           solidHeader = TRUE,
                           background = "blue",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           "The age of patients starting treatment.",
                           width = 6
                       )
                   ),
                   
                   fluidRow(
                       box(radioButtons(
                               "mortality",
                               label = "Liver related mortality:",
                               choices = list("Baseline" = "Base case",
                                             "Decrease 25%" = "Low",
                                             "Increase 50%" = "High"),
                               selected = "Base case",
                               inline = FALSE
                           ),
                           width = 6
                       ),
                       
                       box(radioButtons(
                           "age",
                           label = "Age at start of treatment:",
                           choices = list("45 years" = 45,
                                          "55 years" = 55,
                                          "65 years" = 65),
                           selected = 55,
                           inline = FALSE
                          ),
                           width = 6
                       )
                   ),
                   
                   
                   box(title = strong("Treatment effects"),
                       solidHeader = TRUE,
                       background = "aqua",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       "Treatment benefit is achieved through either prevention of progressive liver disease, 
                       improved cardiometabolic risk, or both.", 
                       br(),
                       br(),
                       "In the REGENERATE trial, the relative risk reduction in progression was 30%. 
                       In prevention of cardiovascular disease, relative risk reductions in mortality are typically 10-15%. See background information for further details.",
                       width = NULL
                    ),
                   
                   box(selectInput("treatmentLiver",
                                   "Select relative risk reduction in liver related mortality:",
                                   choices = 
                                       c("None" = 0,
                                         "10%" = 0.1,
                                         "20%" = 0.2,
                                         "30%" = 0.3,
                                         "40%" = 0.4,
                                         "50%" = 0.5,
                                         "60%" = 0.6,
                                         "70%" = 0.7),
                                   selected = 0.3),

                      
                          selectInput("treatmentCVD",
                                       "Select change in relative risk of CVD related mortality:",
                                       choices = 
                                           c("15% reduction" = 0.85,
                                             "10% reduction" = 0.9,
                                             "5% reduction" = 0.95,
                                             "None" = 1,
                                             "5% increase" = 1.05,
                                             "10% increase" = 1.10),
                                      selected = 1),
                       width = NULL
                       ),
                   
                   
                   box(title = strong("Further information"),
                       "The estimates explored here were developed to understand the potential impact of new therapies for NASH.
                        The models developed are calibrated to existing data sets - see background information tab.",
                       br(),
                       br(),
                       icon("envelope"), a(" i.a.c.rowe@leeds.ac.uk", href = ("mailto:i.a.c.rowe@leeds.ac.uk")),  
                       br(),
                       icon("twitter"), a(" @IanARowe", href = "https://twitter.com/IanARowe", target = "_blank"),
                       br(),
                       icon("linkedin"), a(" Ian Rowe", href = "https://www.linkedin.com/in/ian-rowe-9981708a", target = "_blank"),
                       br(),
                       icon("chrome"), a(" rowe-liver.com", href = "https://www.rowe-liver.com", target = "_blank"),
                       width = NULL
                   )
                   
            ),
            
            
            column(width = 9,
                   tabBox(title = " ", 
                          width = NULL,
                          tabPanel("Mortality outcomes", icon = icon("tablets"),
                                   fluidRow(
                                       box(title = strong("Fibrosis stage 2"), 
                                           solidHeader = TRUE,
                                           strong("Liver related deaths"),
                                           plotOutput("F2plot", height = 270),
                                           br(),
                                           strong("Cardiovascular deaths"),
                                           plotOutput("F2overall", height = 270),
                                           br(),
                                           strong("10 year mortality benefit"),
                                           tableOutput("F2table"),
                                           br(),
                                           "Negative values indicate harm from treatment",
                                           width = 4),
                                       box(title = strong("Fibrosis stage 3"),
                                           solidHeader = TRUE,
                                           strong("Liver related deaths"),
                                           plotOutput("F3plot", height = 270),
                                           br(), 
                                           strong("Cardiovascular deaths"),
                                           plotOutput("F3overall", height = 270),
                                           br(),
                                           strong("10 year mortality benefit"),
                                           tableOutput("F3table"),  
                                           br(),
                                           "Negative values indicate harm from treatment",
                                           width = 4),
                                       box(title = strong("Fibrosis stage 4"),
                                           solidHeader = TRUE,
                                           strong("Liver related deaths"),
                                           plotOutput("F4plot", height = 270),
                                           br(), 
                                           strong("Cardiovascular deaths"),
                                           plotOutput("F4overall", height = 270),
                                           br(),
                                           strong("10 year mortality benefit"),
                                           tableOutput("F4table"),
                                           br(),
                                           "Negative values indicate harm from treatment",
                                           width = 4)
                                   )
                          ),
                          
                          tabPanel("Morbidity outcomes", icon = icon("tablets"),
                                   fluidRow(
                                       box(title = strong("Fibrosis stage 2"), 
                                           solidHeader = TRUE,
                                           strong("Decompensation"),
                                           plotOutput("F2decomp", height = 270),
                                           br(),
                                           strong("HCC"),
                                           plotOutput("F2hcc", height = 270),
                                           width = 4),
                                       box(title = strong("Fibrosis stage 3"),
                                           solidHeader = TRUE,
                                           strong("Decompensation"),
                                           plotOutput("F3decomp", height = 270),
                                           br(), 
                                           strong("HCC"),
                                           plotOutput("F3hcc", height = 270),
                                           width = 4),
                                       box(title = strong("Fibrosis stage 4"),
                                           solidHeader = TRUE,
                                           strong("Decompensation"),
                                           plotOutput("F4decomp", height = 270),
                                           br(), 
                                           strong("HCC"),
                                           plotOutput("F4hcc", height = 270),                                
                                           width = 4)
                                   )
                          ),
                          
                          tabPanel("Background information", icon = icon("file-alt"),
                                   fluidRow(
                                       box(title = strong("Fibrosis stage and liver related mortality risk"),
                                           solidHeader = TRUE,
                                           strong("Fibrosis stage"),
                                           br(),
                                           "The critical factor defining the risk of future liver-related mortality is fibrosis stage (",
                                           a("Taylor et al, Gastroenterology 2020", href = "https://pubmed.ncbi.nlm.nih.gov/32027911/", target = "_blank"), "& ",
                                           a("Dulai et al, Hepatology 2017", href = "https://pubmed.ncbi.nlm.nih.gov/28130788/", target = "_blank"),
                                           "). With minimal fibrosis, stages 0 or 1, the risk of future liver-related mortality is not increased above that of the general population. 
                                            As fibrosis progresses the risk of liver-related mortality increases substantially such that patients with cirrhosis (fibrosis stage 4) are at high risk.",
                                           br(),
                                           br(),
                                           "For this reason, large phase 3 randomised clinical trials of drugs in development for NASH have focused on two populations: 
                                            those with significant fibrosis (F2/3), and those with cirrhosis (F4).",
                                           br(),
                                           br(),
                                           strong("Risk of liver-related mortality and morbidity"),
                                           br(),
                                           "It is recognised that liver-related deaths are less frequent that deaths due to cardiovascular disease and non-liver cancer in individuals with NASH (",
                                           a("Angulo et al, Gastroenterology 2015", href = "https://pubmed.ncbi.nlm.nih.gov/25935633/", target = "_blank"), "& ",
                                           a("Hagstrom et al, Journal of Hepatology 2017", href = "https://pubmed.ncbi.nlm.nih.gov/28803953/", target = "_blank"),
                                            "). The presents a challenge to treatments that aim to prevent progression of liver disease, 
                                            since the majority of patients who are not destined to die from liver disease may not benefit from treatment.",
                                           br(),
                                           br(),
                                           "In these calculations the current estimate is taken from the various published series above
                                           such that the survival outcomes are calibrated to those observed in a contemporary  patient population (",
                                           a("Hagstrom et al, Journal of Hepatology 2017", href = "https://pubmed.ncbi.nlm.nih.gov/28803953/", target = "_blank"),
                                           "). The tool allows for these to be increased by 50% (high) or decreased by 25% (low) to explore variation in baseline risk.",
                                           br(),
                                           br(),
                                           "Estimates of the incidence of decompensation and hepatocellular carcinoma (HCC) were informed by rates ovbserved in primary studies (",
                                           a("Vilar- Gomez et al, Gastroenterology 2018", href = "https://pubmed.ncbi.nlm.nih.gov/29733831/", target = "_blank"),
                                           ") and calibrated to the mortality outcomes described above.",
                                           br(),
                                           br(),
                                           strong("Additional information"),
                                           br(),
                                           "The paradigm of treatment for liver diseases has typically been seen from the individual perspective.
                                           NAFLD is a population level disease and considering treatment in that light suggests comparisons with cardiovascular disease (",
                                           a("Rowe & Parker, Nature Reviews Gastroenterology & Hepatology", 
                                             href = "https://drive.google.com/file/d/1iA6yRKvdoJcU8-51T5C7wnxqBH-vK5Xi/view?usp=sharing", target = "_blank"),
                                           ").",
                                           
                                           width = 6),
                                     
                                       box(title = strong("Treatment outcomes"),
                                           solidHeader = TRUE,
                                           strong("Impact on liver-related mortality"),
                                           br(),
                                           "To date, no phase 3 trial with a clinically relevant endpoint has reported.  
                                            The REGENERATE trial provides some information regarding the possible benefits of treatment in patients without cirrhosis (",
                                           a("Younossi et al, Lancet 2019", href = "https://pubmed.ncbi.nlm.nih.gov/31813633/", target = "_blank"),
                                            "). In this study the proportion of patients where fibrosis progressed was 21% in the placebo arm vs. 13% in the obeticholic acid 25mg arm in a per protocol analysis.
                                            This suggests an approximately 30% relative risk reduction in liver disease progression that might translate 
                                            to a reduction in liver-related mortality of a similar magnitude.
                                            There is no comparable trial reporting favourable changes in surrogate or clinical endpoints in cirrhosis and the impact of treatment in this important group is uncertain.",
                                           br(),
                                           br(),
                                           "The tool allows a wide range of values, from 10% to 70% relative risk reduction in liver-related mortality to be explored across fibrosis stages.",
                                           br(),
                                           br(),
                                           strong("Impact on cardiometabolic comorbidity"),
                                           br(),
                                           "The shared risk factors of obesity, type 2 diabetes, and hypertension explain at least part of the increased risk of cardiovascular disease in persons with NASH.
                                            A treatment for NASH that also addressed aspects of this metabolic multimorbidity complex is likely to have important additional benefits for patients.
                                            For instance, drugs under investigation in NASH including the GLP-1 receptor agonists have been shown to reduce 
                                            cardiovascular risk in persons at high risk but without overt liver disrease (",
                                           a("Kristensen et al, Lancet Diabetes & Endocrinology 2019", href = "https://www.ncbi.nlm.nih.gov/pubmed/31422062", target = "_blank"),
                                            "). The relative risk reduction of mortality in those trials is in the region of 10-15% though it should be recognised that this is largely in a secondary prevention population.",
                                           br(),
                                           br(),
                                           "For comparison purposes, treatment of persons at high cardiovascular risk in the primary prevention setting with a statin 
                                            results in a relative risk reduction of 15% in cardiovascular mortality (",
                                           a("Cholesterol Treatment Trialists, Lancet 2015", href = "https://pubmed.ncbi.nlm.nih.gov/25579834/", target = "_blank"),
                                            ").",
                                           br(),
                                           br(),
                                           "To explore the relative benefits of treatment that aims to reduce cardiovascular risk in comparison to risk from progressive liver disease 
                                           it is possible to estimate the impact of each of these treatments in isolation and in combination.",
                                           width = 6),
                                       
                                   )
                          )
                   )
            ),
            
            
        )
    )
)



server <- function(input, output) {
    
    
    
    output$F2plot <- renderPlot({
        
        F2_liver_data <- 
          F2plot %>%
          filter(
            Cause == "Liver",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver,
            CVDRx == input$treatmentCVD
          )
      
        F2full <-
            ggplot(F2_liver_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            ylim(c(0, 100)) +
            labs(x = "Years", 
                 y = "Liver related mortality (%)")
        
        F2zoom <-
            ggplot(F2_liver_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            scale_y_continuous(
                breaks = c(0, 10, 20),
                limits = c(0, 25)) +
            labs(x = "", 
                 y = "") +
            geom_text(
                data = textLRM,
                aes(x = time, y = est),
                label = c("New drug", "Current lifestyle intervention"),
                colour = c("skyblue", "deepskyblue4"),
                fontface = "bold",
                hjust = 0)
        
        F2full + F2zoom + plot_layout(design = layout)
        
    })
    
    output$F3plot <- renderPlot({
        
        F3_liver_data <-
          F3plot %>%
          filter(
            Cause == "Liver",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver,
            CVDRx == input$treatmentCVD)
      
        F3full <-
            ggplot(F3_liver_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            ylim(c(0, 100)) +
            labs(x = "Years", 
                 y = "Liver related mortality (%)") 
        
        F3zoom <-
            ggplot(F3_liver_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            scale_y_continuous(
                breaks = c(0, 10, 20),
                limits = c(0, 25)) +
            labs(x = "", 
                 y = "") +
            geom_text(
                data = textLRM,
                aes(x = time, y = est),
                label = c("New drug", "Current lifestyle intervention"),
                colour = c("skyblue", "deepskyblue4"),
                fontface = "bold",
                hjust = 0)
        
        F3full + F3zoom + plot_layout(design = layout)
        
    })
    
    output$F4plot <- renderPlot({
        
        F4_liver_data <-
          F4plot %>%
          filter(
            Cause == "Liver",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver,
            CVDRx == input$treatmentCVD)
        
        F4full <-
            ggplot(F4_liver_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            ylim(c(0, 100)) +
            labs(x = "Years", 
                 y = "Liver related mortality (%)") 
        
        F4zoom <-
            ggplot(F4_liver_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            scale_y_continuous(
                breaks = c(0, 10, 20),
                limits = c(0, 25)) +
            labs(x = "", 
                 y = "") +
            geom_text(
                data = textLRM,
                aes(x = time, y = est),
                label = c("New drug", "Current lifestyle intervention"),
                colour = c("skyblue", "deepskyblue4"),
                fontface = "bold",
                hjust = 0)
        
        F4full + F4zoom + plot_layout(design = layout)
        
    })
    
    
    output$F2overall <- renderPlot({
        
        F2_other_data <-
          F2plot %>%
          filter(
            Cause == "Other",
            Risk == input$mortality,
            Age == input$age,
            CVDRx == input$treatmentCVD,
            LiverRx == input$treatmentLiver
          )
      
        F2_cvd <-
          ggplot(F2_other_data) +
          geom_smooth(
              aes(x = time, y = estRx),
              colour = "skyblue", lwd = 1) +
          geom_smooth(
              aes(x = time, y = est * 0.45),
              colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 100)) +
          labs(x = "Years", 
               y = "CVD related mortality (%)")
        
        F2zoom_cvd <-
          ggplot(F2_other_data) +
          geom_smooth(
            aes(x = time, y = estRx),
            colour = "skyblue", lwd = 1) +
          geom_smooth(
            aes(x = time, y = est * 0.45),
            colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          scale_y_continuous(
            breaks = c(0, 10, 20),
            limits = c(0, 25)) +
          labs(x = "", 
               y = "") +
          geom_text(
            data = textNonLRM,
            aes(x = time, y = est),
            label = c("New drug", "Current lifestyle intervention"),
            colour = c("skyblue", "deepskyblue4"),
            fontface = "bold",
            hjust = 0)
        
        F2_cvd + F2zoom_cvd + plot_layout(design = layout)
        
    })
    
    output$F3overall <- renderPlot({
        
        F3_other_data <-
          F3plot %>%
          filter(
            Cause == "Other",
            Risk == input$mortality,
            Age == input$age,
            CVDRx == input$treatmentCVD,
            LiverRx == input$treatmentLiver)
        
        F3_cvd <-
          ggplot(F3_other_data) +
          geom_smooth(
            aes(x = time, y = estRx),
            colour = "skyblue", lwd = 1) +
          geom_smooth(
            aes(x = time, y = est * 0.4),
            colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 100)) +
          labs(x = "Years", 
               y = "CVD related mortality (%)")
        
        F3zoom_cvd <-
          ggplot(F3_other_data) +
          geom_smooth(
            aes(x = time, y = estRx),
            colour = "skyblue", lwd = 1) +
          geom_smooth(
            aes(x = time, y = est * 0.4),
            colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          scale_y_continuous(
            breaks = c(0, 10, 20),
            limits = c(0, 25)) +
          labs(x = "", 
               y = "") +
          geom_text(
            data = textNonLRM,
            aes(x = time, y = est),
            label = c("New drug", "Current lifestyle intervention"),
            colour = c("skyblue", "deepskyblue4"),
            fontface = "bold",
            hjust = 0)
        
        F3_cvd + F3zoom_cvd + plot_layout(design = layout)
        
    })
    
    output$F4overall <- renderPlot({
        
        F4_other_data <-
          F4plot %>%
          filter(
            Cause == "Other",
            Risk == input$mortality,
            Age == input$age,
            CVDRx == input$treatmentCVD,
            LiverRx == input$treatmentLiver)
      
        F4_cvd <-
          ggplot(F4_other_data) +
          geom_smooth(
            aes(x = time, y = estRx),
            colour = "skyblue", lwd = 1) +
          geom_smooth(
            aes(x = time, y = est * 0.3),
            colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 100)) +
          labs(x = "Years", 
               y = "CVD related mortality (%)")
        
        F4zoom_cvd <-
          ggplot(F4_other_data) +
          geom_smooth(
            aes(x = time, y = estRx),
            colour = "skyblue", lwd = 1) +
          geom_smooth(
            aes(x = time, y = est * 0.3),
            colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          scale_y_continuous(
            breaks = c(0, 10, 20),
            limits = c(0, 25)) +
          labs(x = "", 
               y = "") +
          geom_text(
            data = textNonLRM,
            aes(x = time, y = est),
            label = c("New drug", "Current lifestyle intervention"),
            colour = c("skyblue", "deepskyblue4"),
            fontface = "bold",
            hjust = 0)
        
        F4_cvd + F4zoom_cvd + plot_layout(design = layout)
        
    })
        
    output$F2table <- renderTable({
        
        Liver <-
            F2tableData %>%
            filter(Age == input$age,
                   Risk == input$mortality,
                   LiverRx == input$treatmentLiver,  
                   CVDRx == input$treatmentCVD,
                   Cause == "Liver") %>%  
            group_by(Risk, LiverRx, CVDRx) %>%
            arrange(time) %>%
            summarise_all(list(first)) %>% 
            mutate(ARR = est - estRx 
                   ) %>%  # NNT = as.integer(100 / ARR)
            ungroup() %>% 
            select(Cause, ARR) 
        
        
        nonLiver <-
            F2tableData %>%
            filter(Age == input$age,
                   Risk == input$mortality,
                   LiverRx == input$treatmentLiver,  
                   CVDRx == input$treatmentCVD,
                   Cause == "Other") %>%  
            group_by(Risk, LiverRx, CVDRx) %>%
            arrange(time) %>%
            summarise_all(list(first)) %>% 
            mutate(ARR = est * 0.45 - estRx 
            ) %>%  # NNT = as.integer(100 / ARR)
            ungroup() %>% 
            select(Cause, ARR)
        
        
        a <- 
            bind_rows(Liver, nonLiver)
        
        b <- 
            a %>%
            summarise("Absolute risk reduction (%)" = sum(ARR)) %>%
            mutate(Cause = "Overall")
        
        bind_rows(
            Liver,
            nonLiver) %>%
            rename("Absolute risk reduction (%)" = ARR) %>%
            bind_rows(
                b) %>%
            mutate("Number needed to treat" = as.integer(100 / `Absolute risk reduction (%)`))
        
        
    }, digits = 2)
    
    
    output$F3table <- renderTable({
        
        Liver <-
            F3tableData %>%
            filter(Age == input$age,
                   Risk == input$mortality,
                   LiverRx == input$treatmentLiver,  
                   CVDRx == input$treatmentCVD,
                   Cause == "Liver") %>%  
            group_by(Risk, LiverRx, CVDRx) %>%
            arrange(time) %>%
            summarise_all(list(first)) %>% 
            mutate(ARR = est - estRx 
            ) %>%  # NNT = as.integer(100 / ARR)
            ungroup() %>% 
            select(Cause, ARR) 
        
        
        nonLiver <-
            F3tableData %>%
            filter(Age == input$age,
                   Risk == input$mortality,
                   LiverRx == input$treatmentLiver,  
                   CVDRx == input$treatmentCVD,
                   Cause == "Other") %>%  
            group_by(Risk, LiverRx, CVDRx) %>%
            arrange(time) %>%
            summarise_all(list(first)) %>% 
            mutate(ARR = est * 0.4 - estRx 
            ) %>%  # NNT = as.integer(100 / ARR)
            ungroup() %>% 
            select(Cause, ARR)
        
        
        a <- 
            bind_rows(Liver, nonLiver)
        
        b <- 
            a %>%
            summarise("Absolute risk reduction (%)" = sum(ARR)) %>%
            mutate(Cause = "Overall")
        
        bind_rows(
            Liver,
            nonLiver) %>%
            rename("Absolute risk reduction (%)" = ARR) %>%
            bind_rows(
                b) %>%
            mutate("Number needed to treat" = as.integer(100 / `Absolute risk reduction (%)`))
        
        
    }, digits = 2)
    
    output$F4table <- renderTable({
        
        Liver <-
            F4tableData %>%
            filter(Age == input$age,
                   Risk == input$mortality,
                   LiverRx == input$treatmentLiver,  
                   CVDRx == input$treatmentCVD,
                   Cause == "Liver") %>%  
            group_by(Risk, LiverRx, CVDRx) %>%
            arrange(time) %>%
            summarise_all(list(first)) %>% 
            mutate(ARR = est - estRx 
            ) %>%  # NNT = as.integer(100 / ARR)
            ungroup() %>% 
            select(Cause, ARR) 
        
        
        nonLiver <-
            F4tableData %>%
            filter(Age == input$age,
                   Risk == input$mortality,
                   LiverRx == input$treatmentLiver,  
                   CVDRx == input$treatmentCVD,
                   Cause == "Other") %>%  
            group_by(Risk, LiverRx, CVDRx) %>%
            arrange(time) %>%
            summarise_all(list(first)) %>% 
            mutate(ARR = est * 0.3 - estRx 
            ) %>%  # NNT = as.integer(100 / ARR)
            ungroup() %>% 
            select(Cause, ARR)
        
        
        a <- 
            bind_rows(Liver, nonLiver)
        
        b <- 
            a %>%
            summarise("Absolute risk reduction (%)" = sum(ARR)) %>%
            mutate(Cause = "Overall")
        
        bind_rows(
            Liver,
            nonLiver) %>%
            rename("Absolute risk reduction (%)" = ARR) %>%
            bind_rows(
                b) %>%
            mutate(
              "Number needed to treat" = as.integer(100 / `Absolute risk reduction (%)`))
        
        
    }, digits = 2)
    
    output$F2decomp <- renderPlot({
        
        F2_decomp_data <- 
          F2morbid %>%
          filter(
            Cause == "Decompensation",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver)
      
        ggplot(F2_decomp_data) +
          geom_smooth(
              aes(x = time, y = estRx),
              colour = "skyblue", lwd = 1) +
          geom_smooth(
              aes(x = time, y = est),
              colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 30)) +
          labs(x = "Years", 
               y = "Decompensation (%)") +
        geom_text(
          data = textLRM,
          aes(x = time, y = est),
          label = c("New drug", "Current lifestyle intervention"),
          colour = c("skyblue", "deepskyblue4"),
          fontface = "bold",
          hjust = 0)
        
    })   
    
    
    output$F2hcc <- renderPlot({
        
        F2_hcc_data <-
          F2morbid %>%
          filter(
            Cause == "HCC",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver)
      
        ggplot(F2_hcc_data) +
          geom_smooth(
              aes(x = time, y = estRx),
              colour = "skyblue", lwd = 1) +
          geom_smooth(
              aes(x = time, y = est),
              colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 20)) +
          labs(x = "Years", 
               y = "Hepatocellular carcinoma (%)")
      
    }) 
    
    output$F3decomp <- renderPlot({
        
        F3_decomp_data <-
          F3morbid %>%
          filter(
            Cause == "Decompensation",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver)
        
        ggplot(F3_decomp_data) +
          geom_smooth(
              aes(x = time, y = estRx),
              colour = "skyblue", lwd = 1) +
          geom_smooth(
              aes(x = time, y = est),
              colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 30)) +
          labs(x = "Years", 
               y = "Decompensation (%)") +
          geom_text(
            data = textLRM,
            aes(x = time, y = est),
            label = c("New drug", "Current lifestyle intervention"),
            colour = c("skyblue", "deepskyblue4"),
            fontface = "bold",
            hjust = 0)
        
    })   
    
    
    output$F3hcc <- renderPlot({
        
        F3_hcc_data <-
          F3morbid %>%
          filter(
            Cause == "HCC",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver)
      
        ggplot(F3_hcc_data) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            ylim(c(0, 20)) +
            labs(x = "Years", 
                 y = "Hepatocellular carcinoma (%)")
        
    }) 
    
    output$F4decomp <- renderPlot({
        
        F4_decomp_data <-
          F4morbid %>%
          filter(
            Cause == "Decompensation",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver)
      
        ggplot(F4_decomp_data) +
          geom_smooth(
              aes(x = time, y = estRx),
              colour = "skyblue", lwd = 1) +
          geom_smooth(
              aes(x = time, y = est),
              colour = "deepskyblue4", lwd = 1) +
          xlim(c(0, 10)) +
          ylim(c(0, 30)) +
          labs(x = "Years", 
               y = "Decompensation (%)") +
        geom_text(
          data = textLRM,
          aes(x = time, y = est),
          label = c("New drug", "Current lifestyle intervention"),
          colour = c("skyblue", "deepskyblue4"),
          fontface = "bold",
          hjust = 0)
        
    })   
    
    
    output$F4hcc <- renderPlot({
        
        F4_hcc_plot <-
          F4morbid %>%
          filter(
            Cause == "HCC",
            Age == input$age,
            Risk == input$mortality,
            LiverRx == input$treatmentLiver)
      
        ggplot(F4_hcc_plot) +
            geom_smooth(
                aes(x = time, y = estRx),
                colour = "skyblue", lwd = 1) +
            geom_smooth(
                aes(x = time, y = est),
                colour = "deepskyblue4", lwd = 1) +
            xlim(c(0, 10)) +
            ylim(c(0, 20)) +
            labs(x = "Years", 
                 y = "Hepatocellular carcinoma (%)")
        
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)

