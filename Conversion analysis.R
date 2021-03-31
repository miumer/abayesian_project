#Conversion rate analysis
library(tidyverse)
library(brms)
library(magrittr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(ggrepel)
library(RColorBrewer)
library(bayestestR)
library(scales)
library(kableExtra)
library(patchwork)
library(shiny)

#Function for transforming logits to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


ui <- fluidPage(
  numericInput(
    inputId="Avisitor",
    label= "Variation A New visitors", value=0),
  numericInput(
    inputId="Bvisitor",
    label= "Variation B New visitors", value=0),
  numericInput(
    inputId="Aconversion",
    label= "Variation A Conversions", value=0),
  numericInput(
    inputId="Bconversion",
    label= "Variation B conversions", value=0),
  actionButton(inputId="go", label= "Go"),
  htmlOutput("convtab"),
  plotOutput("convrdist"),
  plotOutput("convdifdist")
)

server <- function(input, output) {observeEvent(input$go,{
  conv_data2 <- reactive({ req(input$Avisitor)
    tibble(variation = c("A", "B"),
           n = c(input$Avisitor, input$Bvisitor),
           conversions =c(input$Aconversion, input$Bconversion)) })
  #Binomial model
  fit <- reactive({brm(family = binomial,
                       conversions | trials(n) ~ variation,
                       data = conv_data2(),
                       iter = 2000,
                       warmup = 500,
                       refresh = 0)})

  #Calculating the probability of out performing for each variation
  outperforming <- reactive({posterior_samples(fit()) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]) , VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`)})
  
  #Calculating uplift uring the back-transformed (with logit2prob) model coefficinets
  uplift <- function(x,y){
    b_rate = logit2prob(x+y)
    dif = b_rate - logit2prob(x)
    uplift = dif/logit2prob(x)
    return(uplift)
  }
  #Creating the table from the numbers
  conv_tab <- reactive({conv_data2 () %>% 
      cbind(., outperforming()) %>%
      mutate(CR = paste0(round((conversions/n)*100,2),"%")) %>%
      mutate(uplift = c("", paste0(round(uplift(posterior_summary(fit())[1,1],posterior_summary(fit())[2,1])*100,2
      ),"%"))) %>% 
      kbl(caption = "Conversion rate statistics") %>% 
      kable_styling()})
  
  output$convtab <- renderText({conv_tab()})
  
  #Conversion rates distribution for both variations side by side
  
  conv_r_dist <- reactive({posterior_samples(fit()) %>% 
      select(1:2) %>%
      mutate(`Variation A` = logit2prob(.[,1]), `Variation B` = logit2prob(.[,1]+.[,2])) %>%
      select(`Variation A`, `Variation B`) %>% 
      gather(key = "variation", value = "CR", `Variation A`, `Variation B`) %>% 
      ggplot(aes(x = CR, fill = variation, color = fct_rev(variation))) +
      ggtitle("Conversion (visitor to npc or pc) rates for the variations")+
      stat_slab(alpha = .5)+
      stat_pointinterval(position = position_dodge(width = .4, preserve = "single"))+
      scale_fill_brewer(palette="Dark2")+
      scale_x_continuous(name  = "Conversion Rate", labels = percent)+
      scale_y_continuous(NULL, breaks = NULL) +
      guides(color = FALSE, fill = guide_legend(title=NULL))+
      theme_bw()})
  
  output$convrdist <- renderPlot({conv_r_dist()})
  
  #Distribution of difference of conversion rates
  conv_dif_dist <- reactive({posterior_samples(fit()) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      transmute(dif = (VarB_per-varA_per)/mean(varA_per)) %>% 
      ggplot(aes(x = dif)) +
      ggtitle("Distribution of difference in conversion rate (visitor to npc or pc)")+
      stat_slab(alpha = .5, fill = "#7570B3")+
      stat_pointinterval(position = position_dodge(width = .4, preserve = "single"))+
      scale_x_continuous(name  = "Difference in conversion Rate", labels = percent)+
      scale_y_continuous(NULL, breaks = NULL) +
      guides(color = FALSE, fill = guide_legend(title=NULL))+
      theme_bw()}) 
  
  output$convdifdist <- renderPlot({conv_dif_dist()}) 
})
}


shinyApp(ui = ui, server = server)