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
library(plotly)
library(shiny)
library(HDInterval)
library(shinyBS)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

uplift <- function(x,y){
  b_rate = logit2prob(x+y)
  dif = b_rate - logit2prob(x)
  uplift = dif/logit2prob(x)
  return(uplift)
}

ui <- fluidPage(
  titlePanel("A random bayesian analysis tool"),
  sidebarPanel(
  numericInput(
    inputId="Avisitor",
    label= "Variation A New visitors", value = NULL, width = "50%"),
  numericInput(
    inputId="Bvisitor",
    label= "Variation B New visitors", value = NULL, width = "50%"),
  numericInput(
    inputId="Aconversion",
    label= "Variation A Conversions",  value = NULL, width = "50%"),
  numericInput(
    inputId="Bconversion",
    label= "Variation B conversions",  value = NULL, width = "50%"),
  actionButton(inputId="go", label= "Analyze Conversion!"),
  hr(),
  fileInput("mrr_file", "Choose a .csv file", accept = ".csv"),
  uiOutput('ui_varA'),
  uiOutput('ui_varB'),
  checkboxInput("log", "Log Transform", TRUE),
  #checkboxInput("diag", "Check performance", FALSE),
  actionButton("assign_vars", label = "Raw Summary!"),
  actionButton("model_mrr", label = "Model avg customer mrr!"),
  hr(),
  actionButton("model_mrr_and_conv", label = "Model avg visitor mrr!")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Conversion model",
        p(""),
        tableOutput("convtab"),
        plotlyOutput("convrdist"),
        plotlyOutput("convdifdist"),
        uiOutput("conv_creds")
      ),
      tabPanel(
      "Raw data",
      p(""),
      tableOutput("raw_tabl"),
      #fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("raw_plt"), plotOutput("compare")))
      plotOutput("raw_plt",
                 height = "800px")
    ),
      tabPanel(
      "Customer mean mrr model",
      p(""),
      tableOutput("model2_tabl"),
      plotlyOutput("model2_plt_both"),
      plotlyOutput("model2_plt_dif"),
      uiOutput("mrr_creds")
    ),
      tabPanel(
      "Visitor mean mrr model",
      p(""),
      tableOutput("mrr_per_visitor_tab"),
      plotlyOutput("overall_dist"),
      uiOutput("mean_btn"),
      plotlyOutput("overall_dist_dif"),
      uiOutput("mrr_v_creds")
    )
    )
  )
  )

server <- function(input, output) {
  conv_model <- eventReactive(input$go, {
    
    output$conv_creds <- renderUI({
      sliderInput("creds1",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.01, max = 0.999)})
    
    conv_dat <- tibble(variation = c("A", "B"),
           n = c(input$Avisitor, input$Bvisitor),
           conversions =c(input$Aconversion, input$Bconversion))
    withProgress(message = "Modeling in progress. Please wait Mr. Strang ...",{
    fit <- brm(family = binomial,
                        conversions | trials(n) ~ variation,
                        data = conv_dat,
                        iter = 2000,
                        warmup = 500,
                        refresh = 0)})
    
    outperforming <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]) , VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`) 
    
    conv_tab <- conv_dat %>% 
      cbind(., outperforming) %>%
      mutate(CR = paste0(round((conversions/n)*100,2),"%")) %>%
      mutate(uplift = c("", paste0(round(uplift(posterior_summary(fit)[1,1],posterior_summary(fit)[2,1])*100,2
      ),"%"))) 
    
    conv_dist_tab <- posterior_samples(fit) %>% 
      select(1:2) %>%
      mutate(`Variation A` = logit2prob(.[,1]), `Variation B` = logit2prob(.[,1]+.[,2])) %>%
      select(`Variation A`, `Variation B`) %>% 
      gather(key = "variation", value = "CR", `Variation A`, `Variation B`) 
    
    conv_dif_tab <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) %>% 
      transmute(dif = (VarB_per-varA_per)/mean(varA_per)) 
    
    conv_dif_dist <- posterior_samples(fit) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      mutate(varA_per = 100*varA_per,
             VarB_per = 100*VarB_per) %>% 
      select(varA_per, VarB_per) %>% 
      transmute(dif = (VarB_per-varA_per))
    
    list(conv_dat, fit, outperforming, conv_tab, conv_dist_tab, conv_dif_tab, conv_dif_dist)
        })
  
    output$convtab <- function(){conv_model()[[4]] %>% 
        kbl(caption = "Conversion rate statistics") %>% 
        kable_styling()
    }
    
    
      
    output$convrdist <- renderPlotly({
      g_conv_both <- conv_model()[[5]] %>% 
        ggplot(aes(x = CR, fill = variation, color = variation)) +
        ggtitle("Conversion (visitor to npc or pc) rates for the variations")+
        geom_density(alpha = 0.7)+
        scale_fill_brewer(palette="Set1")+
        scale_x_continuous(name  = "Conversion Rate", labels = percent)+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw()
      
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      p_conv_both <- ggplotly(g_conv_both, tooltip= c("x", "variation")) %>% 
        layout(yaxis = ax2)
      p_conv_both
      
      })
    
    output$convdifdist <- renderPlotly({
      
      pre_g_conv_dif <- conv_model()[[7]] %>% 
        ggplot(aes(x = dif)) +
        geom_density()
      
      dens1 <- ggplot_build(pre_g_conv_dif)$data[[1]]
      
      g_conv_dif <- ggplot(data = conv_model()[[7]], aes(x=dif))+
        ggtitle("Distribution of difference in conversion rate (visitor to npc or pc)")+
        geom_density()+
        geom_area(data = dens1 %>% filter(x > 0),
                  aes(x=x,y=y),
                  fill = "#377EB8",
                  color = "#377EB8",
                  alpha = 0.8)+
        geom_area(data = dens1 %>% filter(x < 0),
                  aes(x=x,y=y),
                  fill = "#E41A1C",
                  color = "#E41A1C",
                  alpha = 0.8)+
        geom_area(data = dens1 %>% filter(x < quantile(conv_model()[[7]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)),
          aes(x=x,y=y),
          fill = "black",
          color = "black",
          alpha = 0.5)+
        geom_area(data = dens1 %>% filter(x > quantile(conv_model()[[7]]$dif, probs = 1-((1-input$creds1)*0.5), names=FALSE)),
          aes(x=x,y=y),
          fill = "black",
          color = "black",
          alpha = 0.5)+
        scale_x_continuous(name  = "Difference in conversion Rate", labels = percent)+
        scale_y_continuous(NULL, breaks = NULL) +
        guides(color = FALSE, fill = guide_legend(title=NULL))+
        theme_bw()
      
      ax2 <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showgrid = FALSE)
      
      p_mod2_dif <- ggplotly(g_conv_dif, tooltip= c("x")) %>% 
        layout(yaxis = ax2) #%>% 
        #add_segments(x=c(quantile(conv_model()[[7]]$dif, probs = 1-((1-input$creds1)*0.5), names=FALSE),
                         #quantile(conv_model()[[7]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)), 
                     #xend = c(quantile(conv_model()[[7]]$dif, probs = 1-((1-input$creds1)*0.5), names = FALSE),
                              #quantile(conv_model()[[7]]$dif, probs = (1-input$creds1)*0.5, names = FALSE)),
                     #y=c(0,0), yend= c(12,12), line=list(color=c("darkgreen", "darkgreen"), width = c(4,4))) 
      p_mod2_dif
      }) 
    
  ###Model 2
  
  variations <- eventReactive(input$mrr_file, {
    req(input$mrr_file)
    var_names <- read.csv(input$mrr_file$datapath, header = T) %>% 
      distinct(variation)
    var_names <- c(var_names)
    var_names
  })
  output$ui_varA <- renderUI({
    selectInput('pick_varA',
                label ='Pick variation A',
                choices=variations(),
                selected = NULL, multiple = FALSE,width="450px", selectize = TRUE)
  })
  output$ui_varB <- renderUI({
    selectInput('pick_varB',
                label ='Pick variation B',
                choices=variations(),
                selected = NULL, multiple = FALSE,width="450px", selectize = TRUE)
  })
  npc_dat <- eventReactive(input$assign_vars, {
    req(input$mrr_file)
    npc_dat <- read.csv(input$mrr_file$datapath, header = T) %>% 
      select(1,2) %>% 
      mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!sym(input$pick_varB) := "B")) %>%
      filter(variation %in% c("A", "B")) %>%
      rename(firstmrr = 2)
    npc_dat
  })
  
  model2 <- eventReactive(input$model_mrr,{
    req(input$mrr_file)
    
    output$mrr_creds <- renderUI({
      sliderInput("creds2",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.01, max = 0.999)})
    
    npc_wide <- read.csv(input$mrr_file$datapath, header = T) %>% 
      select(1,2) %>% 
      mutate(variation = recode(variation, !!sym(input$pick_varA) := "A", !!(input$pick_varB) := "B")) %>% 
      filter(variation %in% c("A", "B")) %>% 
      rename(firstmrr = 2) %>% 
      mutate(A = as.numeric(as.character(factor(variation, labels=c(1,0))))) %>% 
      mutate(B = 1-A)
    withProgress(message = "Modeling in progress. Please wait Mr. Strang ...",{
      b4.3 <- brm(data = npc_wide, family = shifted_lognormal(link_sigma = "identity"),
                  bf(firstmrr ~ 0 + A + B, sigma ~ 0 + A + B), #sigma is modeled as also depending on variation
                  #prior = c(prior(student_t(3,0,1), class = b)), #Look into  more informative priors for accuracy managment
                  control = list(adapt_delta = 0.95),
                  iter = 3000, warmup = 600, chains = 4, cores = 4, #Look into this for managing computing time 
                  seed = 4)
      post_pred <- as.data.frame(t(posterior_predict(object = b4.3, 
                                                     newdata = npc_wide[,3:4], nsamples = 1)))
      npc_dat2 <- cbind(npc_wide[,1:2], post_pred)
      
      post_dist <- as.data.frame(t(posterior_epred(b4.3, nsamples = 6000)))
      mean_samp <- cbind(npc_wide$variation, post_dist) %>%
        distinct() %>%
        pivot_longer(-`npc_wide$variation`) %>%    
        pivot_wider(names_from=`npc_wide$variation`, values_from=value) %>% 
        select(A,B) 
      
      #Calculations with model distribution of means
      mrr_mean <- mean_samp %>%
        pivot_longer(cols = c(A, B), values_to = "means", names_to = "Variation")  %>% 
        group_by(Variation) %>% 
        summarise(`Estimate of mean` = round(mean(means),2),
                  `Est. error of mean` = round(sd(means),2),
                  Q2.5 = round(quantile(means, prob = c(.025)),2),
                  Q97.5 = round(quantile(means, prob = c(.975)),2))
      
      #Calculating probabikity of each variation having higher true mean MRR
      mrr_better <- mean_samp %>%  
        summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                  b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
        pivot_longer(cols = c(a_better, b_better), values_to = "Prob. of outperforming (on mean)") %>% 
        select("Prob. of outperforming (on mean)")
      
      model2_tabl <- cbind(mrr_mean, mrr_better)
      
      mean_dif <- mean_samp %>% 
        transmute(dif = B - A)
      
      list(b4.3, npc_dat2, mean_samp, model2_tabl, mean_dif)
    })
  })
  output$raw_tabl = function(){npc_dat() %>%
      #filter(.[[2]] < 300) %>% #Possible to put in filter for removing outliers. Look into making optional? 
      group_by(variation) %>% 
      summarise(mean = mean(firstmrr),
                SD = sd(firstmrr),
                max = max(firstmrr),
                min= min(firstmrr),
                Q25 = quantile(firstmrr, prob = c(.25)),
                median = median(firstmrr),
                Q75 = quantile(firstmrr, prob = c(.75))) %>% 
      kbl(caption="Overview of raw MRR data") %>% 
      kable_styling(
      )}
  
  output$raw_plt = renderPlot({
    raw_plt <- npc_dat() %>% 
      ggplot(aes(x = as.factor(variation), y = firstmrr, fill = as.factor(variation), color = as.factor(variation))) +
      ggtitle("Distribution of log-transformed \n raw MRR data")+
      geom_violin(trim=FALSE, alpha =1, color = FALSE)+ #alpha changed from backend
      geom_jitter(shape=18, position=position_jitter(0.05), size = 5)+ #colours changed from backend
      geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      scale_colour_manual(values = c("#4DAF4A", "orange"), guide = FALSE)+
      scale_fill_brewer(palette="Set1")+
      scale_x_discrete(name = "Variation")+
      scale_y_continuous(name = "MRR", n.breaks = 20)+
      guides(fill=guide_legend(title = "Variations"))#+
      #theme_bw()
    
    if(input$log)
      raw_plt <- npc_dat() %>% 
        ggplot(aes(x = as.factor(variation), y = log(firstmrr), fill = as.factor(variation), color = as.factor(variation))) +
        ggtitle("Distribution of log-transformed \n raw MRR data")+
        geom_violin(trim=FALSE, alpha =1,color=FALSE)+ #alpha changed from backend
        geom_jitter(shape=18, position=position_jitter(0.05), size = 5)+ #colours changed from backend
        geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
        scale_colour_manual(values = c("#4DAF4A", "orange"), guide = FALSE)+
        scale_fill_brewer(palette="Set1")+
        scale_x_discrete(name = "Variation")+
        scale_y_continuous(name = "MRR", n.breaks = 20)+
        guides(fill=guide_legend(title = "Variations"))#+
        #theme_bw()
    return(raw_plt)
  })
  
  #output$compare <- renderPlot({model2()[[2]] %>% 
      #ggplot(aes(x = as.factor(variation), y = log(model2()[[2]][[3]]), fill = as.factor(variation), color = as.factor(variation))) +
      #ggtitle("Diagnostic sample dist. of model predicted data")+
      #geom_violin(trim=FALSE, alpha =1, color=FALSE)+ #alpha changed from backend
      #geom_jitter(shape=10, position=position_jitter(0.05))+ #colours changed from backend
      #geom_boxplot(width=0.15, fill = "white", alpha = 0.5, color = "black" )+
      #scale_colour_manual(values = c("#4DAF4A", "gold"), guide = FALSE)+
      #scale_fill_brewer(palette="Set1")+
      #scale_x_discrete(name = "Variation")+
      #scale_y_continuous(name = "MRR", n.breaks = 20, limits = ggplot_build(raw_dist)$layout$panel_scales_y[[1]]$range$range)+
      #guides(fill=guide_legend(title = "Variations"))+
      #theme_bw()
  #})
  output$model2_tabl <- function(){model2()[[4]] %>% 
      kbl(caption = "Table of model implied estimates of mean MRR per PC per variation") %>% 
      kable_styling()
  }
  output$model2_plt_both <- renderPlotly({ #plotly used so stat_slab, stat_pointinterval not used here
    g_mod2_both <- model2()[[3]] %>% 
      gather(key = "variation", value = "mean_mrr", A, B) %>% 
      ggplot(aes(x = mean_mrr, fill = variation , color = variation))+
      ggtitle("Distributions of model implied mean MRR per PC per variation")+
      geom_density(alpha = 0.7)+ #alpha changed from back end
      scale_fill_brewer(palette="Set1")+ #colours changed from back-end code
      scale_x_continuous(name  = "Mean MRR per PC", labels = dollar_format())+
      scale_y_continuous(NULL, breaks = NULL) +
      guides(color = FALSE, fill = guide_legend(title=NULL))+
      theme_bw() 
    
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      ticks = "",
      showgrid = FALSE)
    
    p_mod2_both <- ggplotly(g_mod2_both, tooltip= c("x", "variation")) %>% 
      layout(yaxis = ax)
    p_mod2_both
    
  })
  output$model2_plt_dif <- renderPlotly({ #plotly used so stat_slab, stat_pointinterval not used here
    pre_g_mod2_dif <- model2()[[5]] %>% 
      ggplot(aes(x = dif)) +
      geom_density()
    
    dens2 <- ggplot_build(pre_g_mod2_dif)$data[[1]]
    
    g_mod2_dif <- ggplot(data = model2()[[5]], aes(x=dif))+
      geom_density()+
      ggtitle("Distribution of model implied difference of mean MRR per NPC between variations")+
      geom_area(data = dens2 %>% filter(x > 0),
                aes(x=x,y=y),
                fill = "#377EB8",
                color = "#377EB8",
                alpha = 0.8)+
      geom_area(data = dens2 %>% filter(x < 0),
                aes(x=x,y=y),
                fill = "#E41A1C",
                color = "#E41A1C",
                alpha = 0.8) +
      #geom_area(data = dens2 %>% filter(x < hdi(model2()[[5]]$dif, credMass = input$creds2)[1]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      #geom_area(data = dens2 %>% filter(x > hdi(model2()[[5]]$dif, credMass = input$creds2)[2]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      geom_area(data = dens2 %>% filter(x < quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      geom_area(data = dens2 %>% filter(x > quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names=FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      scale_y_continuous(NULL, breaks = NULL) +
      scale_x_continuous(name  = "mean MRR per PC of B - mean MRR per PC of A", labels = dollar_format())+
      theme_bw()
    
    ax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      ticks = "",
      showgrid = FALSE)
    
    p_mod2_dif <- ggplotly(g_mod2_dif, tooltip= c("x")) %>% 
      layout(yaxis = ax2) # %>% 
    
    #add_segments(type = "rect", x=c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names=FALSE),
                                    #quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)), 
                #xend = c(quantile(model2()[[5]]$dif, probs = 1-((1-input$creds2)*0.5), names = FALSE),
                          #quantile(model2()[[5]]$dif, probs = (1-input$creds2)*0.5, names = FALSE)),
                 #y=c(0,0), yend= c(12,12), line=list(color=c("darkgreen", "darkgreen"), width = c(4,4))) 
    p_mod2_dif
  })
  
  ##Model 3
  
  conv_mrr_model <- eventReactive(input$model_mrr_and_conv, {
    
    #output$mean_btn <- renderUI({
      #checkboxGroupButtons(
        #inputId = "mean_btn", label = "Expected difference is:",
        #choices = c(paste0("$",round(conv_mrr_model()[[5]],2))),
        #justified = TRUE, status = "primary", size = "lg", width = "20%")
    #})
    
    output$mean_btn <- renderUI({
      bsButton("mean_btn", label = paste("Expected difference is:", "$",round(conv_mrr_model()[[5]],2)),
               type = "toggle", value = TRUE, size = "large", style="info")
    })
    
    
    output$mrr_v_creds <- renderUI({
      sliderInput("creds3",
                  label = "Choose a credible interval",
                  value = 0.68, min = 0.01, max = 0.999)})
    
    post_samp_transformed <- posterior_samples(conv_model()[[2]]) %>%
      select(1:2) %>%
      mutate(varA_per = logit2prob(.[,1]), VarB_per = logit2prob(.[,1]+.[,2])) %>%
      select(varA_per, VarB_per) 
    
    overall_results <- model2()[[3]]%>%
      select(A, B) %>% 
      mutate(A_overall = A*post_samp_transformed$varA_per,
             B_overall = B*post_samp_transformed$VarB_per) %>% 
      select(A_overall, B_overall)
    
    overall_better <- overall_results%>% 
      summarise(a_better = paste0(round((sum(.[,1]>.[,2])/n())*100,2),"%"),
                b_better = paste0(round((sum(.[,2]>.[,1])/n())*100,2),"%")) %>% 
      gather(key = variation, value = `Prob. of outperforming`, a_better, b_better) %>% 
      select(`Prob. of outperforming`)
    
    tab_overall <- overall_results %>%
      rename(A = "A_overall", B = "B_overall") %>% 
      gather(key="Variation", value = "Values", A, B)%>%
      group_by(Variation) %>% 
      summarise(Mean = round(mean(Values),2),
                SD = round(sd(Values),2),
                Q2.5 = round(quantile(Values, prob = c(.025)),2),
                Q97.5 = round(quantile(Values, prob = c(.975)),2)) %>% 
      cbind(overall_better) %>%
      mutate(`MRR per 1000 visitors` = 1000*Mean) 
    
    mean_difference <- overall_results %>% 
      summarise(dif=mean(B_overall-A_overall))
    
    dist_overall <- overall_results %>%
      gather(key = "variation", value = "overall", A_overall, B_overall)
    
    dist_dif_overall <- overall_results %>% 
      transmute(dif = B_overall-A_overall)
    
    list(post_samp_transformed, overall_results, overall_better, tab_overall, mean_difference, dist_overall, dist_dif_overall)
    
  })
  
  
  output$mrr_per_visitor_tab <- function(){ conv_mrr_model()[[4]] %>% 
      kbl(caption="MRR per visitor") %>% 
      kable_styling()}
  
  output$overall_dist <- renderPlotly({
    
    g_overall_dist <- conv_mrr_model()[[6]] %>% 
      ggplot(aes(x = overall, fill = variation , color = variation))+
      ggtitle("Model implied distributions of MRR per visitor")+
      geom_density(alpha = 0.7)+
      scale_fill_brewer(palette="Set1")+
      scale_x_continuous(name  = "MRR per visitor", labels = dollar_format())+
      scale_y_continuous(NULL, breaks = NULL) +
      guides(color = FALSE, fill = guide_legend(title=NULL))+
      theme_bw() 
    
    
    ax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      ticks = "",
      showgrid = FALSE)
    
    p_overall_dist <- ggplotly(g_overall_dist, tooltip= c("x", "variation")) %>% 
      layout(yaxis = ax2)
    p_overall_dist
    
  })
  
  output$overall_dist_dif <- renderPlotly({
    
    pre_g_overall_dist_dif <- conv_mrr_model()[[7]] %>%
      ggplot(aes(x = dif))+
      geom_density()
    
    dens3 <- ggplot_build(pre_g_overall_dist_dif)$data[[1]]
    
    g_overall_dist_dif <- ggplot(data = conv_mrr_model()[[7]], aes(x=dif))+
      geom_density()+
      ggtitle("Model implied distribution of difference of overall MRR per visitor")+
      geom_area(data = dens3 %>% filter(x > 0),
              aes(x=x,y=y),
              fill = "#377EB8",
              color = "#377EB8",
              alpha = 0.8)+
      geom_area(data = dens3 %>% filter(x < 0),
                aes(x=x,y=y),
                fill = "#E41A1C",
                color = "#E41A1C",
                alpha = 0.8) +
      #geom_area(data = dens2 %>% filter(x < hdi(conv_mrr_model()[[7]]$dif, credMass = input$creds3)[1]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      #geom_area(data = dens2 %>% filter(x > hdi(conv_mrr_model()[[7]]$dif, credMass = input$creds3)[2]),
                #aes(x=x,y=y),
                #fill = "black",
                #color = "black",
                #alpha = 0.5)+
      geom_area(data = dens3 %>% filter(x < quantile(conv_mrr_model()[[7]]$dif, probs = (1-input$creds3)*0.5, names = FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      geom_area(data = dens3 %>% filter(x > quantile(conv_mrr_model()[[7]]$dif, probs = 1-((1-input$creds3)*0.5), names=FALSE)),
                aes(x=x,y=y),
                fill = "black",
                color = "black",
                alpha = 0.5)+
      scale_y_continuous(NULL, breaks = NULL) +
      scale_x_continuous(name  = "mean MRR per PC of B - mean MRR per PC of A", labels = dollar_format())+
      theme_bw()
    
    ax2 <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      ticks = "",
      showgrid = FALSE)
    
    p_overall_dist_dif <- ggplotly(g_overall_dist_dif, tooltip= c("x", "variation")) %>% 
      layout(yaxis = ax2) 
    
    if(input$mean_btn == TRUE)
      p_overall_dist_dif <- p_overall_dist_dif %>%  
      add_segments(type="rect", x = conv_mrr_model()[[5]]$dif, xend = conv_mrr_model()[[5]]$dif, 
                   y = 0, yend=12, line=list(color = "deepskyblue", width = 4, dash = "dash"))

      #add_segments(type = "rect", x=c(quantile(conv_mrr_model()[[7]]$dif, probs = 1-((1-input$creds3)*0.5), names=FALSE),
                                     # quantile(conv_mrr_model()[[7]]$dif, probs = (1-input$creds3)*0.5, names = FALSE)), 
                   #xend = c(quantile(conv_mrr_model()[[7]]$dif, probs = 1-((1-input$creds3)*0.5), names = FALSE),
                            #quantile(conv_mrr_model()[[7]]$dif, probs = (1-input$creds3)*0.5, names = FALSE)),
                   #y=c(0,0), yend= c(12,12), line=list(color=c("darkgreen", "darkgreen"), width = c(4,4)))
    
    p_overall_dist_dif
  })
  
}

shinyApp(ui = ui, server = server)