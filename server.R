library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rpart)
library(rpart.plot)
library(BCRA)
library(flexdashboard)

char_race <- c("Asian","Non-Asian")

deter_race <- function(x) {
    return(char_race[as.numeric(x)])
}

breastAll <- read.csv("BreastCancer.csv")

breast <- breastAll[,-1]

library(caret)
set.seed(1992)

intrain<-createDataPartition(y=breast$Class,p=0.7,list=FALSE)

training   <- breast[ intrain , ]
validation <- breast[-intrain , ]


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

zipdata <- allzips[sample.int(nrow(allzips), 500),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$zip_code),]

function(input, output, session) {
    
    
        
        output$distPlot <- renderPlot({
            
            
            dtree <- rpart(Class ~ ., data=breast,  method="class",
                           control=rpart.control(minsplit = input$Split))
            
            
            rpart.plot(dtree , type=4, extra=1,main="PREDICTION DATA")
            
            
        })
        
        
        output$Prediction <- renderText({
            dtree <- rpart(Class ~ ., data=breast,  method="class",
                           control=rpart.control(minsplit = input$Split))
            validation <- data.frame(Clump=input$Clump,UniCell_Size=input$UniCell_Size,
                                     Uni_CellShape=input$Uni_CellShape,
                                     MargAdh=input$MargAdh,SEpith=input$SEpith,
                                     BareN=input$BareN,BChromatin=input$BChromatin,
                                     NoemN=input$NoemN,Mitoses=input$Mitoses)
            dtree.pred <- predict(dtree, newdata=validation, type="class")
            
            paste("Prediction:" , dtree.pred)
        })
        

    
    
    
    
    
    
    ###=================risk
    
    pred_five <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                     T1=as.numeric(input$age),
                                                                     T2=as.numeric(input$age)+5,
                                                                     N_Biop=as.integer(input$biopsies),
                                                                     HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                        as.integer(input$biopsies) == 99,
                                                                                    99,as.integer(input$hyperplasia)),
                                                                     AgeMen=as.numeric(input$menstruation),
                                                                     Age1st=as.numeric(input$first_birth),
                                                                     N_Rels=as.numeric(input$relatives),
                                                                     Race =as.integer(input$race))),1))
    })
    
    pred_avg <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                    T1=as.numeric(input$age),
                                                                    T2=as.numeric(input$age)+5,
                                                                    N_Biop=as.integer(input$biopsies),
                                                                    HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                       as.integer(input$biopsies) == 99,
                                                                                   99,as.integer(input$hyperplasia)),
                                                                    AgeMen=as.numeric(input$menstruation),
                                                                    Age1st=as.numeric(input$first_birth),
                                                                    N_Rels=as.numeric(input$relatives),
                                                                    Race =as.integer(input$race))),1))
    })
    
    pred_lifetime <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                         T1=as.numeric(input$age),
                                                                         T2=90,
                                                                         N_Biop=as.integer(input$biopsies),
                                                                         HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                            as.integer(input$biopsies) == 99,
                                                                                        99,as.integer(input$hyperplasia)),
                                                                         AgeMen=as.numeric(input$menstruation),
                                                                         Age1st=as.numeric(input$first_birth),
                                                                         N_Rels=as.numeric(input$relatives),
                                                                         Race =as.integer(input$race))),1))
    })
    
    pred_avg_life <- reactive({as.numeric(round(absolute.risk(data.frame(ID=1,
                                                                         T1=as.numeric(input$age),
                                                                         T2=90,
                                                                         N_Biop=as.integer(input$biopsies),
                                                                         HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                                                            as.integer(input$biopsies) == 99,
                                                                                        99,as.integer(input$hyperplasia)),
                                                                         AgeMen=as.numeric(input$menstruation),
                                                                         Age1st=as.numeric(input$first_birth),
                                                                         N_Rels=as.numeric(input$relatives),
                                                                         Race =as.integer(input$race))),1))
    })
    
    output$lifetime_pred <- eventReactive(input$do,{
        round(absolute.risk(data.frame(ID=1,
                                       T1=as.numeric(input$age),
                                       T2=90,
                                       N_Biop=as.integer(input$biopsies),
                                       HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                          as.integer(input$biopsies) == 99,
                                                      99,as.integer(input$hyperplasia)),
                                       AgeMen=as.numeric(input$menstruation),
                                       Age1st=as.numeric(input$first_birth),
                                       N_Rels=as.numeric(input$relatives),
                                       Race =as.integer(input$race))),1)
    })
    output$`5_year_pred` <- eventReactive(input$do,{
        round(absolute.risk(data.frame(ID=1,
                                       T1=as.numeric(input$age),
                                       T2=as.numeric(input$age)+5,
                                       N_Biop=as.integer(input$biopsies),
                                       HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                          as.integer(input$biopsies) == 99,
                                                      99,as.integer(input$hyperplasia)),
                                       AgeMen=as.numeric(input$menstruation),
                                       Age1st=as.numeric(input$first_birth),
                                       N_Rels=as.numeric(input$relatives),
                                       Race =as.integer(input$race))),1)
    })
    output$avg_five <- eventReactive(input$do,{
        round(absolute.risk(data.frame(ID=1,
                                       T1=as.numeric(input$age),
                                       T2=as.numeric(input$age)+5,
                                       N_Biop=as.integer(input$biopsies),
                                       HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                          as.integer(input$biopsies) == 99,
                                                      99,as.integer(input$hyperplasia)),
                                       AgeMen=as.numeric(input$menstruation),
                                       Age1st=as.numeric(input$first_birth),
                                       N_Rels=as.numeric(input$relatives),
                                       Race =as.integer(input$race)), iloop = 2),1)
    })
    output$avg_life <- eventReactive(input$do,{
        round(absolute.risk(data.frame(ID=1,
                                       T1=as.numeric(input$age),
                                       T2=90,
                                       N_Biop=as.integer(input$biopsies),
                                       HypPlas=ifelse(as.integer(input$biopsies) == 0 |
                                                          as.integer(input$biopsies) == 99,
                                                      99,as.integer(input$hyperplasia)),
                                       AgeMen=as.numeric(input$menstruation),
                                       Age1st=as.numeric(input$first_birth),
                                       N_Rels=as.numeric(input$relatives),
                                       Race =as.integer(input$race)),iloop = 2),1)
    })
    
    
    observeEvent( eventExpr = input$do , handlerExpr = {
        output$plt1 <- flexdashboard::renderGauge({
            gauge(pred_five(),
                  min = 0,
                  max = 17/10,
                  symbol = '%',
                  label = paste("5-Year Risk"),
                  gaugeSectors(success = c(0,14/10),
                               warning = c(1.4, 1.69),
                               danger = c(17/10,100),
                               colors = c("success","warning","danger")
                  ))
        })
    })
    
    text_race <- reactive({deter_race(input$race)
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$five_yr_text <- renderText({ 
            paste0("Based on the information provided, the woman's estimated risk for developing invasive
                   breast cancer over the next 5 years is ",pred_five(),"% compared to a risk of ",
                   pred_avg(), "% for an average ", input$age, " year old ", text_race()," female from the general
                    India population. This calculation also means that the woman's risk of NOT
                   getting breast cancer over the next 5 years is ", 100-pred_five(), "%.")
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$lifetime_text <- renderText({ 
            paste0("Based on the information provided, the woman's estimated risk for developing invasive
                   breast cancer over her lifetime (to age 90) is ",pred_lifetime() ,"% compared to a risk of ",
                   pred_avg_life(), "% for an average ", input$age, " year old ", text_race()," female from the general
                   India population. This calculation also means that the woman's risk of NOT
                   getting breast cancer over her lifetime is ", 100-pred_lifetime(), "%.")
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$five_yr_title <- renderText({ 
            "5-Year Risk"
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$lifetime_title <- renderText({ 
            "Lifetime Risk"
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$advice_title <- renderText({ 
            "Advice"
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$advice_text1 <- renderText({ 
            "Patients who have an increased risk of developing breast cancer,
                defined as calculated 5 year risk >1.7% and are at least 35 years
                old, are candidates for chemoprevention (such as tamoxifen or raloxifene)."
            
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$advice_text2 <- renderText({ 
            "Patients with elevated breast cancer risk (>1.7%) should be referred to a
                breast surgeon to discuss possible risk reduction interventions."
            
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$short_five <- renderText({ 
            paste0("<font size=\"4\" color=\"#ff8c00\"><b>", ">>>  ","</b></font>", "<font size=\"4\"><b>Your 5-Year Risk: ", pred_five(), "%</b></font>")
        })
    })
    
    observeEvent(eventExpr = input$do, handlerExpr = { 
        output$short_life <- renderText({ 
            paste0("<font size=\"4\" color=\"#ff8c00\"><b>", ">>>  ","</b></font>", "<font size=\"4\"><b>Your Lifetime Risk: ", pred_lifetime(), "%</b></font>")
        })
    })
    
    output$image <- renderImage({
        list(src = "www/question-mark-icon.png",
             contentType = 'image/png',
             width = 25,
             height = 25,
             style = "border-radius: 50%;cursor:hand;cursor:pointer")
    }, deleteFile = FALSE)
    
    observeEvent(input$image_click, {
        showModal(modalDialog(
            title = "Help with the Questionaire",
            HTML("<span style=color:#ff8c00;>Question 1:</span> Current age?<br>
                     Explanation: The risk of developing breast cancer increases with age.<br><br>
                     <span style=color:#ff8c00;>Question 2:</span> Age of first menstruation?<br>
                     Explanation: Women who start menstruating at a very young age have a slight 
                     increase in breast cancer risk that may be linked to their longer lifetime 
                     exposure to estrogen. <br><br>
                     <span style=color:#ff8c00;>Question 3:</span> Age at first birth?<br>
                     Explanation: Risk depends on many factors, including age at first live birth 
                     and family history of breast cancer. The relationship of these two factors 
                     helps determine risk. <br><br>
                     <span style=color:#ff8c00;>Question 4:</span> Number of 1st degree relatives that have had breast cancer? <br>
                     Explanation: Having one or more first-degree relatives (mother, sisters, 
                     daughters) who have had breast cancer increases a woman's chances of 
                     developing this disease.<br><br>
                     <span style=color:#ff8c00;>Question 5:</span> Number of breast biopsies?<br>
                     <span style=color:#ff8c00;>Question 5.1:</span> Did the biopsy display hyperplasia?<br>
                     Explanation: Women who have had breast biopsies have an increased risk of 
                     breast cancer, especially if their biopsy specimens showed atypical 
                     hyperplasia. Women who have a history of breast biopsies are at increased 
                     risk because of whatever breast changes prompted the biopsies. Breast 
                     biopsies themselves do not cause cancer. <br><br>
                     <span style=color:#ff8c00;>Question 6:</span> Race/Ethnicity?<br>
                     Explanation: The original Breast Cancer Risk Assessment was based on 
                     data from white women. But race/ethnicity can influence the calculation 
                     of breast cancer risk. Over the years, as additional data became available, 
                     researchers have updated the model to more accurately estimate risk."),
            size = "l",
            easyClose = TRUE,
            footer = modalButton("Dismiss")
        ))
    })
    
    
    
    
    ## Data Explorer ###########################################
    
    
    # 
    # observe({
    #     cities <- if (is.null(input$states)) character(0) else {
    #         filter(cleantable, State %in% input$states) %>%
    #             `$`('City') %>%
    #             unique() %>%
    #             sort()
    #     }
    
    
    
   
}