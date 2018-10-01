# server.R

library(shiny)
library(highcharter)
library(tidyverse)

shinyServer(function(input, output) {
  
  ## Start by creating a reactive version of the dataset listing. This
  ##   will then let us access the data for use in dynamically creating
  ##   the listing of the available datasets. We only need the "id"
  ##   and "title" datasets.
  
  my_data <- reactive({
    input$listDatasets
    isolate({
      user_name <- if (input$username == "NULL") NULL else input$username
      password <- if (input$password == "NULL") NULL else input$password
    })
    isolate({
      out <- kobo_datasets(
        user = c(user_name, password), 
        api = input$api)[, c("id", "title"), with = FALSE]
    })
  })
  
  ## This is for the datatable output
  # 
  # output$datasetsAvailable <- renderDataTable({
  #   datatable(my_data())
  # })
  # 
  ## This creates the dropdown UI for the sidebar. The values are 
  ##   automatically populated with the "id" and "title" values from
  ##   the my_data dataset, which must be accessed using my_data()
  
  # output$select_dataset <- renderUI({
  #   dat <- my_data()
  #   selectInput("select", label = "Select Dataset", 
  #               choices = setNames(c("269825","269824"), c("Questionnaire_HH_R2P_10092018","Questionnaire_KII_R2P_10092018")), 
  #               selected = 1, selectize = TRUE)
  # })
  
  ## This downloads the requested dataset to your global environment,
  ##   and displays it in the "Requested Dataset" tab in the UI.
  
  output$datasetRequestedHH <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<-R2P_HH[,c("date_assessment" ,"begin_group_R9re9AMAa/Questionnaire/respondent_name/First_name", "enum_slov_id","enum_siev_id" ,"enum_mari_id" ,"base_eecp","begin_group_R9re9AMAa/Questionnaire/phone","_submission_time" )]
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      
      R2P_HH_new<-R2P_HH_new[,c("date_assessment" ,"begin_group_R9re9AMAa/Questionnaire/respondent_name/First_name", "enum_slov_id","enum_siev_id" ,"enum_mari_id" ,"base_eecp","begin_group_R9re9AMAa/Questionnaire/phone","_submission_time" )]
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      names(R2P_HH) <- c("AssessmentDate", "name","enum slov ID", "enum siev ID", "enum mar ID","eecp","phone","sub_time")
      R2P_HH <- separate(R2P_HH,8,into=c("submission_date", "submission_time"), sep=" ")
      R2P_HH$ID <- 1:nrow(R2P_HH)
      R2P_HH$phone <- suppressWarnings(sapply(R2P_HH$phone, as.character))
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH$submission_date<-as.Date(R2P_HH$submission_date)
      
    })
    datatable(R2P_HH, filter = "top", extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$hcontainerHHa <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      gates<- as.character(checkpt.Count$CP_names)
      
      highchart() %>%
        hc_chart(type = "bar", borderColor = '#848D95', borderRadius = 10,
                 borderWidth = 2) %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = checkpt.Count[1,1], style=list( fontSize= "1em",fontWeight= "bold", fontFamily = 'Arial')) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          title= list(text= paste0(checkpt.Count[1,2], " /", "195"),  style=list( fontSize = "2em", fontFamily = 'Arial')),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 200,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(data = checkpt.Count[1,2],color= "#DDE387",dataLabels=list(style = list(fontSize = "40px", fontFamily = 'Arial') ) )
    })
  })
  output$hcontainerHHb <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      gates<- as.character(checkpt.Count$CP_names)
      
      highchart() %>%
        hc_chart(type = "bar", borderColor = '#848D95', borderRadius = 10,
                 borderWidth = 2) %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = checkpt.Count[2,1], style=list( fontSize= "1em",fontWeight= "bold", fontFamily = 'Arial')) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          title= list(text= paste0(checkpt.Count[2,2], " /", "195"),  style=list( fontSize = "2em", fontFamily = 'Arial')),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 200,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(data = checkpt.Count[2,2],color= "#DDE387",dataLabels=list(style = list(fontSize = "40px", fontFamily = 'Arial') ) )
    })
  })
  output$hcontainerHHc <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      gates<- as.character(checkpt.Count$CP_names)
      
      highchart() %>%
        hc_chart(type = "bar", borderColor = '#848D95', borderRadius = 10,
                 borderWidth = 2) %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = checkpt.Count[3,1], style=list( fontSize= "1em",fontWeight= "bold", fontFamily = 'Arial')) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          title= list(text= paste0(checkpt.Count[3,2], " /", "195"),  style=list( fontSize = "2em", fontFamily = 'Arial')),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 200,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(data = checkpt.Count[3,2],color= "#DDE387",dataLabels=list(style = list(fontSize = "40px", fontFamily = 'Arial') ) )
    })
  })
  output$hcontainerHHd <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      gates<- as.character(checkpt.Count$CP_names)
      
      highchart() %>%
        hc_chart(type = "bar", borderColor = '#848D95', borderRadius = 10,
                 borderWidth = 2) %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = checkpt.Count[4,1], style=list( fontSize= "1em",fontWeight= "bold", fontFamily = 'Arial')) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          title= list(text= paste0(checkpt.Count[4,2], " /", "195"),  style=list( fontSize = "2em", fontFamily = 'Arial')),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 200,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(data = checkpt.Count[4,2],color= "#DDE387",dataLabels=list(style = list(fontSize = "40px", fontFamily = 'Arial') ) )
    })
  })
  output$hcontainerHHe <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      gates<- as.character(checkpt.Count$CP_names)
      
      highchart() %>%
        hc_chart(type = "bar", borderColor = '#848D95', borderRadius = 10,
                 borderWidth = 2) %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = checkpt.Count[5,1], style=list( fontSize= "1em",fontWeight= "bold", fontFamily = 'Arial')) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          title= list(text= paste0(checkpt.Count[5,2], " /", "195"),  style=list( fontSize = "2em", fontFamily = 'Arial')),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 200,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(data = checkpt.Count[5,2],color= "#DDE387",dataLabels=list(style = list(fontSize = "40px",  fontFamily = 'Arial') ) )
    })
  })
      
  output$hcontainerHH <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      total.C<-as.data.frame(count(R2P_HH))
      
      highchart() %>%
        hc_chart(type = "bar", borderColor = '#848D95', borderRadius = 10,
                 borderWidth = 2) %>%
        hc_title(text = "Total", style=list( fontSize= "2em",fontWeight= "bold", fontFamily = 'Arial')) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_size(height=200, width=100) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          # stops=col.stops,
          title= list(text= paste0(total.C[1,1], " /", "975"),  style=list( fontSize = "3em", fontFamily = 'Arial')),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 995,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_theme(hc_theme_google())%>%
        hc_add_series(data = total.C[1,1],color= "#21908C",dataLabels=list(style = list(fontSize = "40px", fontFamily = 'Arial') ) ) 
      
    })})
  output$hcontainerHHs <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_HH <- kobo_data_downloader(
        "269825", c(user_name, password), input$api)
      R2P_HH<- select(R2P_HH,3,5,6,15) #subset columns of interest
      colnames(R2P_HH)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH<-as.data.frame(R2P_HH)
      R2P_HH_new <- kobo_data_downloader(
        "273454", c(user_name, password), input$api)
      R2P_HH_new<- select(R2P_HH_new,3,5,6,15)
      colnames(R2P_HH_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Gender")
      R2P_HH_new<-as.data.frame(R2P_HH_new)
      R2P_HH<-rbind(R2P_HH, R2P_HH_new)
      CP_names<-c("mayorsk_horlivka", "mariinka_oleksandrivka", "novotroitske_olenivka",
                  "hnutove_pyschevyk_oktiabr", "stanitsa_luhanska")
      nV<-c(0,0,0,0,0)
      emptyT<-as.data.frame(cbind(CP_names,nV))
      emptyT$CP_names<-as.character(emptyT$CP_names)
      emptyT$nV<-as.numeric(as.character(emptyT$nV))
      
      checkpt.C<- group_by(R2P_HH,CheckPoint)
      checkpt.C<-as.data.frame(count(checkpt.C))
      
      
      checkpt.Count<-data.frame(CP_names=emptyT$CP_names, n=checkpt.C[match(emptyT$CP_names, checkpt.C$CheckPoint), 2])
      checkpt.Count$n<-replace_na(checkpt.Count$n, 0)
      gender.C<-R2P_HH%>%
        group_by(Gender)%>%
        summarise(n = n())
      gender.C<-as.data.frame(gender.C)
      
      gender.C
      highchart() %>% 
        hc_add_theme(hc_theme_sandsignika()) %>% 
        hc_chart(type = "pie") %>% 
        hc_title(text = "Gender Proportion of Surveys") %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = TRUE, 
                   pointFormat = paste('<b>{point.percentage:.1f}%</b>')) %>%
        hc_xAxis(labels = list(format = "{value}%")) %>%
        hc_add_series_labels_values(labels = gender.C$Gender, values = gender.C$n)
      
    })})
  output$hcontainerKIIa <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[1,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[1,2]+TallyT[1,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[1,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[1,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIb <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[2,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[2,2]+TallyT[2,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[2,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[2,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIc <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[3,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[3,2]+TallyT[3,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[3,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[3,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIId <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[4,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[4,2]+TallyT[4,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[4,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[4,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIe <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[5,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[5,2]+TallyT[5,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[5,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[5,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIf <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[6,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[6,2]+TallyT[6,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[6,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[6,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIg <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[7,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[7,2]+TallyT[7,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[7,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[7,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIh <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=50,text = TallyT[8,1], style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_legend(enabled=TRUE)%>%
        hc_yAxis(
          title= list(text= paste0((TallyT[8,2]+TallyT[8,3]), " /", "30"),  style=list( fontSize = "2em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 15,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(name="Adult", data = TallyT[8,2],color= "#36D1B4",dataLabels=list(style = list(enabled=TRUE) ) ) %>%
        hc_add_series(name="Elderly",data = TallyT[8,3],color= "#8E42E3",dataLabels=list(style = list(enabled=TRUE) ) )
      
    })})
  output$hcontainerKIIt <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      total.C1<-as.data.frame(count(R2P_KII))
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(y=50,text = "Total", style=list( fontSize= "1em",fontWeight= "bold")) %>%
        hc_xAxis(labels=list(y=26,style = list(fontSize = "0px"))) %>%
        hc_size(height=350) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          # stops=col.stops,
          title= list(text= paste0(total.C1[1,1], " /", "240"),  style=list( fontSize = "3em")),
          lineWidth=0,
          minorTickWidth=0,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 995,
          labels=list(y=26,style = list(fontSize = "0px") )
        ) %>%
        hc_add_series(data = total.C1[1,1],color= "#21908C",dataLabels=list(style = list(fontSize = "40px") ) )
      
    })})
  output$hcontainerKIIs <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      R2P_KII <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      R2P_KII<- select(R2P_KII,3,5,6,11,15,16) #subset columns of interest
      colnames(R2P_KII)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII_new <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      R2P_KII_new<- select(R2P_KII_new,3,5,6,11,15,16)
      colnames(R2P_KII_new)<- c("Date", "CheckPoint", "EnumeratorOfficeCity", "Settlement", "Gender", "Age")
      R2P_KII<-rbind(R2P_KII, R2P_KII_new)
      
      
      R2P_KII$Settlement[R2P_KII$Settlement == "Donetsk City"] <- "Donetsk_City"
      
      Da<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age<60)))
      Ha<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age<60)))
      Pa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age<60)))
      La<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age<60)))
      Ka<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age<60)))
      Sa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age<60)))
      Na<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age<60)))
      Soa<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age<60)))
      Adult<-c(Da, Ha, Pa, La, Ka, Sa, Na, Soa)
      
      De<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Donetsk_City" & R2P_KII$Age>60)))
      He<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Horlivka" & R2P_KII$Age>60)))
      Pe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Pervomaisk" & R2P_KII$Age>60)))
      Le<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Luhansk" & R2P_KII$Age>60)))
      Ke<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Krasni_Luch" & R2P_KII$Age>60)))
      Se<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Shaktarsk" & R2P_KII$Age>60)))
      Ne<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Novoazovskii_Raion" & R2P_KII$Age>60)))
      Soe<-as.numeric(count(filter(R2P_KII,R2P_KII$Settlement=="Sorokynskyi_Raion" & R2P_KII$Age>60)))
      Elder<-c(De, He, Pe, Le, Ke, Se, Ne, Soe)
      
      S_names<-c("Donetsk_City", "Horlivka", "Pervomaisk", "Luhansk",
                 "Krasni_Luch", "Shaktarsk", "Novoazovskii_Raion", "Sorokynskyi_Raion")
      TallyT<-as.data.frame(cbind(S_names,Adult, Elder))
      TallyT$S_names<-as.character(TallyT$S_names)
      TallyT$Adult<-as.numeric(as.character(TallyT$Adult))
      TallyT$Elder<-as.numeric(as.character(TallyT$Elder))
      total.C1<-as.data.frame(count(R2P_KII))
      
      gender.C<-R2P_KII%>%
        group_by(Gender)%>%
        summarise(n = n())
      gender.C<-as.data.frame(gender.C)
      
      
      highchart() %>% 
        hc_chart(type = "pie") %>% 
        hc_add_theme(hc_theme_sandsignika()) %>% 
        hc_title(text = "Gender Proportion of Surveys") %>%
        hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = TRUE, 
                   pointFormat = paste('<b>{point.percentage:.1f}%</b>')) %>%
        hc_xAxis(labels = list(format = "{value}%")) %>%
        hc_add_series_labels_values(labels = gender.C$Gender, values = gender.C$n)
      
    })})
  output$datasetRequestedKII <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      KIIo <- kobo_data_downloader(
        "269824", c(user_name, password), input$api)
      KIIo<- KIIo[,c("today"  ,"Questionnaire/Questionnaire_001/Name/First_name"  , "enum_slov_id","enum_siev_id" ,"enum_mari_id" ,"eecp","Questionnaire/settl_group" ,"Questionnaire/Questionnaire_001/phone","_submission_time" )]
      names(KIIo) <- c("today", "name","enum slov ID", "enum siev ID", "enum mar ID","eecp","settlement","phone","sub_time")
      KIIo <- separate(KIIo,9,into=c("submission_date", "submission_time"), sep=" ")
      KIIo$ID <- 1:nrow(KIIo)
      KIIo$phone <- suppressWarnings(sapply(KIIo$phone, as.character))
      KII <- kobo_data_downloader(
        "273453", c(user_name, password), input$api)
      KII<- KII[,c("today"  ,"Questionnaire/Questionnaire_001/Name/First_name"  , "enum_slov_id","enum_siev_id" ,"enum_mari_id" ,"eecp","Questionnaire/settl_group" ,"Questionnaire/Questionnaire_001/phone","_submission_time" )]
      names(KII) <- c("today", "name","enum slov ID", "enum siev ID", "enum mar ID","eecp","settlement","phone","sub_time")
      KII <- separate(KII,9,into=c("submission_date", "submission_time"), sep=" ")
      KII$ID <- 1:nrow(KII)
      KII$phone <- suppressWarnings(sapply(KII$phone, as.character))
      KII_all<-rbind(KIIo, KII)
      KII_all$submission_date<-as.Date(KII_all$submission_date)
      
      
      #ADD DATA WRANGLiNG ETC HERERERERERERE
    })
    datatable(KII_all, filter = "top", extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$hcontainerSOS <- renderHighchart({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      DNS_raw <- kobo_data_downloader(
        "270394", c(user_name, password), input$api)
      DNS <- DNS_raw[,c("today","Questionaire/respondent_name/First_name", "enum_kiev_id","Questionaire/area_group","Questionaire/phone","_submission_time")]
      names(DNS) <- c("today", "name","enum","area","phone","sub_time")
      DNS <- separate(DNS,6,into=c("submission_date", "submission_time"), sep=" ")
      DNS$ID <- 1:nrow(DNS)
      DNS$phone <- suppressWarnings(sapply(DNS$phone, as.character))
      DNS_g<- DNS  %>%
        group_by(area)
      DNS_g<-as.data.frame(DNS_g %>%
                             summarise(n=n()))
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_size(height=300) %>%
        hc_title(y=10,text = "Completed Surveys by Settlement Area", style=list( fontSize= "2em",fontWeight= "bold")) %>%
        hc_xAxis(categories=DNS_g$area) %>%
        hc_legend(enabled=FALSE)%>%
        hc_yAxis(
          lineWidth=4,
          minorTickWidth=4,
          tickAmount=2,
          endOnTick = TRUE,
          min = 0,
          max = 200,
          labels=list(y=26,style = list(fontSize = "21px") )
        ) %>%
        hc_add_series(name= "Surveys Completed", data = DNS_g$n, dataLabels=list(enabled=TRUE,style = list(fontSize = "12px") ) )
      
    })

  })
  output$datasetRequestedSOS <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      DNS_raw <- kobo_data_downloader(
        "270394", c(user_name, password), input$api)
      DNS <- DNS_raw[,c("today","Questionaire/respondent_name/First_name", "enum_kiev_id","Questionaire/area_group","Questionaire/phone","_submission_time")]
      names(DNS) <- c("today", "name","enum","area","phone","sub_time")
      DNS <- separate(DNS,6,into=c("submission_date", "submission_time"), sep=" ")
      DNS$ID <- 1:nrow(DNS)
      DNS$phone <- suppressWarnings(sapply(DNS$phone, as.character))
      
    })
    datatable(DNS, filter = "top", extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  
})