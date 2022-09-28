
#######################################
# Loading Libraries
#######################################
library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(shinyWidgets)
library(dplyr)
library(kableExtra)
library(DT) # this is requried to run "JS" functions
library(colourpicker)


##############################################################################
#Action button style function default 30px and width 100px
##############################################################################
styleButtonBlue<- function(xheight="50px",xwidth="180px",xcolor='#4682B4'){
  paste(
    "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:",xcolor,";",
    "border-color: #ffffff;
                        border-width:2px;
                        height:",xheight,";
                        width:",xwidth,";
                        font-size: 13px;")
}


#######################################
# function for percentage with decimals
#######################################
fnpercent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

#######################################
# function to wrap text anywhere
#######################################
our_strwrap <- function(x) lapply(strwrap(x, width = 12, simplify= FALSE), paste, collapse = "\n")

#######################################
# Dashboard UI
#######################################
ui <- fluidPage(
  # This script is to format all datatables
  tags$head(
    tags$style(HTML("
                      .datatables { overflow-y: auto; font-size:12px;font-weight:bold;font-family: 
                      'Lucida Sans Unicode';text-font:normal; }" )
    )
  ),
  
  tags$head(
    tags$style(HTML(
      "label { font-size:85%; font-family:Times New Roman; margin-bottom: 
    5px; }"
    ))
  ),
  
  # UI starts here
  column(
    width = 12,
    align='left',
    div(style = "margin-top:10px"),
    column(
      width = 4,
      HTML(paste('<h4><b>','Contingency Table Automated','</b><h5>'))
    ),
    column(
      width = 2,
      actionButton(inputId = 'mMakeDtTable',label = 'Create Temp Table!',style = styleButtonBlue(xheight = '35px',xwidth = '150px'))
    ),
    column(
      width = 2,
      actionButton(inputId = 'mUpdateTblTotals',label = 'Refresh Totals!',style = styleButtonBlue(xheight = '35px',xwidth = '150px'))
    ),
    column(
      width = 2,
      colourpicker::colourInput(inputId = "mTableBackColor", 
                                label = NULL, 
                                value = "white",
                                showColour = c("both", "text", "background"),
                                palette = c("square", "limited"),
                                allowedCols = NULL,
                                allowTransparent = FALSE,
                                returnName = FALSE,
                                closeOnClick = FALSE)
    ),
    column(
      width = 2,
      
      actionButton(inputId = 'mSaveKbl2Html',label = 'Export to HTML!',style = styleButtonBlue(xheight = '35px',xwidth = '150px'))
    ),
    
    div(style = "margin-top:-10px"),
    column(style = "border: 2px double red;height: 550px;overflow-y: auto;font-size:15px;font-style:normal;",
           width = 3,
           align = 'center',
           tabsetPanel(
             id='msidetabset',
             tabPanel(title = 'Parameter',
                      div(style = "margin-top:-15px"),
                      column(width = 12,
                             div(style = "margin-top:5px"),
                             splitLayout(cellWidths = c('50%','50%'),
                                         numericInput(inputId = 'mMaxRows',label = 'No.of Rows',value = 3,min = 2,max = 9,step = 1,width = '100px'),
                                         numericInput(inputId = 'mMaxColumn',label = 'No.of Columns',value = 3,min = 2,max = 9,step = 1,width = '100px')
                             ),
                             div(style = "margin-top:5px"),
                             textInput(inputId = 'mgrptitle',label = "Group Header eg 'Gender'",value = 'Gender',width = '100%'),
                             div(style = "margin-top:5px"),
                             HTML(paste('<h6><b>',"You can Alter Column Width here..!",'</b><h5>')),
                             div(style = "margin-top:-10px"),
                             splitLayout(cellWidths = c('25%','25%','25%','25%'),
                                         numericInput(inputId = 'm1stColWidth',label = '1st Col.',value = 8,min = 3,max = 30,step = 1),
                                         numericInput(inputId = 'm2ndColWidth',label = '2nd Col.',value = 10,min = 3,max = 30,step = 1),
                                         numericInput(inputId = 'mOtherColWidth',label = 'Others',value = 6,min = 3,max = 30,step = 1),
                                         numericInput(inputId = 'mTotalColWidth',label = 'Total Col',value = 6,min = 3,max = 30,step = 1)
                                         
                             ),
                             textAreaInput(inputId = 'mContingencyTblTitle',
                                           label = 'Enter Title (add <br> for new line)',
                                           value = 'Main Title <br> Report dated: 1st Oct 2022',
                                           width = '100%',height = '100px'),
                             prettyCheckbox(inputId = 'mAlignCenter',label = "Check to Align Title to Center",value = FALSE,status = 'danger'),
                             HTML(paste('<h6><b>',"You can Alter Font Size here..!",'</b><h5>')),
                             div(style = "margin-top:-10px"),
                             splitLayout(cellWidths = c('25%','25%','25%','25%'),
                                          numericInput(inputId = 'mHeaderColFont',label = 'Header',value = 14,min = 8,max = 30,step = 1),
                                         numericInput(inputId = 'mTableColFont',label = 'Table',value = 13,min = 8,max = 30,step = 1)
                                         
                             )
                      )  
             ), #TabPanel closure
             tabPanel(title = 'Col Rename',
                      div(style = "margin-top:10px"),
                      DT::dataTableOutput("mColRenameTbl",height = '425px'),
                      useShinyjs(),
                      extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('mColRenameTbl_cell_clicked', null); }"),functions = c('foo','bar')),
                      actionButton(inputId = "mColRenameYesBtn",label = "Commit Rename",style = styleButtonBlue(xheight = '35px',xwidth = '150px'))
             ) #TabPanel closure
           ) #Tabset panel closure
    ), #column closure
    column(style = "border: 2px double red;height: 550px;overflow-y: auto;",
           width = 9,
           align = 'center',
           tabsetPanel(
             id='mTblSetPanel',
             tabPanel(
               title = "Data Entry",
               DT::dataTableOutput("mEditableTbl", height = '390px', width = "100%"),
               tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
               tags$style(HTML('table thead th{white-space: wrap;}')),
               div(style = "margin-top:5px"),
               tags$head(
                 tags$style(
                   paste0("#mdatalistString{color:black; font-size:18px; font-style:bold;overflow:auto;text-align: justify;margin: 5px 5px 5px 5px;
                                            width: 800px;height: 45px;max-height: 45px; background: #ffffff;}")
                 )
               ),
               div(style = "margin-top:-10px"),
               textInput(inputId = 'mdatalistString',label = "Import this list, eg United States,10,20,30,40",value = "United States,10,20,30,40",width = '800px' ),
               div(style = "margin-top:-15px"),
               actionButton(inputId = 'mdatalistImportBtn',label = 'Click me..!',style = styleButtonBlue(xheight = '35px',xwidth = '150px'))
             ), #TabPanel closure
             tabPanel(
               title = "Contingency Table",
               htmlOutput('mshowmatrixtbl')
             ), #TabPanel closure
             tabPanel(
               title = "Statistics",
               HTML(paste('<h4><b><u>',"Chi-Square Test",'</u></b>','<br>',
                          '<h6>',"Chi-Square Test for Association/Independence",'<h5>')),
               div(style = "margin-top:-5px"),
               HTML(paste('<h5><b>',"What Is a Chi-Square Test?",'</b><br>' ,
               "The Chi-Square test is a statistical procedure for determining the difference between",
                          "observed and expected data. This test can also be used to determine whether",
                          "it correlates to the categorical variables in our data. It helps to find out", 
                          "whether a difference between two categorical variables is due to chance or a", 
                          "relationship between them")),
               HTML(paste('<h5><b>',"What type of data should a chi-squared test be applied to?",'</b><br>' ,
                          "The Chi-square test analyzes categoricaldata. It means that the data", 
                          "has been counted and divided into categories.", "In otherwords, The Chi-Square", 
                          "Test offers a global test / goodness of fit measure of the general relationship between the two variables.")),
               tags$head(
                 tags$style(
                   paste0("#mChiSqrTest{background: #ffffcd;}")
                 )
               ),
               div(style = "margin-top:5px"),
               verbatimTextOutput(outputId = 'mChiSqrTest'),
               div(style = "margin-top:5px"),
               uiOutput(outputId = 'mChiSqrTestComments')
               
             ) #TabPanel closure
           ) #tabSetpanel closure
    ) #Column Closure
  ) #fluidrow closure
) #UI closure






server <- function(input, output, session) {
  vmy <- reactiveValues(mydata=NULL)
  
  #############################################
  # Making Editable Temporary Table to get data
  #############################################
  observeEvent(input$mMakeDtTable,{
    # Create 9 rows and 9 clumns in a dataframe
    vmy$mydata <- data.frame(matrix(0,    # Create empty data frame
                                    nrow = 9,
                                    ncol = 9))
    vmy$mydata['Row_Header'] <- as.character()
    vmy$mydata <- vmy$mydata %>% dplyr::select(Row_Header, everything())
    vmy$mydata <- vmy$mydata %>% dplyr::select(Row_Header, everything())
    total <- vmy$mydata %>%
      summarise(across(where(is.numeric), sum)) %>%
      mutate(Row_Header = "Total")
    vmy$mydata <- rbind(vmy$mydata,total)
    vmy$mydata  <- vmy$mydata %>%
      mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE))
    
    disable(id = 'mMakeDtTable')
    fnCreateRenamedf()
  })
  
  
  #############################################
  # Editable Temporary Table to get data
  #############################################
  
  output$mEditableTbl <- DT::renderDataTable({
    if (length(vmy$mydata)==0){
      return()
    }
    n <- ncol(vmy$mydata)
    DT::datatable(vmy$mydata,
                  # class ='cell-border stripe compact white-space: nowrap', #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = TRUE,
                  options = list(columnDefs = list(list(className = "dt-center", targets = "_all")),
                                 list(width = '10px', targets = list(c(2:n))),
                                 dom = 't',ordering=F, pageLength = -1,class="compact",scrollX = T,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': 'steelblue', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}"))
                
    )%>% 
      formatStyle(c(1:dim(vmy$mydata)[2]), border = '0.5px solid #000000',`font-size` = '12px','font-weight'='bold')
  })
  
  
  proxy = dataTableProxy('mEditableTbl')
  
  observeEvent(input$mEditableTbl_cell_edit, {
    info = input$mEditableTbl_cell_edit
    i = info$row
    j = info$col+1
    v = info$value
    vmy$mydata[i, j] <- DT::coerceValue(v, vmy$mydata[i, j])
    replaceData(proxy, vmy$mydata, resetPaging = FALSE)  # important
    fnCreateRenamedf()
  })
  
  
  observeEvent(
    c(input$mMaxRows,
      input$ mMaxColumn,
      input$mUpdateTblTotals,
      input$mgrptitle,
      input$mTableBackColor),{
        vmy$mydata[is.na(vmy$mydata)] = 0
        fnUpdateContingencyTbl()
        fnCalcStatistics()
      })
  
  
  
  ######################################################################
  # Function to update total in Table and also output Contingency Table
  ######################################################################
  fnUpdateContingencyTbl <- function(){
    
    if (length(vmy$mydata)==0){
      return()
    }
    updateTabItems(session,inputId = "mTblSetPanel", selected = "Contingency Table")
    
    #Adding row total and column total
    n <- nrow(vmy$mydata)
    vmy$mydata <- vmy$mydata[-n,]
    n <- ncol(vmy$mydata)
    vmy$mydata <- vmy$mydata[,-n]
    
    total <- vmy$mydata %>% 
      summarise(across(where(is.numeric), sum)) %>% 
      mutate(Row_Header = "Total")
    vmy$mydata <- rbind(vmy$mydata,total)
    vmy$mydata  <- vmy$mydata %>%
      mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE))
    
    #Making unwanted rows and column as zero
    tempdf <- vmy$mydata
    r <- input$mMaxRows
    c <- input$mMaxColumn
    for (i in (r+1):nrow(tempdf)){
      tempdf[i,]<- 0
    }
    for (i in (c+2):ncol(tempdf)){
      tempdf[,i]<- 0
    }
    
    #Making once again row total and column total
    total <- tempdf %>% 
      summarise(across(where(is.numeric), sum)) %>% 
      mutate(Row_Header = "Total")
    tempdf <- rbind(tempdf,total)
    
    tempdf  <- tempdf %>%
      mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE))
    
    n <- nrow(tempdf)
    vmy$df <- tempdf[-c(1:n),]
    n <- ncol(tempdf)
    vmy$df['Sub_Category'] <- as.character()
    vmy$df  <- vmy$df %>%
      mutate(across(everything(), as.character))
    
    
    #Now making all calculations and adding to dataframe 
    mlast <- nrow(tempdf)
    n <- ncol(tempdf)
    for (i in 1:nrow(tempdf)){
      
      
      vmy$df[nrow( vmy$df) + 1,] = c(tempdf[i,1],as.character(tempdf[i,2]),as.character(tempdf[i,3]),
                                     as.character(tempdf[i,4]), as.character(tempdf[i,5]), as.character(tempdf[i,6]),
                                     as.character(tempdf[i,7]), as.character(tempdf[i,8]), as.character(tempdf[i,9]),
                                     as.character(tempdf[i,10]), as.character(tempdf[i,11]),"Count" )
      
      vmy$df[nrow( vmy$df) + 1,] = c("",fnpercent(tempdf[i,2]/tempdf[i,n],2),fnpercent(tempdf[i,3]/tempdf[i,n],2),
                                     fnpercent(tempdf[i,4]/tempdf[i,n],2),fnpercent(tempdf[i,5]/tempdf[i,n],2),
                                     fnpercent(tempdf[i,6]/tempdf[i,n],2),fnpercent(tempdf[i,7]/tempdf[i,n],2),
                                     fnpercent(tempdf[i,8]/tempdf[i,n],2),fnpercent(tempdf[i,9]/tempdf[i,n],2),
                                     fnpercent(tempdf[i,10]/tempdf[i,n],2),fnpercent(tempdf[i,11]/tempdf[i,n],2),
                                     paste("% within",tempdf[i,1]))
        vmy$df[nrow( vmy$df) + 1,] = c("", fnpercent(tempdf[i,2]/tempdf[mlast,2],2), fnpercent(tempdf[i,3]/tempdf[mlast,3],2), fnpercent(tempdf[i,4]/tempdf[mlast,4],2), 
                                       fnpercent(tempdf[i,5]/tempdf[mlast,5],2),fnpercent(tempdf[i,6]/tempdf[mlast,6],2),fnpercent(tempdf[i,7]/tempdf[mlast,7],2),
                                       fnpercent(tempdf[i,8]/tempdf[mlast,8],2), fnpercent(tempdf[i,9]/tempdf[mlast,9],2),fnpercent(tempdf[i,10]/tempdf[mlast,10],2),
                                       fnpercent(tempdf[i,11]/tempdf[mlast,11],2),paste("% within",input$mgrptitle))
      
      vmy$df[nrow( vmy$df) + 1,] =c("",fnpercent(tempdf[i,2]/tempdf[mlast,n],2), fnpercent(tempdf[i,3]/tempdf[mlast,n],2),
                                    fnpercent(tempdf[i,4]/tempdf[mlast,n],2),fnpercent(tempdf[i,5]/tempdf[mlast,n],2),fnpercent(tempdf[i,6]/tempdf[mlast,n],2),
                                    fnpercent(tempdf[i,7]/tempdf[mlast,n],2),fnpercent(tempdf[i,8]/tempdf[mlast,n],2),fnpercent(tempdf[i,9]/tempdf[mlast,n],2),
                                    fnpercent(tempdf[i,10]/tempdf[mlast,n],2),fnpercent(tempdf[i,11]/tempdf[mlast,n],2),paste("% within",'Total'))
    }
    
    #moving category from the end to second position
    msecondcolname <- names(vmy$df)[2]
    vmy$df <- vmy$df %>% relocate(Sub_Category, .before = all_of(msecondcolname))
    
    
    #removing un-wanted rows and columns from df table
    r <- input$mMaxRows
    c <- input$mMaxColumn
    n <- nrow(vmy$df)
    vmy$df <- vmy$df[-c((r*4+1):(n-4)),]
    n <- ncol(vmy$df)
    c <- if(c!=9){
      vmy$df <- vmy$df[,-c((c+3):(n-1))]
    }
  }
  
  
  
  ##############################################
  # function to make HTML KBL table final output
  ##############################################
  fnConstructKblTbl <- function() {
    mydf <<- vmy$mydata
    req(input$mgrptitle)
    
    #set table columns width
    mCol1Width     <-paste0(as.character(input$m1stColWidth),'em')
    mCol2Width     <-paste0(as.character(input$m2ndColWidth),'em')
    mColTotalWidth <-paste0(as.character(input$mTotalColWidth),'em')
    mColRestWidth  <-paste0(as.character(input$mOtherColWidth),'em')
    
    mHeaderFont   <-input$mHeaderColFont
    mTableFont    <-input$mTableColFont
    
    
    # Set a Title for the table
    if (input$mAlignCenter==TRUE){
      mtitle <- HTML(paste('<h4><STRONG><CENTER>',input$mContingencyTblTitle, '</CENTER></STRONG><h5>'))
    }else{
      mtitle <- HTML(paste('<h4><STRONG><RIGHT>',input$mContingencyTblTitle, '</RIGHT></STRONG><h5>'))
    }
    
    
    #set Group header / group title
    mgrptitle      <- input$mgrptitle
    
    # Set a named vector for dynamic header
    myGrpHeader <- c(" " = 2, mgrptitle = (ncol(vmy$df)-3), " " = 1)    #where you got this: https://stackoverflow.com/questions/45206908/kableextra-dynamic-add-header-above-labeling
    # set vector names
    names(myGrpHeader) <- c(" ", mgrptitle," ")
    
    #Border line Parameters
    mBorderLineRow <- c(seq(4,nrow(vmy$df),4))
    mBoldNumbers <- c(seq(1,nrow(vmy$df),4))
    
    # Column header parameter
    n <- nrow(vmy$df)
    c <- ncol(vmy$df)
    mcol.names = c("", "", names(vmy$df)[-c(1,2)])
    
    
    #Background and font color  Parameters
    mbackcolor <- input$mTableBackColor
    icol <- col2rgb(input$mTableBackColor, alpha = FALSE)
    yiq  <-  ((icol[1,1] * 299) + (icol[2,1] * 587) + (icol[3,1] * 114)) / 1000
    mfontcolor <- ifelse(yiq >=128,"black","white")
    
    #where you got this https://stackoverflow.com/questions/70871438/creating-horizontal-lines-after-each-collapsed-row-in-kableextra-table
    
    vmy$mKblTbl <- vmy$df %>%
      kbl(align = "llcccccccccc",caption =mtitle,linesep = "\\addlinespace",
          row.names=FALSE,escape=T,col.names = mcol.names) %>%
      kable_paper(c("striped"),full_width = T) %>%
      column_spec(1, bold = T, background = "#91D1C233",border_left = T) %>%
      column_spec(1,width = mCol1Width, border_right = T,background = mbackcolor,color = mfontcolor,italic = T,bold = FALSE) %>%
      column_spec(2,width = mCol2Width, border_right = T,background = mbackcolor,color = mfontcolor,italic = T,bold = FALSE) %>%
      column_spec(c,width = mColTotalWidth, border_right = T,italic = T,bold = FALSE) %>%
      column_spec(c(3:(c-1)), italic = T,border_right = T,width = mColRestWidth) %>%
      row_spec(0, bold = T, font_size = mHeaderFont,
               background = mbackcolor,color =mfontcolor,italic = T, 
               extra_css = "border-bottom: 1px solid;height:25px;") %>%
      row_spec(mBorderLineRow, extra_css = "border-bottom: 1px solid;") %>%
      row_spec(c(n), extra_css = "border-bottom: 1px solid;") %>%
      row_spec(mBoldNumbers, bold = T) %>% 
      row_spec(c(1:n),font_size = mTableFont) %>%
      add_header_above(header = myGrpHeader,line_sep = 10,font_size = mHeaderFont,border_left = T,
                       border_right = T,background = mbackcolor,color = mfontcolor,italic = T,
                       bold = T,extra_css ="border-bottom: 1px solid;height:25px;") %>%
      kable_classic_2(lightable_options = "basic",
                      html_font = "\"Arial Narrow\", \"Source Sans Pro\", sans-serif")
    
  }
  
  #############################################
  # OUtput kbl table
  #############################################
  
  library(dplyr)
  library(tidyr)
  library(knitr)
  
  output$mshowmatrixtbl <- renderUI({
    req(input$mgrptitle)
    fnConstructKblTbl()
    
    HTML(
      vmy$mKblTbl
    ) 
  })
  
  #############################################
  # Save kbl table as html table
  #############################################
  observeEvent(input$mSaveKbl2Html,{
    fnConstructKblTbl()
    save_kable(x = vmy$mKblTbl,file = "test2.html",
               bs_theme = "bootstrap",   #  'bootstrap' or 'simplex'
               self_contained = TRUE,
               extra_dependencies = NULL,
               latex_header_includes = NULL,
               keep_tex = FALSE,
               density = 800)
    browseURL('test2.html')
  })
  
  
  
  
  #############################################################################
  # Rename Data Frame columns  
  #############################################################################
  
  fnCreateRenamedf <- function(){
    df_rename <- data.frame("New_Name" = unlist(lapply(vmy$mydata, class)))
    df_rename['Var_name'] <- rownames(df_rename)
    df_rename['New_Name'] <- rownames(df_rename)
    row.names(df_rename) <- seq(1,nrow(df_rename))
    
    vmy$df_rename <- df_rename
    vmy$df_rename <-vmy$df_rename %>% dplyr::select('Var_name', everything())      
    
  }
  
  output$mColRenameTbl <- DT::renderDataTable({
    DT::datatable(vmy$df_rename,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = list(target = "cell", disable = list(columns = c(0))),
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  options = list(dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
  })
  
  
  proxy = dataTableProxy('mColRenameTbl')
  
  observeEvent(input$mColRenameTbl_cell_edit, {
    info = input$mColRenameTbl_cell_edit
    i = info$row
    j = info$col+1
    v = info$value
    vmy$df_rename[i, j] <- DT::coerceValue(v, vmy$df_rename[i, j])
    replaceData(proxy, vmy$df_rename, resetPaging = FALSE)  # important
  })
  
  
  observeEvent(input$mColRenameYesBtn,{
    n <- nrow(vmy$df_rename)
    for (i in 2:(n-1)){
      if (nchar(vmy$df_rename[i,2])==0){
        return()
      }
      n=which(colnames(vmy$mydata)== vmy$df_rename[i,1])
      colnames(vmy$mydata)[n] <- our_strwrap(vmy$df_rename[i,2])
    }
    vmy$df_rename[1,2] <- 'Row_Header'
    vmy$df_rename[n,2] <- 'Total'
    
    fnCreateRenamedf()
    fnUpdateContingencyTbl()
  })
  
  
  ##############################################
  # Data import from a comma separated String
  ##############################################
  observeEvent(input$mdatalistImportBtn,{
    library("stringr")
    list = str_split(string = input$mdatalistString, pattern = ",")
    v <- unlist(list)
    vv <<- v
    vlist <<-  list 
    if(length(input$mEditableTbl_rows_selected)>=1 ){
      i <- input$mEditableTbl_rows_selected
      
      vmy$mydata[i,1] <- v[1]
      vmy$mydata[i,2] <- v[2]
      vmy$mydata[i,3] <- v[3]
      vmy$mydata[i,4] <- v[4]
      vmy$mydata[i,5] <- v[5]
      vmy$mydata[i,6] <- v[6]
      vmy$mydata[i,7] <- v[7]
      vmy$mydata[i,8] <- v[8]
      vmy$mydata[i,9] <- v[9]
      vmy$mydata[i,10] <- v[10]
    }else{
      shinyalert::shinyalert(title = 'Error',text = 'Select a row in the above table..!',type = 'warning')
    }
    vmy$mydata[,2:11] <- sapply(vmy$mydata[,2:11],as.numeric)
  })
  
  
  fnCalcStatistics <- function(){
    if (length(vmy$mydata)==0){
      return()
    }
    if (vmy$mydata[10,11]==0){
      return()
    }
    dd <<- vmy$mydata
    dd <- dd[, !apply(dd == 0, 2, all)]
    dd <- dd%>%dplyr::filter(Total!=0)

    n <- nrow(dd)
    dd <- dd[-n,]
    n <- ncol(dd)
    dd <- dd[,-n]
    dd <- dd[,-1]
    
    
    # #removing un-wanted rows and columns from df table
    # r <- input$mMaxRows
    # c <- input$mMaxColumn
    # 
    # n <- nrow(dd)
    # dd <- dd[-c((r+1):n),]
    # n <- ncol(dd)
    # c <- if(c!=9){
    #   dd <- dd[,-c((c+2):n)]
    # }
    # dd <- dd[-1]
    # 
    output$mChiSqrTest <- renderPrint({
      chisq.test(dd)
    })
    
    pv <- chisq.test(dd)$p.value
    if (pv <0.05){
      pvTxt <- HTML(paste('<h5><b>',"Conclusion",'</b>','<br>',"Since the p-value of",as.character(round(pv,4)),"is less than 0.05 we would reject the NULL Hypothesisis",
                     "and conclude that there is a relation between row and column variables and they are dependent",'</b><h5>'))
    }else{
      pvTxt <- HTML(paste('<h5><b>',"Conclusion",'</b>','<br>',"Since the p-value of" ,as.character(round(pv,4)),"is greater than 0.05 we would ACCEPT the null Hypothesisis",
                     "and state that the distributions ARE EQUAL and there is no association or relationship",'</b><h5>'))
    }
    
    
    output$mChiSqrTestComments <- renderText({
      HTML(paste('<h5><b>',"Hypothesis Test",'</b>','<br>',"NULL Hypothesis in this scenario is that there is no significant difference between observed and expected values or",'<br>',
                 " there is no association between the row and column variables and they are independent ",'<br>',
                 "The Alternative Hypothesisis is that they are dependent or have association",pvTxt))
    })
    }

 
  
}
shinyApp(ui, server) 

