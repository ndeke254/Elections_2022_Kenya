# application user interface
ui <- navbarPage(
  tags$style(HTML("

.selectize-input.items.not-full.has-options:before {
content:'';
background: url('kenya.png') ; /*url of image*/
height: 20px; /*height of image*/
width: 20px;  /*width of image*/
position: absolute;
 display: block;
 position: absolute;
 left: 0;
 background-size: 20px 20px;
 background-repeat: no-repeat;
 margin-left: 3px;
}

.selectize-input.dropdown-active:before {
    top: 0;
    margin-top: 6px;
 }
  
  .selectize-input.items.not-full.has-options {
    padding-left: 24px;
  }
 
 .selectize-input.items.not-full.has-options.has-items {
    padding-left: 0px;
 }
 
  .selectize-input.items.not-full.has-options .item:first-child {
      margin-left: 20px;
 }

")),
  header = tagList(
    useShinydashboard()
      ),
  title = tags$div(
    class ='logo',
    tags$img(
      checkboxInput(
        inputId = 'check',
        label = '',
        width = '12px')
    ),
    tags$a(
      tags$img(
        src = 'iebc.png',
        width = '60px'
      ),
      tags$img(
      actionButton(inputId = 'diaspora',
                   label = 'Diaspora')
      ),
      tags$img(
        src = 'shape.png',
        width = '50px'
      ),
      tags$img(
        src = 'choice.png',
        width = '120px'
      )
    )
  ),
 tabPanel(
   #create a select input
   selectizeInput(
     inputId = 'search_name',
                     label = 'County:',
                     width = '200px',
                     choices = NULL,
                     options = list(placeholder ='Search County',
                                    create = FALSE,
                                    maxOptions = 5,
                                    maxItems = '1',
                                    onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                    onType = I("function (str) {if (str === \"\") {this.close();}}"))
                     ),
   fluidRow(
     infoBoxOutput('county_name',
                   width = 2),
     valueBoxOutput('registered',
                    width = 2),
     valueBoxOutput('valid',
                    width = 2),
     valueBoxOutput('rejected',
                    width = 2),
     valueBoxOutput('raila',
                    width = 2),
     valueBoxOutput('ruto',
                    width = 2),
     valueBoxOutput('waihiga',
                    width = 2),
     valueBoxOutput('wajackoya',
                    width = 2),
     echarts4rOutput('clock') |> loading(),
   column(6,
            leafletOutput("livemap") |> loading_1()
   ),
   column(6,
            echarts4rOutput('graph') |> loading_1()
          )
   )
   ),
 includeCSS(path = "www/styles.css")
)


