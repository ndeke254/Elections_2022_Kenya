# application user interface
ui <- navbarPage(
  windowTitle = tags$head(
    tags$link(rel = "icon",
              type = "image/png",
              href = "shape.png"),
    tags$title("Election Results 2022")),
  tags$audio(src = "sound.mp3", type = "audio/mp3",
             autoplay = TRUE, loop = TRUE),
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
  bsTooltip("check", "Extract county", "right", "hover"),
  bsTooltip("twitter", "Twitter", "bottom", "hover"),
  bsTooltip("github", "Github", "bottom", "hover"),
  bsTooltip("linkedin", "LinkedIn", "bottom", "hover"),
  header = tagList(
    useShinydashboard(),
    absolutePanel(
      id = "timer",
      class = "panel panel-default",
      top = 10, 
      left = "auto", 
      right = "auto",
      bottom ="auto",
      width = "auto", 
      height = "auto",
      tags$div(
        class = "p2",
        tags$div(
          class = "p3",
          tags$div(
            class = "flag",
          img(
            src = "kenya.png"
          )
          ),
          tags$div(
            "KENYA DECIDES 2027"
          )
        ),
        tags$div(
          class = "p6",
          flipdownr::flipdown(
            downto = "2024-04-30 00:00:00 EAT",
            id = "flipdown", 
            theme = "youkous"
            )
          )
        )
      ),
    absolutePanel(
      id = "socials",
      class = "panel panel-default",
      top = 10,
      left = "auto",
      right = 60,
      bottom ="auto",
      width = 140,
      height = "auto",
      tags$div(class = "p2",
               tags$div(
                 class = "p3",
                 tags$div(
                   class = "flag",
                 tags$img(src = "author.png"),
                 ),
                 tags$div(
                   "FIND ME"
                   )
               ),
               tags$div(
                 class = "p7",
                 tags$li(
                   tags$div(
                     class = "p8",
                     actionLink(
                       "linkedin",
                       label = "",
                       icon = icon("linkedin"),
                       onclick = "window.open('https://www.linkedin.com/in/jefferson-ndeke-027062202/')"
                       )
                     )
                   ),
                 tags$li(
                   tags$div(
                     class = "p8",
                     actionLink(
                       "github",
                       label = "",
                       icon = icon("github"),
                       onclick = "window.open('https://github.com/ndeke254')")
                     )
                   ),
                 tags$li(
                   tags$div(
                     class = "p8",
                     actionLink(
                       "twitter",
                       label = "",
                       icon = icon("twitter"),
                       onclick = "window.open('https://twitter.com/jefferson_ndeke')"
                       )
                     )
                   )
                 ),
               tags$a(href = "https://www.iebc.or.ke/resources/", "Data: IEBC",
                      target = "_blank")
               )
      ),
    tags$div(
      id = "extract",
      Toggle.shinyInput("check", value = FALSE)
    )
  ),
  title = tags$div(
    class = "logo",
    tags$a(
      tags$img(
        class = "button",
        actionButton(inputId = "diaspora",
                     label = "DIASPORA")
      ),
      tags$img(
        src = "iebc.png",
        width = "60px"
      )
    ),
    tags$a(
      tags$img(
        src = "shape.png",
        width = "50px"
      ),
      tags$img(
        src = "choice.png",
        width = "120px"
      ))
  ),
  tabPanel(
    
    #create a select input
    selectizeInput(
      inputId = "search_name",
      label = "COUNTY:",
      width = "200px",
      choices = NULL,
      options = list(placeholder = "Search County",
                     create = FALSE,
                     maxOptions = 5,
                     maxItems = "1",
                     onDropdownOpen = I("function($dropdown) 
                                    {if (!this.lastQuery.length) 
                                    {this.close(); this.settings.
                                    openOnFocus = false;}}"),
                     onType = I("function (str)
                                     {if (str === \"\") {this.close();}}"))
    ),
    fluidRow(
      infoBoxOutput("county_name",
                    width = 2),
      valueBoxOutput("registered",
                     width = 2),
      valueBoxOutput("valid",
                     width = 2),
      valueBoxOutput("rejected",
                     width = 2),
      valueBoxOutput("raila",
                     width = 2),
      valueBoxOutput("ruto",
                     width = 2),
      valueBoxOutput("waihiga",
                     width = 2),
      valueBoxOutput("wajackoya",
                     width = 2),
      echarts4rOutput("clock") |> loading(),
      column(6,
             leafletOutput("livemap") |> loading_1()
      ),
      column(6,
             echarts4rOutput("graph") |> loading_1()
      )
    )
  ),
  includeCSS(path = "www/styles.css")
)