# app.R
########### A SINGLE-FILE SHINY APP ##############

require(rvest)
require(httr)
require(shiny)
require(leaflet)
require(dplyr)
require(shinyBS)

##################################################
############ VARIABLES & CONSTANTS ###############
##################################################

# Mapping themes
providers <- c(
    'CartoDB.Positron',
    'OpenStreetMap.HOT',
    'Esri.WorldImagery',
    'Thunderforest.Landscape',
    'Hydda.Full',
    'Stamen.Terrain',
    'Stamen.Watercolor',
    'MapQuestOpen.Aerial',
    'MapBox',
    'HERE.hybridDay',
    'HERE.normalDay',
    'HERE.normalDayGrey'
)

# Mapping color palette for circles
pal <- colorFactor(c("#e41a1c", "#377eb8"), domain = c("Cancelled", "Scheduled"))

# Some constants
auto_refresh_time <- 3600 * 24
api_key <- '3a1e5f46619520940685de1d4cf630cc3ed92f9'
#api_key <- '786637d0b88317996dbb58b554bb8139d711879'
time_file_format  <- "%Y-%m-%d %H:%M:%S"
warren_redirect <- "http://www.co.warren.oh.us/auditor/property_search/prop_grid.asp?strSQL_CMD=SELECT+*+FROM+CAMAWEB.PRPTY+WHERE+SDWLL_NBR+like+'PARCEL_NUM%25'+ORDER+BY+SDWLL_NBR"
hamilton_redirect <- "http://www.hamiltoncountyauditor.org/realestateii/list_owner.asp?sid=%s&l_nm=owner&l_wc=|owner=PARCEL_OWNER&owner=PARCEL_OWNER"
hamilton_newsession_url <- "http://www.hamiltoncountyauditor.org/hamilton/default.asp"
cincy_url <- "http://apps.hcso.org/PropertySale.aspx"
instant_street <- 'https://www.instantstreetview.com/s/'
google_url <- "https://www.google.com/maps/place/"
bing_url <- "http://www.bing.com/mapspreview?&lvl=19&style=b&q="

##################################################
################## FUNCTIONS #####################
##################################################

####### FUNCTION: GETGEOCODE3 ####################
## Gives lat/long coordinates from address
getGeocode3 <- function(api_key, addresses) {

    param_list <- list()
    param_list$api_key = api_key
    #param_list$q = address

    httr::POST('https://api.geocod.io/v1/geocode',
               query = param_list,
               body = as.list(addresses), encode = "json") %>%
        content %>%
        .[["results"]]
}

####### FUNCTION: SIMPLIFY_COORD ###############
## Extracts coordinates from json retrieval
simplify_coord <- function(x) {
    x$response$results[[1]]$location
}

####### FUNCTION: GETCOORD3 ####################
## Gives lat/long coordinates from address
getCoord3 <- function(api_key, addresses) {

    getGeocode3(api_key, addresses) %>%
        lapply(simplify_coord) %>%
        lapply(data.frame, stringsAsFactors=FALSE) %>%
        do.call(rbind, .)
}


####### FUNCTION: CINCY1 ####################
## Inner scraper for Hamilton county for a single date
cincy1 <- function(session, form, date) {

    cat("Scraping Hamilton county for date ...",date,"\n")
    tmp.session <- submit_form(session,
                             form %>% set_values(ddlDate = date),
                             "btnGo") 
    
    tmp.avail <- tmp.session %>% 
        read_html %>% 
        html_nodes(".aspNetDisabled") %>% 
        html_nodes("input") %>% 
        html_attr("checked")
    
    tmp.table <- tmp.session %>%
        html_table %>%
        .[[1]] %>% 
        mutate(WD = ifelse(is.na(tmp.avail), "Scheduled", "Cancelled"))
}


####### FUNCTION: RESCRAPE #####################
## Fetch new data upon request
rescrape <- function() {

        cat("SCRAPING\n")
        ptm <- proc.time()

        # Warren County Scrape
        msg <- "Refreshing Warren County ..."
        cat(msg,"\n")
        setProgress(message = msg)
        dat <- read_html("http://www.wcsooh.org/SheriffSales/slsgrid.aspx") %>%
            html_nodes("table") %>%
            html_table() %>%
            data.frame %>%
            rename(Date = Date.of.Sale,
                   Status = Sale.Status,
                   Address = Property.Address,
                   Parcel = Parcel..,
                   MinBid = Starting.Bid,
                   Name = Defendant,
                   Appraisal = Appraisal.Amount,
                   Case = Case.Number) %>%
            mutate(Address =  gsub(",?   +,?", ", ", Address),
                   County = 'W') %>%
            select(Date, Status, Address, Name, Parcel, MinBid, Appraisal, Case, County)


        # Hamilton County Scrape
        msg <- "Refreshing Hamilton County ..."
        cat(msg, "\n")
        setProgress(message = msg)
        # submitting the form
        session <- cincy_url %>% html_session
        form <- session %>% html_form %>% .[[1]]

        session <- submit_form(session, form, "btnCurrent")
        form <- session %>% html_form %>% .[[1]]

        # Getting the valid dates
        cat("Getting valid dates\n")
        dates <- form$fields[["ddlDate"]]$options %>% as.character %>% .[!. == ""]

        # Looping over valid dates while scraping
        dat2 <- dates %>%
            lapply(function(x) cincy1(session, form, x)) %>%
            do.call(rbind, .) %>%
            rename(Date = SaleDate,
                   Status = WD,
                   Parcel = AttyPhone,
                   Case = CaseNO) %>%
            mutate(Address = paste0(Address,
                                    ", ",
                                    "CINCINNATI", #Township,
                                    ", OHIO"),
                   County = 'H') %>%
#             mutate(Address = gsub("CINTI", "CINCINNATI", Address)) %>% 
#             mutate(Address = gsub("TWSP", "TOWNSHIP", Address)) %>% 
            select(Date, Status, Address, Name, Parcel, MinBid, Appraisal, Case, County)
        
        # Combine counties
        msg <- "Combining counties ... "
        cat(msg,"\n")
        setProgress(message = msg)
        dat <- rbind(dat, dat2)

        # Get latitude longitude
        msg <- "Fetching coordinates ... "
        cat(msg,"\n")
        setProgress(message = msg)
        dat <- dat$Address %>%
            getCoord3(api_key, .) %>%
            cbind(dat, .)

        # Output time
        print(proc.time() - ptm)

        # Save output
        write.csv(Sys.time(), "CSV/timestamp.csv", row.names = F)
        write.csv(dat, "CSV/dat.csv",row.names = F)
}

####### FUNCTION: ADDPROVIDERTILES_RECURSIVE ##############
## Dynamically add N provider tiles
addProviderTiles_recursive <- function(map, providers) {

    tmp <- addProviderTiles(map,
                            providers[1],
                            group = providers[1]
                            #options = providerTileOptions(noWrap = TRUE)
    )

    if (length(providers) > 1) {
        tmp %>% addProviderTiles_recursive(providers[-1])
    } else {
        tmp
    }
}


####### FUNCTION: GEN_POPUP ####################
## Generate HTML for popup
gen_popup <- function(dat, ham.cookie) {

    paste(sep = "</br>",
          dat["Address"] %>% h4(),
          paste0(strong("Date: "), dat["Date"]),
          paste0(strong("A "), dat["Appraisal"],
                 strong('  /  S '), dat["MinBid"]),
          #dat["Parcel"],
          paste0(
              # Auditor link
              a(href =
                    ifelse(dat["County"] == "W",
                           dat["Parcel"] %>%
                               gsub("-","",.) %>%
                               gsub("PARCEL_NUM", . , warren_redirect),
                           dat["Name"] %>%
                               gsub(",", "", .) %>%
                               gsub("PARCEL_OWNER", . , hamilton_redirect %>% sprintf(ham.cookie))
                           ),
                target="_blank",
                dat["Parcel"]),

              # Street View link
              ' / ',
              a(href = dat["Address"] %>% paste0(instant_street, .),
                target="_blank",
                'Street'),
              ' / ',
              a(href = dat["Address"] %>% paste0(google_url, .),
                target="_blank",
                'Google'),
              ' / ',
              a(href = dat["Address"] %>% paste0(bing_url, .),
                target="_blank",
                'Bing')
          )
    )
}



##################################################
##################### UI #########################
##################################################

ui <- navbarPage("Cincy Real", id = "nav",
                 tabPanel(
                     "Map",
                     div(class="outer",
                         tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css")
                             #includeScript("gomap.js")
                         ),
                         leafletOutput("mymap", width="100%", height="100%")
#                          div(class = "btn-toolbar pull-right",
#                              div(class = "btn-group btn-group-sm",
#                                  actionButton("property_show_table", label = "List", icon = icon("list"))
#                              )
#                          ),
#                              
#                          bsModal(id = "property_modal", title = "Keywords", 
#                                  trigger = "property_show_table", 
#                                  size = "small",
#                                  DT::dataTableOutput('property_table')
#                          )
                     )
                 ),
                 tabPanel(
                     "Settings",
                     actionButton(
                         "refresh", label = "Refresh", icon = icon("refresh")
                     ),
                     actionButton("property_show_table", label = "List", icon = icon("list")),
                     
                     a("Hamilton County Property Sales", href = "http://apps.hcso.org/PropertySale.aspx"),
                     bsModal(id = "property_modal", title = "Properties List", 
                             trigger = "property_show_table", 
                             #size = "small",
                             DT::dataTableOutput('property_table')
                     )
                 )
)


##################################################
#################### SERVER ######################
##################################################

server <- function(input, output, session) {

    ####### RUN INITIAL #####################
    withProgress(message = "Loading data ...", {
        # Recording current time and checking timestamp from previously downloaded data
        curtime  <- Sys.time()
        lasttime <- try(
            read.csv("CSV/timestamp.csv", stringsAsFactors = F) %>% unlist,
            silent = T)

        if (class(lasttime) == "try-error")
        {
            lasttime <- curtime  - auto_refresh_time
            cat("No Scrape History Found", "\n\n")

        } else {
            lasttime <- lasttime %>%
                strptime(time_file_format) %>%
                as.POSIXct

            cat("Last Scrape:  ",
                format(lasttime, time_file_format, usetz = T, tz = "EST5EDT"),
                "\n")
        }

        # Status Updates
        cat("Current Time: ",
            format(curtime, time_file_format, usetz = T, tz = "EST5EDT"),
            "\n")
        cat("Refresh Due:  ",
            format(lasttime + auto_refresh_time, time_file_format, usetz = T, tz = "EST5EDT"),
            "\n\n")

        # Rescrape if due time
        if (curtime >= lasttime + auto_refresh_time) {
            withProgress(message = "Refreshing data ...", rescrape())
        }

        cat('about to create hamilton cookie\n')
        # Establish new Hamilton county session
        hamilton.cookie <- hamilton_newsession_url %>% 
            html_session %>% 
            cookies() %>% .[["value"]] %>% .[1]
        
        cat('created hamilton cookie: ', hamilton.cookie,'\n')
        # Read in auction data
        dat  <- "CSV/dat.csv" %>%
            read.csv(stringsAsFactors = F)

        pop <- dat %>%
            apply(1, function(x) gen_popup(x, hamilton.cookie))
        names(pop) <- NULL

    })

    ####### OUTPUT: PROPERTY_LIST #####################
    output$property_table <- renderDataTable(
        {dat %>% select(-c(lat,lng))},
        #style = 'bootstrap',
        #width = "50%",
        #server = F,
        #class = 'hover',
        options = list(dom = 'fitp', pageLength = 25)
    )
    
    
    ####### OUTPUT: MYMAP #####################
    output$mymap <- renderLeaflet({
        leaflet(dat) %>%
            addTiles(group = "OpenStreetMap.default") %>%
            addProviderTiles_recursive(providers) %>%
            addLayersControl(
                baseGroups = c(providers, "OpenStreetMap.default"),
                overlayGroups = c("Quakes", "Outline"),
                position = 'bottomleft',
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            addCircleMarkers(~lng, ~lat,
                       popup = pop,
                       clusterOptions = markerClusterOptions(),
                       #radius = 10,
                       radius = ~sqrt(as.numeric(gsub("[$,]","", Appraisal))/1000),
                       color = ~pal(Status),
                       stroke = FALSE, fillOpacity = 0.65
            )
    })
    
    ####### OBSERVER: REFRESH #####################
    observeEvent(input$refresh, {
        withProgress(message = "Refreshing data ...", rescrape())
        
        dat  <- "CSV/dat.csv" %>%
            read.csv(stringsAsFactors = F)
        
        pop <- dat %>%
            apply(1, function(x) gen_popup(x, hamilton.cookie))
        names(pop) <- NULL
    })
    
    ####### OBSERVER: MARKER CLICK #####################
    observe({
        tmp <- input$mymap_marker_click
        cat("\nclick\n")
        tmp %>% unlist %>% print
    })

    ####### OBSERVER: MARKER HOVER #####################
    observe({
        tmp <- input$mymap_marker_mouseover
        cat("\nmouseover\n")
        tmp %>% unlist %>% print
    })
}

shinyApp(ui, server)