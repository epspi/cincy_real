# app.R
########### A SINGLE-FILE SHINY APP ##############

require(rvest)
require(httr)
require(shiny)
require(leaflet)
require(dplyr)

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

# Some constants
auto_refresh_time <- 3600 * 24
api_key <- '3a1e5f46619520940685de1d4cf630cc3ed92f9'
time_file_format  <- "%Y-%m-%d %H:%M:%S"
warren_redirect <- "http://www.co.warren.oh.us/auditor/property_search/prop_grid.asp?strSQL_CMD=SELECT+*+FROM+CAMAWEB.PRPTY+WHERE+SDWLL_NBR+like+'PARCEL_NUM%25'+ORDER+BY+SDWLL_NBR"


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

####### FUNCTION: RESCRAPE #####################
## Fetch new data upon request
rescrape <- function() {
    dat <- html("http://www.wcsooh.org/SheriffSales/slsgrid.aspx") %>%
        html_nodes("table") %>%
        html_table() %>%
        data.frame %>%
        select(-Var.1) %>%
        rename(Address = Property.Address, Parcel = Parcel..) %>%
        mutate(Address =  gsub(",?   +,?", ", ", Address))

    dat <- dat$Address %>%
        getCoord3(api_key, .) %>%
        cbind(dat, .)

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
gen_popup <- function(dat) {

    paste(sep = "</br>",
        h4(dat["Address"]),
        paste0(strong("A "), dat["Appraisal.Amount"],
               strong('  /  S '), dat["Starting.Bid"],
               strong("  /  J "), dat["Judgement.Amount"]),
        #dat["Parcel"],
        a(href = dat["Parcel"] %>%
              gsub("-","",.) %>%
              gsub("PARCEL_NUM", . , warren_redirect),
          target="_blank",
          dat["Parcel"]),
          #tags$i(class="fa fa-home")
        icon("home")
    ) #%>% paste0
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
                     )
                 ),
                 tabPanel(
                     "Settings"
                 )
)


##################################################
#################### SERVER ######################
##################################################

server <- function(input, output, session) {

    ####### RUN INITIAL #####################
    {
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
            rescrape()
        }

        # Filter out auctions that have already passed
        dat  <- "CSV/dat.csv" %>%
            read.csv(stringsAsFactors = F)

        pop <- dat %>%
            apply(1, gen_popup)
        names(pop) <- NULL

    }

    ####### OUTPUT: MYMAP #####################
    output$mymap <- renderLeaflet({
        leaflet(dat) %>%
            addMarkers(~lng, ~lat, popup = pop) %>%
            addTiles(group = "OpenStreetMap.default") %>%
            addProviderTiles_recursive(providers) %>%
            addLayersControl(
                baseGroups = c(providers, "OpenStreetMap.default"),
                position = 'bottomleft',
                options = layersControlOptions(collapsed = TRUE)
            )
    })
}

shinyApp(ui, server)