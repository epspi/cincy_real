require(rvest)
require(shiny)
require(leaflet)

bases <- c(
    'OpenStreetMap.HOT',
    'Thunderforest.Landscape',
    'Stamen.Terrain',
    'Stamen.Watercolor'
)

# Some constants
auto_refresh_time <- 3600 * 24
api_key <- '3a1e5f46619520940685de1d4cf630cc3ed92f9'
time_file_format  <- "%Y-%m-%d %H:%M:%S"

################################################
################ FUNCTIONS #####################
################################################

####### FUNCTION: GETGEOCODE ###################
## Gives entire google geocode json from address

getGeocode <- function(address) {

    param_list <- list()
    param_list$address = address
    #     if(!missing(tag)) param_list$tag = tag

    httr::GET('http://maps.googleapis.com/maps/api/geocode/json',
              query = param_list) %>%
        content %>%
        .[["results"]] %>%
        .[[1]]
}


####### FUNCTION: GETCOORD ####################
## Gives lat/long coordinates from address

getCoord <- function(address) {

    a <- getGeocode(address)
    a$geometry$location
}


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

####### FUNCTION: GEN_POPUP ####################
## Generate HTML for popup

gen_popup <- function(dat) {

    paste(sep = "<br/>",
        strong(dat["Address"]),
        dat["Appraisal.Amount"],
        dat["Judgement.Amount"],
        dat["Starting.Bid"],
        dat["Parcel"]
    ) %>% HTML
}

################################################
################### RUN ########################
################################################

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

pop <- dat %>%
    apply(1, gen_popup)
names(pop) <- NULL

dat %>%
    leaflet %>%
    addMarkers(~lng, ~lat, popup = pop) %>%
    addTiles(group = "OpenStreetMap.default") %>%
    addProviderTiles(bases[1], group = bases[1]) %>%
    addProviderTiles(bases[2], group = bases[2]) %>%
    addProviderTiles(bases[3], group = bases[3]) %>%
    addProviderTiles(bases[4], group = bases[4]) %>%
    addLayersControl(
        baseGroups = c("OpenStreetMap.default", bases),
        options = layersControlOptions(collapsed = FALSE)
    )
