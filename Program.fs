open Saturn

let allRoutes = router {
    get "/" View.startingPage
    post "/search-suggestions" Api.suggestDestinations
    post "/do-search" Api.findEnergyReports
}

let app = application { use_router allRoutes }

run app