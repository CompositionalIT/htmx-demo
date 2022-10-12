open Giraffe
open Saturn
open System

[<AutoOpen>]
module Domain =
    /// The input search request parameters.
    [<CLIMutable>]
    type RawSearchRequest = {
        SearchInput: string
        SortColumn: string
    }

    /// Represents a report for a specific country.
    type CountryReport = {
        Country: string
        Nuclear: float option
        EnergyImports: float option
        RenewableEnergyConsumption: float option
        FossilFuelEnergyConsumption: float option
    }

    /// The sort column to use.
    type SortColumn =
        | Renewables
        | Country
        | Imports
        | Fossil
        | Nuclear

        member this.AsString =
            match this with
            | Renewables -> "Renewables"
            | Country -> "Country"
            | Imports -> "Imports"
            | Fossil -> "Fossil"
            | Nuclear -> "Nuclear"

        static member TryOfString v =
            match v with
            | "Renewables" -> Some Renewables
            | "Country" -> Some Country
            | "Imports" -> Some Imports
            | "Fossil" -> Some Fossil
            | "Nuclear" -> Some Nuclear
            | _ -> None

    type SearchRequest =
        {
            SearchInput: string option
            SortColumn: SortColumn option
        }

        static member OfRawSearchRequest(request: RawSearchRequest) = {
            SearchInput = request.SearchInput |> Option.ofObj
            SortColumn = SortColumn.TryOfString request.SortColumn
        }

    /// Sorts the supplied reports using the specified sort column.
    let sortReports reports sortColumn =
        match sortColumn with
        | Country -> reports |> List.sortBy (fun c -> c.Country)
        | Imports -> reports |> List.sortByDescending (fun c -> c.EnergyImports)
        | Fossil -> reports |> List.sortByDescending (fun c -> c.FossilFuelEnergyConsumption)
        | Renewables -> reports |> List.sortByDescending (fun c -> c.RenewableEnergyConsumption)
        | Nuclear -> reports |> List.sortByDescending (fun c -> c.Nuclear)

module View =
    open Giraffe.Htmx
    open Giraffe.ViewEngine
    open Giraffe.ViewEngine.Htmx

    let private describe =
        function
        | None -> str "-"
        | Some v -> str $"%.2f{v}%%"

    /// Builds a table based on all reports.
    let createReportsTable reports =
        table [ _class "table" ] [
            thead [] [
                tr [] [
                    let makeTh value (column: SortColumn) =
                        th [
                            _hxInclude "#search-input"
                            _hxTarget "#search-results"
                            _hxPost "/do-search"
                            _hxTrigger "click"
                            _hxVals $"{{ \"sortColumn\" : \"{column.AsString}\" }}"
                            _style "cursor: pointer"
                        ] [ str value ]

                    makeTh "Country" Country
                    makeTh "Energy Imports (% of total)" Imports
                    makeTh "Renewables (% of total)" Renewables
                    makeTh "Fossil Fuels (% of total)" Fossil
                    makeTh "Nuclear & Other (% of total)" Nuclear
                ]
            ]
            tbody [] [
                for report in reports do
                    tr [] [
                        td [] [ str report.Country ]
                        td [] [ describe report.EnergyImports ]
                        td [] [ describe report.RenewableEnergyConsumption ]
                        td [] [ describe report.FossilFuelEnergyConsumption ]
                        td [] [ describe report.Nuclear ]
                    ]
            ]
        ]

    /// The initial start page of the application.
    let startingPage =
        html [] [
            head [] [
                link [
                    _rel "stylesheet"
                    _href "https://cdn.jsdelivr.net/npm/bootstrap@5.2.0/dist/css/bootstrap.min.css"
                ]
            ]
            Script.minified
            body [ _class "p-3 m-0 border-0 bd-example" ] [
                form [] [
                    div [ _class "mb-3" ] [
                        label [ _for "search-input"; _class "form-label" ] [ str "Find energy stats!" ]
                        datalist [ _id "search-suggestions" ] []
                        input [
                            _id "search-input"
                            _class "form-control"
                            _list "search-suggestions"
                            _name "searchinput"
                            _placeholder "Enter a country name"
                            _type "search"

                            _hxTrigger "keyup changed delay:500ms"
                            _hxPost "/search-suggestions"
                            _hxTarget "#search-suggestions"
                            _hxSwap HxSwap.OuterHtml
                        ]
                    ]
                    button [
                        _class "btn btn-primary"
                        _id "search-button"
                        _name "searchButton"
                        _type "button"

                        _hxPost "/do-search"
                        _hxInclude "#search-input"
                        _hxTarget "#search-results"
                    ] [
                        span [ _class "htmx-indicator spinner-border spinner-border-sm" ] []
                        str "Search!"
                    ]
                ]
                div [ _id "search-results"; _class "mt-3" ] []
            ]
        ]
        |> htmlView

    /// Creates a datalist for the supplied countries.
    let createCountriesSuggestions countries =
        datalist [ _id "search-suggestions" ] [
            for (country: string) in countries do
                option [ _value country ] []
        ]

module DataAccess =
    open FSharp.Data
    let private ctx = WorldBankData.GetDataContext()
    let private allCountries = ctx.Countries |> Seq.toList
    let private allRegions = ctx.Regions |> Seq.toList

    let private countriesAndRegions =
        let countries = allCountries |> List.map (fun country -> country.Name)
        let regions = allRegions |> List.map (fun region -> region.Name)
        countries @ regions

    let private containsText (text: string) (v: string) =
        v.Contains(text, StringComparison.CurrentCultureIgnoreCase)

    let private createReport (country: WorldBankData.ServiceTypes.Country) = {
        Country = country.Name
        Nuclear =
            country.Indicators.``Alternative and nuclear energy (% of total energy use)``.Values
            |> Seq.tryLast
        EnergyImports =
            country.Indicators.``Energy imports, net (% of energy use)``.Values
            |> Seq.tryLast
        RenewableEnergyConsumption =
            country.Indicators.``Renewable energy consumption (% of total final energy consumption)``.Values
            |> Seq.tryLast
        FossilFuelEnergyConsumption =
            country.Indicators.``Fossil fuel energy consumption (% of total)``.Values
            |> Seq.tryLast
    }

    /// Gets the top ten destinations that contain the supplied text.
    let findDestinations (text: string option) =
        match text with
        | None -> countriesAndRegions
        | Some text -> countriesAndRegions |> List.filter (containsText text)
        |> List.truncate 10

    /// Finds all country-level reports that contain the supplied text.
    let findReportsByCountries (text: string) =
        allCountries
        |> List.filter (fun country -> country.Name |> containsText text)
        |> List.truncate 20
        |> List.map createReport

    /// Looks for an exact match of a country or region based on the text supplied. If a region is matched, all
    /// countries within that region are returned.
    let tryExactMatchReport (text: string) =
        let matchingCountry =
            allCountries
            |> List.tryFind (fun c -> c.Name.Equals(text, StringComparison.CurrentCultureIgnoreCase))

        match matchingCountry with
        | Some country -> Some [ createReport country ]
        | None ->
            allRegions
            |> List.tryFind (fun region -> region.Name.Equals(text, StringComparison.CurrentCultureIgnoreCase))
            |> Option.map (fun region -> region.Countries |> Seq.map createReport |> Seq.toList)

module Api =
    open Microsoft.AspNetCore.Http

    /// Finds destinations to suggest.
    let suggestDestinations next (ctx: HttpContext) = task {
        let! request = ctx.BindModelAsync<RawSearchRequest>()
        let request = request |> SearchRequest.OfRawSearchRequest
        let countries = DataAccess.findDestinations request.SearchInput
        return! htmlView (View.createCountriesSuggestions countries) next ctx
    }

    /// Gets all energy reports using the query information supplied in the body.
    let findEnergyReports next (ctx: HttpContext) = task {
        let! request = ctx.BindModelAsync<RawSearchRequest>()
        let request = request |> SearchRequest.OfRawSearchRequest

        let reports =
            let unsorted =
                match request.SearchInput with
                | None -> DataAccess.findReportsByCountries ""
                | Some searchInput ->
                    DataAccess.tryExactMatchReport searchInput
                    |> Option.defaultWith (fun () -> DataAccess.findReportsByCountries searchInput)

            request.SortColumn
            |> Option.map (Domain.sortReports unsorted)
            |> Option.defaultValue unsorted

        return! htmlView (View.createReportsTable reports) next ctx
    }

let allRoutes = router {
    get "/" View.startingPage
    post "/search-suggestions" Api.suggestDestinations
    post "/do-search" Api.findEnergyReports
}

let app = application { use_router allRoutes }

run app
