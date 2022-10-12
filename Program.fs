open System
open Saturn
open Giraffe
open Giraffe.ViewEngine
open Giraffe.ViewEngine.Htmx
open Giraffe.Htmx
open Microsoft.AspNetCore.Http

[<AutoOpen>]
module Domain =
    [<CLIMutable>]
    type SearchRequest = {
        SearchInput: string
        SortColumn: string
    }

    [<Measure>]
    type Gbp

    type Report = {
        Country: string
        Nuclear: float option
        EnergyImports: float option
        RenewableEnergyConsumption: float option
        FossilFuelEnergyConsumption: float option
    }

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

module View =
    let describe =
        function
        | None -> str "-"
        | Some v -> str $"%.2f{v}%%"

    let searchResults reports =
        table [ _class "table" ] [
            thead [] [
                tr [] [
                    let makeTh value (column: SortColumn) =
                        th [
                            _hxInclude "#search-input"
                            _hxTarget "#search-results"
                            _hxPost "/do-search"
                            _hxTrigger "mouseenter"
                            _hxVals $"{{ \"sortColumn\" : \"{column.AsString}\" }}"
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

    let page =
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
                        label [ _for "search-input"; _class "form-label" ] [
                            str "Find fruit from a country"
                        ]
                        datalist [ _id "search-suggestions" ] []
                        input [
                            _id "search-input"
                            _class "form-control"
                            _list "search-suggestions"
                            _name "searchinput"
                            _placeholder "Enter a country name"
                            _type "search"

                            _hxPost "/search-suggestions"
                            _hxTarget "#search-suggestions"
                            _hxSwap HxSwap.OuterHtml
                            _hxTrigger "keyup changed delay:500ms"
                        ]
                    ]
                    button [
                        _class "btn btn-primary"
                        _id "search-button"
                        _name "searchButton"
                        _type "button"

                        _hxPost "/do-search"
                        _hxTarget "#search-results"
                        _hxInclude "#search-input"
                    ] [
                        span [ _class "htmx-indicator spinner-border spinner-border-sm" ] []
                        str "Search!"
                    ]
                ]
                div [ _id "search-results"; _class "mt-3" ] []
            ]
        ]

    let suggestions countries =
        datalist [ _id "search-suggestions" ] [
            for (country: string) in countries do
                option [ _value country ] []
        ]

module DataAccess =
    open FSharp.Data
    let ctx = WorldBankData.GetDataContext()
    let allCountries = ctx.Countries |> Seq.toList
    let allRegions = ctx.Regions |> Seq.toList

    let countriesAndRegions =
        let countries = allCountries |> List.map (fun country -> country.Name)
        let regions = allRegions |> List.map (fun region -> region.Name)
        countries @ regions

    let containsText (text: string) (v: string) =
        v.Contains(text, StringComparison.CurrentCultureIgnoreCase)

    let createReport (country: WorldBankData.ServiceTypes.Country) = {
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

    let findSuggestions (text: string) =
        countriesAndRegions |> List.filter (containsText text) |> List.truncate 10

    let getReportsByCountries (text: string) =
        allCountries
        |> List.filter (fun country -> country.Name |> containsText text)
        |> List.truncate 20
        |> List.toArray
        |> Array.Parallel.map createReport
        |> Array.toList

    let tryExactMatchReport (text: string) =
        let matchingCountry =
            allCountries
            |> List.tryFind (fun c -> c.Name.Equals(text, StringComparison.CurrentCultureIgnoreCase))

        match matchingCountry with
        | Some country -> Some [ createReport country ]
        | None ->
            allRegions
            |> List.tryFind (fun region ->
                region.Name.Equals(text, StringComparison.CurrentCultureIgnoreCase))
            |> Option.map (fun region -> region.Countries |> Seq.map createReport |> Seq.toList)

module Api =
    let getSuggestions next (ctx: HttpContext) = task {
        let! request = ctx.BindModelAsync<SearchRequest>()
        let countries = DataAccess.findSuggestions request.SearchInput
        return! htmlView (View.suggestions countries) next ctx
    }

    let doSearch next (ctx: HttpContext) = task {
        let! query = ctx.BindModelAsync<SearchRequest>()

        let reports =
            match query with
            | { SearchInput = ("" | null) } -> DataAccess.getReportsByCountries ""
            | _ ->
                DataAccess.tryExactMatchReport query.SearchInput
                |> Option.defaultWith (fun () -> DataAccess.getReportsByCountries query.SearchInput)

        let reports =
            match query.SortColumn |> SortColumn.TryOfString with
            | Some Country -> reports |> List.sortBy (fun c -> c.Country)
            | Some Imports -> reports |> List.sortByDescending (fun c -> c.EnergyImports)
            | Some Fossil ->
                reports |> List.sortByDescending (fun c -> c.FossilFuelEnergyConsumption)
            | Some Renewables ->
                reports |> List.sortByDescending (fun c -> c.RenewableEnergyConsumption)
            | Some Nuclear -> reports |> List.sortByDescending (fun c -> c.Nuclear)
            | None -> reports

        return! htmlView (View.searchResults reports) next ctx
    }

let theAppRouter = router {
    get "/" (htmlView View.page)
    post "/search-suggestions" Api.getSuggestions
    post "/do-search" Api.doSearch
}

let app = application { use_router theAppRouter }

run app
