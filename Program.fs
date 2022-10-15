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
        SortDirection: string
    }

    /// Represents a report for a specific country.
    type CountryReport = {
        Country: string
        Nuclear: float option
        EnergyImports: float option
        RenewableEnergyConsumption: float option
        FossilFuelEnergyConsumption: float option
    }

    type NumericColumn =
        | Renewables
        | Imports
        | Fossil
        | Nuclear

    type TextColumn = | Country

    /// The sort column to use.
    type SortColumn =
        | TextColumn of TextColumn
        | NumericColumn of NumericColumn

        member this.AsString =
            match this with
            | TextColumn Country -> "Country"
            | NumericColumn Renewables -> "Renewables"
            | NumericColumn Imports -> "Imports"
            | NumericColumn Fossil -> "Fossil"
            | NumericColumn Nuclear -> "Nuclear"

        static member TryOfString v =
            match v with
            | "Country" -> Some(TextColumn Country)
            | "Renewables" -> Some(NumericColumn Renewables)
            | "Imports" -> Some(NumericColumn Imports)
            | "Fossil" -> Some(NumericColumn Fossil)
            | "Nuclear" -> Some(NumericColumn Nuclear)
            | _ -> None

    type SortDirection =
        | Ascending
        | Descending

        member this.AsString =
            match this with
            | Ascending -> "Ascending"
            | Descending -> "Descending"

        static member TryOfString v =
            match v with
            | "Ascending" -> Some Ascending
            | "Descending" -> Some Descending
            | _ -> None

    type Sort = SortColumn * SortDirection

    type SearchRequest =
        {
            SearchInput: string option
            Sort: Sort option
        }

        static member OfRawSearchRequest(request: RawSearchRequest) = {
            SearchInput = request.SearchInput |> Option.ofObj
            Sort =
                match SortColumn.TryOfString request.SortColumn, SortDirection.TryOfString request.SortDirection with
                | Some col, Some dir -> Some(col, dir)
                | _ -> None
        }

    /// Sorts the supplied reports using the specified sort column.
    let sortBy (column, direction) =
        match column with
        | TextColumn Country ->
            match direction with
            | Ascending -> Seq.sortBy (fun c -> c.Country)
            | Descending -> Seq.sortByDescending (fun c -> c.Country)
        | NumericColumn column ->
            let column =
                match column with
                | Imports -> fun c -> c.EnergyImports
                | Fossil -> fun c -> c.FossilFuelEnergyConsumption
                | Nuclear -> fun c -> c.Nuclear
                | Renewables -> fun c -> c.RenewableEnergyConsumption

            match direction with
            | Ascending -> Seq.sortBy column
            | Descending -> Seq.sortByDescending column

module View =
    open Giraffe.Htmx
    open Giraffe.ViewEngine
    open Giraffe.ViewEngine.Htmx

    let private describe =
        function
        | None -> str "-"
        | Some v -> str $"%.2f{v}%%"


    /// Builds a table based on all reports.
    let createReportsTable currentSort reports =
        let pickCellColour (success, warning, danger) value =
            [ "success", success; "warning", warning; "danger", danger ]
            |> List.tryFind ((fun (_, f) -> f value))
            |> Option.map (fst >> sprintf "table-%s" >> _class)

        let buildCell field pickers =
            let colour = field |> Option.bind (pickCellColour pickers) |> Option.toList
            td [ yield! colour ] [ describe field ]

        let higherIsBetter = (fun x -> x > 40.), (fun x -> x > 10.), (fun _ -> true)
        let lowerIsBetter = (fun x -> x < 10.), (fun x -> x < 40.), (fun _ -> true)

        table [ _class "table table-bordered table-sm" ] [
            thead [ _class "table-dark" ] [
                tr [] [
                    let makeTh value (column: SortColumn) =
                        th [
                            let nextDirection =
                                match currentSort with
                                | Some (currentColumn, Ascending) when column = currentColumn -> Descending
                                | _ -> Ascending

                            _hxInclude "#search-input"
                            _hxTarget "#search-results"
                            _hxPost "/do-search"
                            _hxTrigger "click"

                            _hxVals
                                $"{{ \"sortColumn\" : \"{column.AsString}\", \"sortDirection\" : \"{nextDirection.AsString}\" }}"

                            _style "cursor: pointer"
                        ] [ str value ]

                    makeTh "Country" (TextColumn Country)
                    makeTh "Energy Imports (% of total)" (NumericColumn Imports)
                    makeTh "Renewables (% of total)" (NumericColumn Renewables)
                    makeTh "Fossil Fuels (% of total)" (NumericColumn Fossil)
                    makeTh "Nuclear & Other (% of total)" (NumericColumn Nuclear)
                ]
            ]
            tbody [ _class "table-group-divider" ] [
                for report in reports do
                    tr [] [
                        td [ _class "table-light" ] [ str report.Country ]
                        buildCell report.EnergyImports lowerIsBetter
                        buildCell report.RenewableEnergyConsumption higherIsBetter
                        buildCell report.FossilFuelEnergyConsumption lowerIsBetter
                        buildCell report.Nuclear higherIsBetter
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
    let findReportsByCountries sort (text: string) =
        allCountries
        |> Seq.filter (fun country -> country.Name |> containsText text)
        |> Seq.truncate 100
        |> Seq.map createReport
        |> fun reports ->
            match sort with
            | Some sort -> reports |> sortBy sort
            | None -> reports
        |> Seq.toList

    /// Looks for an exact match of a country or region based on the text supplied. If a region is matched, all
    /// countries within that region are returned.
    let tryExactMatchReport sortColumn (text: string) =
        let matchingCountry =
            allCountries
            |> List.tryFind (fun c -> c.Name.Equals(text, StringComparison.CurrentCultureIgnoreCase))

        match matchingCountry with
        | Some country -> Some [ createReport country ]
        | None ->
            allRegions
            |> List.tryFind (fun region -> region.Name.Equals(text, StringComparison.CurrentCultureIgnoreCase))
            |> Option.map (fun region ->
                region.Countries
                |> Seq.map createReport
                |> fun reports ->
                    match sortColumn with
                    | Some column -> reports |> sortBy column
                    | None -> reports
                |> Seq.toList)

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
            match request.SearchInput with
            | None -> DataAccess.findReportsByCountries request.Sort ""
            | Some searchInput ->
                DataAccess.tryExactMatchReport request.Sort searchInput
                |> Option.defaultWith (fun () -> DataAccess.findReportsByCountries request.Sort searchInput)

        return! htmlView (View.createReportsTable request.Sort reports) next ctx
    }

let allRoutes = router {
    get "/" View.startingPage
    post "/search-suggestions" Api.suggestDestinations
    post "/do-search" Api.findEnergyReports
}

let app = application { use_router allRoutes }

run app
