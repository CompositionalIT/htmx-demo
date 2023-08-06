[<RequireQualifiedAccess>]
module View

open Domain
open Giraffe
open Giraffe.Htmx
open Giraffe.ViewEngine
open Giraffe.ViewEngine.Htmx

module private Helpers =
    let buildTable children =
        div [ _class "page" ] [
            div [ _class "page-wrapper" ] [
                div [ _class "page-body" ] [
                    div [ _class "container-xl" ] [
                        div [ _class "row row-cards" ] [ div [ _class "col-12" ] children ]
                    ]
                ]
            ]
        ]

module private Header =
    let linksAndScripts = [
        link [
            _rel "stylesheet"
            _href "https://cdn.jsdelivr.net/npm/@tabler/core@latest/dist/css/tabler.min.css"
        ]
        link [
            _rel "stylesheet"
            _href "https://cdn.jsdelivr.net/npm/@tabler/core@latest/dist/css/tabler-flags.min.css"
        ]
        script [
            _src "https://cdn.jsdelivr.net/npm/@tabler/core@latest/dist/js/tabler.min.js"
        ] []
    ]

module private SearchElements =
    let input =
        div [ _class "row" ] [
            div [] [
                label [ _for "search-input"; _class "form-label" ] [ str "Search Term" ]
                datalist [ _id "search-suggestions" ] []
                div [ _class "input-icon mb-3" ] [
                    input [
                        _id "search-input"
                        _class "form-control"
                        _list "search-suggestions"
                        _name "searchinput"
                        _placeholder "Enter an exact or partial country, or a region."
                        _type "search"

                        _hxTrigger "keyup changed delay:500ms"
                        _hxPost "/search-suggestions"
                        _hxTarget "#search-suggestions"
                        _hxSwap HxSwap.OuterHtml
                    ]
                    span [ _id "spinner"; _class "htmx-indicator input-icon-addon" ] [
                        div [ _class "spinner-border spinner-border-sm"; attr "role" "status" ] []
                    ]
                ]
            ]
        ]

    let button =
        button [
            _class "btn btn-primary"
            _id "search-button"
            _name "searchButton"
            _type "submit"

            _hxPost "/do-search"
            _hxInclude "#search-input"
            _hxTarget "#search-results"
            _hxIndicator "#spinner"
        ] [ str " Search!" ]

    let title =
        h4 [ _class "card-title" ] [
            str "World Bank Energy Statistics"
            span [ _class "card-subtitle" ] [ str "F#, HTMX and Tabler demonstrator" ]
        ]

/// The initial start page of the application.
let startingPage: HttpHandler =
    html [] [
        head [] Header.linksAndScripts
        Script.minified
        body [ _class "theme-light" ] [
            div [ _class "page" ] [
                div [ _class "page-wrapper" ] [
                    div [ _class "page-body" ] [
                        div [ _class "container-xl" ] [
                            div [ _class "row row-cards" ] [
                                div [ _class "col-12" ] [
                                    form [ _class "card" ] [
                                        div [ _class "card-header card-header-light" ] [ SearchElements.title ]
                                        div [ _class "card-body" ] [ SearchElements.input ]
                                        div [ _class "card-footer d-flex" ] [ SearchElements.button ]
                                    ]
                                    div [ _id "search-results"; _class "mt-3" ] []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
    |> htmlView

/// Builds a table based on all reports.
let createReportsTable sort reports =
    let pickCellColour (success, warning) value =
        [ "success", success; "warning", warning ]
        |> List.tryFind (fun (_, f) -> f value)
        |> Option.map fst
        |> Option.defaultValue "danger"
        |> sprintf "bg-%s"

    let buildProgressBarCell value pickers =
        let colour = value |> pickCellColour pickers

        td [] [
            div [ _class "row align-items-center" ] [
                div [ _class "col-12 col-lg-auto" ] [
                    div [ _class "progress"; _style "width: 5rem" ] [
                        div [
                            _class $"progress-bar {colour}"
                            _style $"width: {value}%%"
                            attr "role" "progressbar"
                        ] []
                    ]
                ]
                div [ _class "col" ] [ str $"%.2f{value}%%" ]
            ]
        ]

    let higherIsBetter = (fun x -> x > 40.), (fun x -> x > 10.)
    let lowerIsBetter = (fun x -> x < 25.), (fun x -> x < 50.)

    div [ _class "card" ] [
        div [ _id "table-default"; _class "table-responsive" ] [
            table [ _class "table card-table table-striped datatable" ] [
                thead [] [
                    tr [] [
                        let makeTh value (column: SortColumn) =
                            th [] [
                                button [
                                    let nextDirection, sortHeader =
                                        match sort with
                                        | currentColumn, Ascending when column = currentColumn -> Descending, "asc"
                                        | currentColumn, Descending when column = currentColumn -> Ascending, "desc"
                                        | _ -> Ascending, ""

                                    _class $"table-sort {sortHeader}"

                                    _hxInclude "#search-input"
                                    _hxTarget "#search-results"
                                    _hxPost "/do-search"
                                    _hxTrigger "click"

                                    _hxVals
                                        $"{{ \"sortColumn\" : \"{column.AsString}\", \"sortDirection\" : \"{nextDirection.AsString}\" }}"
                                ] [ str value ]
                            ]

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
                            td [] [
                                match report.Code with
                                | Some code -> span [ _class $"flag flag-country-{code}" ] []
                                | None -> ()
                                str $" {report.Country} "
                            ]
                            buildProgressBarCell report.EnergyImports lowerIsBetter
                            buildProgressBarCell report.RenewableEnergyConsumption higherIsBetter
                            buildProgressBarCell report.FossilFuelEnergyConsumption lowerIsBetter
                            buildProgressBarCell report.Nuclear higherIsBetter
                        ]
                ]
            ]
        ]
    ]

/// Creates a datalist for the supplied countries.
let createCountriesSuggestions destinations =
    datalist [ _id "search-suggestions" ] [
        for destination: string in destinations do
            option [ _value destination ] []
    ]