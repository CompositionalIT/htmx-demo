namespace Domain

open FsToolkit.ErrorHandling

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
    Code: string option
    Nuclear: float
    EnergyImports: float
    RenewableEnergyConsumption: float
    FossilFuelEnergyConsumption: float
}

type NumericColumn =
    | Renewables
    | Imports
    | Fossil
    | Nuclear

type TextColumn = Country

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

type SearchRequest = {
    SearchInput: string option
    Sort: Sort
} with

    static member OfRawSearchRequest(request: RawSearchRequest) = {
        SearchInput = request.SearchInput |> Option.ofObj
        Sort =
            let userSort = option {
                let! col = SortColumn.TryOfString request.SortColumn
                let! dir = SortDirection.TryOfString request.SortDirection
                return col, dir
            }

            userSort |> Option.defaultValue (TextColumn Country, Ascending)
    }

module Seq =
    /// Sorts the supplied reports using the specified sort column.
    let pickSort (column, direction) =
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