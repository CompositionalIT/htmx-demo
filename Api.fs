[<RequireQualifiedAccess>]
module Api

open FsToolkit.ErrorHandling
open Domain
open Giraffe

open Microsoft.AspNetCore.Http

/// Finds destinations to suggest.
let suggestDestinations next (ctx: HttpContext) = taskOption {
    let! request =
        ctx.BindModelAsync<RawSearchRequest>()
        |> Task.map SearchRequest.OfRawSearchRequest

    let destinations = DataAccess.findDestinations request.SearchInput
    return! htmlView (View.createCountriesSuggestions destinations) next ctx
}

/// Gets all energy reports using the query information supplied in the body.
let findEnergyReports next (ctx: HttpContext) = task {
    let! request =
        ctx.BindModelAsync<RawSearchRequest>()
        |> Task.map SearchRequest.OfRawSearchRequest

    let reports =
        match request.SearchInput with
        | None -> DataAccess.findReportsByCountries request.Sort ""
        | Some searchInput ->
            DataAccess.tryExactMatchReport request.Sort searchInput
            |> Option.defaultWith (fun () -> DataAccess.findReportsByCountries request.Sort searchInput)

    return! htmlView (View.createReportsTable request.Sort reports) next ctx
}