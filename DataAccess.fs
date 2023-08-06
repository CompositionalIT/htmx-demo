[<RequireQualifiedAccess>]
module DataAccess

open Domain
open FSharp.Data
open FsToolkit.ErrorHandling
open Microsoft.Extensions.Caching.Memory
open Polly
open Polly.Caching.Memory
open System

let cachePolicy =
    let memoryCacheProvider =
        let memoryCache = new MemoryCache(MemoryCacheOptions())
        MemoryCacheProvider memoryCache

    Policy.Cache(memoryCacheProvider, TimeSpan.FromMinutes 5)

type private CountryCodesWiki = HtmlProvider<"https://en.m.wikipedia.org/wiki/List_of_ISO_3166_country_codes">

let private tryGetCountryIsoCode =
    let lookup =
        readOnlyDict [
            for row in CountryCodesWiki.GetSample().Tables.``Current ISO 3166 country codesEdit``.Rows do
                row.``ISO 3166-1[2] - Alpha-3 code[5]``, row.``ISO 3166-1[2] - Alpha-2 code[5]``
        ]

    fun value -> lookup |> Option.tryGetValue value |> Option.map (fun r -> r.ToLower())

let private ctx = WorldBankData.GetDataContext()
let private allCountries = ctx.Countries |> Seq.toList
let private allRegions = ctx.Regions |> Seq.toList

let private countriesAndRegions =
    let countries = allCountries |> List.map (fun country -> country.Name.Trim())
    let regions = allRegions |> List.map (fun region -> region.Name.Trim())
    countries @ regions

let private containsText (text: string) (v: string) =
    v.Trim().Contains(text.Trim(), StringComparison.CurrentCultureIgnoreCase)

let private matches (text: string) (v: string) =
    v.Trim().Equals(text.Trim(), StringComparison.CurrentCultureIgnoreCase)

let tryCreateReport country =
    let actualQuery (country: WorldBankData.ServiceTypes.Country) = option {
        let! nuclear =
            country.Indicators.``Alternative and nuclear energy (% of total energy use)``.Values
            |> Seq.tryLast

        let! imports =
            country.Indicators.``Energy imports, net (% of energy use)``.Values
            |> Seq.tryLast

        let! renewables =
            country.Indicators.``Renewable energy consumption (% of total final energy consumption)``.Values
            |> Seq.tryLast

        let! fossils =
            country.Indicators.``Fossil fuel energy consumption (% of total)``.Values
            |> Seq.tryLast

        return {
            Country = country.Name
            Code = tryGetCountryIsoCode country.Code
            Nuclear = nuclear
            EnergyImports = imports
            RenewableEnergyConsumption = renewables
            FossilFuelEnergyConsumption = fossils
        }
    }

    cachePolicy.Execute((fun ctx -> actualQuery country), Context $"{country.Code}")

/// Gets the top ten destinations that contain the supplied text.
let findDestinations (text: string option) =
    match text with
    | None -> countriesAndRegions
    | Some text -> countriesAndRegions |> List.filter (containsText text)
    |> List.truncate 10

/// Finds all country-level reports that contain the supplied text.
let findReportsByCountries sortParameters (text: string) =
    allCountries
    |> Seq.filter (fun country -> country.Name |> containsText text)
    |> Seq.choose tryCreateReport
    |> Seq.pickSort sortParameters
    |> Seq.toList

/// Looks for an exact match of a country or region based on the text supplied. Tries a country first; if no match,
/// check for a region - if that matches, all countries within that region are returned.
let tryExactMatchReport sortParameters (text: string) =
    let matchingCountry = allCountries |> List.tryFind (fun c -> c.Name |> matches text)

    match matchingCountry with
    | Some country -> tryCreateReport country |> Option.map List.singleton
    | None ->
        allRegions
        |> List.tryFind (fun region -> region.Name |> matches text)
        |> Option.map (fun region ->
            region.Countries
            |> Seq.choose tryCreateReport
            |> Seq.pickSort sortParameters
            |> Seq.toList)