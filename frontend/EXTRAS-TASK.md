# Extras Task List

## pointy-stdlib

- [x] Preserve template-provided `meta.pointy` in `evalSteps` (merge instead of overwrite)
- [x] Add `csvExtras` stdlib helper (generic CSV/TSV metadata generator)
- [x] Add `fastqExtras` stdlib helper (FASTQ read-count generator)
- [x] Add `mergeExtras` stdlib helper (merge multiple extras generators)
- [x] Commit and push stdlib changes to `pointy-stdlib` `main`

## pointy-welker

- [x] Switch to `dev-backend` branch
- [x] Update `pointy-stdlib` flake input after stdlib push

## Backend

- [x] Switch backend to `dev-backend` branch
- [x] `RunStep.hs`: after main step build, resolve optional extras `outPath` and build it with own requirements
- [x] `RunStep.hs`: register GC root for extras out path
- [x] `RunStep.hs`: cancel extras build in stop flow (before main, to prevent restart)
- [x] `Store.hs`: add `GET /step-files/extras` endpoint handler
- [x] `Main.hs`: wire extras endpoint into API type and server
- [x] `Store.hs`: add `.csv` → `text/csv` and `.tsv` → `text/tab-separated-values` MIME mappings
- [x] `Main.hs`: add CORS policy entry for `step-files/extras`
- [x] Backend verification: `nix build -L --no-link .#backend`

## Frontend

- [x] Add `BrianHicks/elm-csv` 4.0.1 dependency (`nix run .#install-elm-package`, then `nix run .#update-elm`)
- [x] `Model/Core.elm`: add `extras : ApiData (Dict String Json.Value)` field to `DirectoryFolder`
- [x] `Model/Core.elm`: update `extractDirectoryFolderBase` and `updateDirectoryFolderBase` for `extras`
- [x] `Model/Core.elm`: add `ColumnMeta` type `{ columnType : ColumnType, nullable : Bool }`
- [x] `Model/Core.elm`: replace hand-rolled `parseDelimited` with `Csv.Parser.parse` in `delimitedGridFromFile`
- [x] `Model/Core.elm`: update `buildDelimitedGrid` to accept optional `List ColumnMeta` from backend; fall back to inference when absent
- [x] `Model/Core.elm`: add nullable numeric `delimitedColumnConfig` variant (nullable int/float → `stringColumnConfig` with numeric comparator, blanks last)
- [x] `Model/Lenses.elm`: add `folderExtras` lens for `DirectoryFolder.extras`
- [x] `Model/Lenses.elm`: add `extrasAt`, `rootExtrasAt`, and `fileDelimitedGridAt` traversals
- [x] `Api/Api.elm`: add `fetchExtras : Int -> Maybe String -> List String -> Flow s (Result Http.Error (Dict String Json.Value))`
- [x] `Api/Decode.elm`: update `directoryItem` decoder to include `delimitedGrid = Nothing` and folder init to include `extras = NotAsked`
- [x] `Actions.elm`: in `toggleOutputEntry` folder action, also fetch extras for that directory
- [x] `Actions.elm`: in `toggleOutputEntry` file action, pass parent folder extras to `loadFileContent`
- [x] `Actions.elm`: update `loadFileContent` to accept and thread extras into `delimitedGridFromFile`
- [x] Frontend verification: `elm make src/Main.elm --output /dev/null`
