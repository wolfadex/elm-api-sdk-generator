# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Fixed

-   [Missing fields with recursive `allOf.$ref` inheritance](https://github.com/wolfadex/elm-open-api-cli/pull/182). [Adam DiCarlo](https://github.com/adamdicarlo0)

### Added

-   Support for params being passed in the header
-   Support for `type : null`
-   Support for `const : <value>`
-   [Support for `additionalProperties` in objects](https://github.com/wolfadex/elm-open-api-cli/pull/184). [Adam DiCarlo](https://github.com/adamdicarlo0)

## [0.7.0] - 2024-09-23

### Added

-   Documentation for generated types. [Leonardo Taglialegne](https://github.com/miniBill)
-   A new record type for generated endpoints. [Leonardo Taglialegne](https://github.com/miniBill)
    -   This allows for incorporating with your own custom HTTP approach.
-   Support for multiple servers as a CLI argument. [Leonardo Taglialegne](https://github.com/miniBill)
-   Support for overriding portions of an OAS with the `--overrides` CLI argument. [Leonardo Taglialegne](https://github.com/miniBill)
    -   Support for writing the overridden OAS to a file. [Leonardo Taglialegne](https://github.com/miniBill)
-   Support for `date` and `date-time` types. [Leonardo Taglialegne](https://github.com/miniBill)
    -   If your OAS has these types, you'll need these additional dependencies:
        -   [wolfadex/elm-rfc3339](https://package.elm-lang.org/packages/wolfadex/elm-rfc3339/latest)
            -   This is the core new requirement as JSON Schema `date` and `date-time` are in [RFC-3339](https://json-schema.org/understanding-json-schema/reference/string#dates-and-times) format
        -   [justinmimbs/time-extra](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest)
        -   [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/latest)
        -   [elm/time](https://package.elm-lang.org/packages/elm/time/latest)
        -   [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest)

### Changed

-   General documentation cleanup. [Leonardo Taglialegne](https://github.com/miniBill)
-   Improved handling of bytes. [Leonardo Taglialegne](https://github.com/miniBill)
-   When there's a single error, don't define a custom type for it. [Leonardo Taglialegne](https://github.com/miniBill)
-   Enums are now custom types not `String`s. [Leonardo Taglialegne](https://github.com/miniBill)
-   Improved errors for Swagger files. [Leonardo Taglialegne](https://github.com/miniBill)

## [0.6.1] - 2024-07-23

### Fixed

-   Decoding a `String` as a `String` and not as JSON. ✨ [Leonardo Taglialegne](https://github.com/miniBill)

## [0.6.0] - 2024-07-16

### Added

-   Support for multiple servers. ✨ [Leonardo Taglialegne](https://github.com/miniBill)
-   Support for query based API keys. ✨ [Leonardo Taglialegne](https://github.com/miniBill)
-   Support for generating `lamdera/program-test` `Effect.Http` requests. ✨ [Leonardo Taglialegne](https://github.com/miniBill)
-   Support for enums.

### Changed

-   How the various effect types are generated. ✨ [Leonardo Taglialegne](https://github.com/miniBill)
-   Now generates multiple `Api.*` modules when generating effects for multiple packages. ✨ [Leonardo Taglialegne](https://github.com/miniBill)
    -   This is to prevent with name collisions as well as horrible naming.

### Fixed

-   Fix for URL builder generation. ✨ [Leonardo Taglialegne](https://github.com/miniBill)
-   Fixed the CLI help doc for effect types, a couple values were reversed.

## [0.5.0] - 2024-06-02

### Added

-   Support for API Keys in auth
-   The new option `--effect-types` for determining which http effect types are generated
    -   Support for generating elm-pages [BackendTask.Http](https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/BackendTask-Http) requests
-   The new option `--server` for being able to pass in a server URL to be used in the generated requests

### Changed

-   Now generates 4 files instead of 2:
    -   `<namespace>/Api.elm`, `<namespace>/Json.elm`, and `<namespace>/Types.elm` which correspond to the OAS directly
    -   `OpenApi/Common.elm` which can be shared across all generated SDKs

## [0.4.1] - 2024-04-28

### Added

-   Support for converting Swagger docs to Open API files, resolves [issue #47](https://github.com/wolfadex/elm-open-api-cli/issues/47)
-   Print status of various steps of the process
    ```sh
    ✓ Read OAS from ./example/realworld-conduit.yaml
    ✓ Parse OAS
    ✓ Generate Elm modules
    ✓ Format with elm-format
    ✓ Write to disk
    ```

### Fixed

-   [Issue #86](https://github.com/wolfadex/elm-open-api-cli/issues/86), some response names start with the status code

## [0.4.0] - 2024-04-23

### Added

-   Included `elm/url` in the possible dependencies list. ✨ [James Robb](https://github.com/jamesrweb)

### Changed

-   Now generates 2 files: `<namespace>/Api.elm` and `<namespace>/OpenApi.elm`, which resolves [issue #81](https://github.com/wolfadex/elm-open-api-cli/issues/81)

## [0.3.0] - 2024-04-21

### Added

-   The risky version of requests, to resolve [issue #71](https://github.com/wolfadex/elm-open-api-cli/issues/71)

### Changed

-   Improved the success message to use links to the generated files &necessary Elm dependencies, and list elm/bytes as a possible dependency

### Fixed

-   Some resopnse names could be status codes, they now generate as `statusCode<number>`
-   [Issue #72](https://github.com/wolfadex/elm-open-api-cli/issues/72), where URLs would end with a trailing `/` when they shouldn't
-   Names (like this in the BIMcloud API) can start with a `$` prefix, make them Elm compliant
-   [Issue #65](https://github.com/wolfadex/elm-open-api-cli/issues/65), not all paths define error responses

## [0.2.3] - 2024-04-19

### Changed

-   Improved accesing JSON content to included vendored JSON. ✨ [James Robb](https://github.com/jamesrweb)

## [0.2.2] - 2024-04-14

### Added

-   Custom error handling: now generates a custom error type for each endpoint that has a non-2xx responses.
-   Post generation notes: after generating the SDK, a message will be displayed with additional dependencies to install. ✨ [James Robb](https://github.com/jamesrweb)

### Changed

-   Warnings are now grouped instead of duplicated ✨ [Leonardo Taglialegne](https://github.com/miniBill)

### Removed

-   [elm-community/json-extra](https://package.elm-lang.org/packages/elm-community/json-extra/latest/) is no loonger required as a dependency

## [0.2.1] - 2024-04-14

-   A duplicate release of 0.2.0

## [0.2.0] - 2024-01-16

### Added

-   Custom error handling:
    -   Now generates a custom error type for each endpoint that has a non-2xx responses.

## [0.1.3-beta.1] - 2023-11-17

## [0.1.2] - 2023-11-13

### Fixed

-   I published the wrong build for 0.1.1, this published the correct build. See 0.1.1 for changes.

## [0.1.1] - 2023-11-01

### Fixed

-   [incorrect parsing of JSON files](https://github.com/wolfadex/elm-open-api-cli/issues/53)

    -   Resolved by https://github.com/wolfadex/elm-open-api-cli/pull/54/ ✨ [Lars Wikman](https://github.com/lawik)

## [0.1.0] - 2023-10-06

### Initial release

_With a large amount of help from [Leonardo Taglialegne](https://github.com/miniBill)_
