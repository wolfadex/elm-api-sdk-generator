#! /bin/sh

set -e

# Generate various SDKs and test cases.

# Every code generation here overwrites the existing OpenApi.Common module, if
# any, so we must run these in a certain order: First run minimal schemas,
# since they generate OpenApi.Common modules without methods like
# `decodeStringDate` that are needed by other generated modules.
#
# (OK, as long as "github-spec" is the last to run before compiling Example,
# we'll be fine!)
npm run dev ./example/recursive-allof-refs.yaml
npm run dev ./example/single-enum.yaml

npm run dev ./example/patreon.json
npm run dev ./example/realworld-conduit.yaml
npm run dev ./example/amadeus-airline-lookup.json
npm run dev -- ./example/db-fahrplan-api-specification.yaml --auto-convert-swagger
npm run dev ./example/MarioPartyStats.json
npm run dev ./example/viaggiatreno.yaml
npm run dev -- ./example/trustmark.json --module-name Trustmark --effect-types 'cmd' --server 'https://api.sandbox.retrofitintegration.data-hub.org.uk'
npm run dev -- ./example/trustmark-trade-check.json --module-name Trustmark.TradeCheck --effect-types 'cmd'
npm run dev ./example/github-spec.json

# Compile example Elm

cd example && npx elm make src/Example.elm --output=/dev/null
