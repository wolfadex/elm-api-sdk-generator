#! /bin/sh

set -e

# Generate various SDKs

npm run dev ./example/patreon.json
npm run dev ./example/int64.json
npm run dev ./example/realworld-conduit.yaml
npm run dev ./example/amadeus-airline-lookup.json
npm run dev -- ./example/db-fahrplan-api-specification.yaml --auto-convert-swagger
npm run dev ./example/single-enum.yaml
npm run dev ./example/MarioPartyStats.json
npm run dev ./example/viaggiatreno.yaml
npm run dev -- ./example/trustmark.json --module-name Trustmark --effect-types 'cmd' --server 'https://api.sandbox.retrofitintegration.data-hub.org.uk'
npm run dev -- ./example/trustmark-trade-check.json --module-name Trustmark.TradeCheck --effect-types 'cmd'
npm run dev ./example/github-spec.json

# Compile example Elm

cd example && npx elm make src/Example.elm --output=/dev/null
