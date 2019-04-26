# Example Fetcher App
Allows you to search through films by ombda REST api.

## Run Project

```sh
yarn install
yarn start
# in another tab
yarn run server 
```
Update Creds.re with  a valid http://www.omdbapi.com/ api key ( they are free )

THen go to localhost:8000

## Parts to check out
First have a look at Api.re to see how the fetchers are set up.
Then look at App.re to see how it is used in ReasonReact
