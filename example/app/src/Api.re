open Fetcheroo;
/*
 Good Response looks like:
 {"Title":"The Postman","Year":"1997","Rated":"R","Released":"25 Dec 1997","Runtime":"177 min","Genre":"Action, Adventure, Drama, Sci-Fi","Director":"Kevin Costner","Writer":"David Brin (novel), Eric Roth (screenplay), Brian Helgeland (screenplay)","Actors":"Kevin Costner, Will Patton, Larenz Tate, Olivia Williams","Plot":"A nameless drifter dons a postman's uniform and bag of mail as he begins a quest to inspire hope to the survivors living in post-apocalyptic America.","Language":"English","Country":"USA","Awards":"7 wins & 7 nominations.","Poster":"https://m.media-amazon.com/images/M/MV5BNTAyYTFmYmUtNTFhZS00NGQxLWI0OGEtZDM2NDk1OGE4YzFmL2ltYWdlXkEyXkFqcGdeQXVyMTQxNzMzNDI@._V1_SX300.jpg","Ratings":[{"Source":"Internet Movie Database","Value":"6.0/10"},{"Source":"Rotten Tomatoes","Value":"9%"},{"Source":"Metacritic","Value":"29/100"}],"Metascore":"29","imdbRating":"6.0","imdbVotes":"63,790","imdbID":"tt0119925","Type":"movie","DVD":"23 Jun 1998","BoxOffice":"N/A","Production":"Warner Home Video","Website":"N/A","Response":"True"}


 Error response looks like:
 {"Response":"False","Error":"Movie not found!"}
 */
module Resources = {
  module Error = {
    type t = {error: string};
    let fromJson = raw => {
      Json.Decode.{error: raw |> field("Error", string)};
    };
  };

  module Movie = {
    type t = {title: string};
    let fromJson = raw => {
      Json.Decode.{title: raw |> field("Title", string)};
    };
  };

  type movieRes =
    | Ok(Movie.t)
    | Error(Error.t);

  module Entity = {
    type t = {foo: int};
    let fromJson = raw => {
      Json.Decode.{foo: raw |> field("title", int)};
    };
  };
};
/*
   Using partial application to create reusable functions.
   I.e set up common stuff, request with auth + content headers,
   Some base url, etc..
 */
let authenticatedFetcher = () =>
  Fetcher.make(
    ~baseUrl=Some("http://www.omdbapi.com/?apikey=" ++ Creds.apiKey),
    ~defaultHeaders=
      () =>
        // You may get an auth key from local storage or something like that.
        Fetch.HeadersInit.makeWithArray(Array.of_list([])),
    ~errorDeserialiser=
      Fetcher.Deserialiser.Json(Resources.Error.fromJson),
  );

/*
   Reuse the base fetcher
 */
let movieFetcher =
  authenticatedFetcher(
    (),
    ~deserialiser=
      Fetcher.Deserialiser.Json(
        js => {
          /*
             Annoyingly this API sometimes gives an Error response when the status is 200. So
             I must do work here to deserialise errors. Typically we would leave the
             error deserialisation to the function passed to errorDeserialiser and 
             just pass a *.fromJson function here :(
           */
          let isError =
            Js_json.decodeObject(js)
            ->Rationale.Option.map(
                o =>
                  o
                  |> Js_dict.keys
                  |> Array.to_list
                  |> List.exists(a => a == "Error"),
                _,
              )
            ->Rationale.Option.default(false, _);
          if (!isError) {
            Resources.Ok(Resources.Movie.fromJson(js));
          } else {
            Resources.Error(Resources.Error.fromJson(js));
          };
        },
      ),
  );

let searchByName = (name: string, subscribe) =>
  movieFetcher(~url="&t=" ++ name, ~subscribe, ());

let searchById = (id: string, subscribe) =>
  movieFetcher(~url="&i=" ++ id, ~subscribe, ());

let fakeEndpoint:
  (string, Fetcher.t(Resources.Entity.t, Resources.Error.t) => unit) =>
  unit =
  (name: string, subscribe) =>
    authenticatedFetcher(
      (),
      ~url="/" ++ name,
      ~deserialiser=Fetcher.Deserialiser.Json(Resources.Entity.fromJson),
      ~subscribe,
      (),
    );