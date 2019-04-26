type rejectionReason('b) =
  | DeserialisedError('b)
  | Other(Js.Promise.error); // When we can't derserialise, e.g: Deserialiser throws exception or Not found string comes back, Offline, etc..

type t('a, 'b) =
  | Pending
  | Fulfilled('a)
  | Error(rejectionReason('b));

module Deserialiser = {
  type t('error) =
    | Raw(Fetch.Response.t => 'error)
    | Json(Js_json.t => 'error)
    | Text(string => 'error);
};

module Body = {
  type t =
    | Json(Js_json.t)
    | Text(string)
    | Raw(Fetch.BodyInit.t);
};
module Private = {
  let makeFetch = (url, ~method_=Fetch.Get, ~body=None, ~defaultHeaders, ()) => {
    let baseRequestInit =
      Fetch.RequestInit.make(~method_, ~headers=defaultHeaders());
    let withBody =
      baseRequestInit
      |> (
        requestSoFar => {
          Rationale.Option.map(
            b =>
            switch (b) {
              | Body.Json(json) => 
                requestSoFar(~body=Fetch.BodyInit.make(Js.Json.stringify(json)))
              | Body.Text(str) => 
                requestSoFar(~body=Fetch.BodyInit.make(str))
              | Body.Raw(body) => requestSoFar(~body)
            },
            body,
          );
        }
      );

    let fetch = Fetch.fetchWithInit(url);

    switch (withBody) {
    | Some(initWithBody) => fetch(initWithBody())
    | None => fetch(baseRequestInit())
    };
  };

  let makeUrl = (baseUrl, url) =>
    Rationale.Option.default("", baseUrl) ++ url;
};
open Private;

type makeFetcher('success, 'error) =
  (
    ~url: string,
    ~deserialiser: Deserialiser.t('success),
    ~errorDeserialiser: Deserialiser.t('error),
    ~method_: Fetch.requestMethod=?,
    ~subscribe: t('success, 'error) => unit,
    ~body: option(Body.t)=?,
    ~baseUrl: option(string)=?,
    ~defaultHeaders: unit => Fetch.HeadersInit.t,
    unit
  ) =>
  unit;
let makeFetcher: makeFetcher('a, 'b) =
  (
    ~url,
    ~deserialiser,
    ~errorDeserialiser,
    ~method_=Fetch.Get,
    ~subscribe,
    ~body=None,
    ~baseUrl=None,
    ~defaultHeaders,
    (),
  ) => {
    subscribe(Pending);
    ignore(Js.Promise.(
      makeFetch(makeUrl(baseUrl, url), ~method_, ~body, ~defaultHeaders, ())
      |> then_(res =>
           if (Fetch.Response.ok(res)) {
             switch (deserialiser) {
             | Json(f) =>
               Fetch.Response.json(res)
               |> then_(js => Fulfilled(f(js)) |> Js_promise.resolve)
             | Text(f) =>
               Fetch.Response.text(res)
               |> then_(t => Fulfilled(f(t)) |> Js_promise.resolve)
             | Raw(f) => Fulfilled(f(res)) |> Js_promise.resolve
             };
           } else {
             switch (errorDeserialiser) {
             | Json(f) =>
               Fetch.Response.json(res)
               |> then_(js =>
                    Error(DeserialisedError(f(js))) |> Js_promise.resolve
                  )
             | Text(f) =>
               Fetch.Response.text(res)
               |> then_(t =>
                    Error(DeserialisedError(f(t))) |> Js_promise.resolve
                  )
             | Raw(f) =>
               Error(DeserialisedError(f(res))) |> Js_promise.resolve
             };
           }
         )
      |> catch(err => Js.Promise.resolve(Error(Other(err))))
      |> then_(res => {
           subscribe(res);
           Js.Promise.resolve(res);
         })
    ))
  };

let isPending =
  fun
  | Pending => true
  | _ => false;