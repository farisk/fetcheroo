open Fetcheroo;

type searchMode =
  | ByName
  | ById;
type movieFetcher =
  Fetcher.t(Api.Resources.movieRes, Api.Resources.Error.t);
type state = {
  searchText: string,
  searchMode,
  searchResult: option(movieFetcher),
};

let searchModeToFetcher =
  fun
  | ByName => Api.searchByName
  | ById => Api.searchById;

type actions =
  | UpdateSearchText(string)
  | UpdateMovieFetchStatus(movieFetcher, string)
  | RequestMovie;

let valFromEvent = e => ReactEvent.Form.target(e)##value;

let debounceSearch = Debouncer.make(~wait=500, dispatcher => dispatcher());

[@react.component]
let make = () => {
  let (state, dispatch) =
    ReactUpdate.useReducer(
      {searchText: "", searchResult: None, searchMode: ByName},
      (action, state) =>
      switch (action) {
      | UpdateSearchText(searchText) =>
        ReactUpdate.UpdateWithSideEffects(
          {...state, searchText},
          self => {
            debounceSearch(() => self.send(RequestMovie));
            None;
          },
        )
      | RequestMovie =>
        ReactUpdate.SideEffects(
          self => {
            let searchText = state.searchText;
            searchModeToFetcher(
              state.searchMode,
              searchText,
              fetchResult => {
                self.send(UpdateMovieFetchStatus(fetchResult, searchText));
                ();
              },
            );
            None;
          },
        )
      | UpdateMovieFetchStatus(fetcher, forSearch) =>
        if (forSearch == state.searchText) {
          ReactUpdate.Update({...state, searchResult: Some(fetcher)});
        } else {
          /* Cancel old request results that never made it through. */
          ReactUpdate.NoUpdate;
        }
      }
    );

  <div>
    <h1> {ReasonReact.string("Movie Searcher")} </h1>
    <input
      value={state.searchText}
      onChange={e => e->valFromEvent->UpdateSearchText->dispatch}
    />
    {state.searchResult
     ->Rationale.Option.map(
         fetcher =>
           switch (fetcher) {
           | Fetcher.Pending => ReasonReact.string("Loading...")
           | Fetcher.Fulfilled(
               Api.Resources.Ok({Api.Resources.Movie.title}),
             ) =>
             ReasonReact.string("Found " ++ title)
           | Fetcher.Fulfilled(
               Api.Resources.Error({Api.Resources.Error.error}),
             ) =>
             ReasonReact.string("Error" ++ error)
           | Fetcher.Error(
               Fetcher.DeserialisedError({Api.Resources.Error.error}),
             ) =>
             ReasonReact.string("Error:" ++ error)
           | Fetcher.Error(e) =>
             // TODO: check for bs-fetch and deserialisation error here, e.g: Fetch Failed
             Js_console.error(e);
             ReasonReact.string(
               "Unexpected error, are you connected to the internet?",
             );
           },
         _,
       )
     ->Rationale.Option.default(ReasonReact.null, _)}
  </div>;
};