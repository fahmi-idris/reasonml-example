type remoteData('res) = RemoteData.t('res, 'res, string);

type repository = {
  id: int,
  owner: string,
  name: string,
  full_name: string,
  stars: int,
  url: string,
  description: option(string),
  fork: bool,
};

type action =
  | ChangeUsername(string)
  | Loading
  | RepositoriesLoaded(list(repository))
  | RepositoriesError(string);

type state = {
  username: string,
  user: string,
  repositories: remoteData(list(repository)),
};

let forkPath = "M8 1a1.993 1.993 0 0 0-1 3.72V6L5 8 3 6V4.72A1.993 1.993 0 0 0 2 1a1.993 1.993 0 0 0-1 3.72V6.5l3 3v1.78A1.993 1.993 0 0 0 5 15a1.993 1.993 0 0 0 1-3.72V9.5l3-3V4.72A1.993 1.993 0 0 0 8 1zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3 10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3-10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z";
let svgPath = "M15.2 40.6c-.2 0-.4-.1-.6-.2-.4-.3-.5-.7-.4-1.1l3.9-12-10.2-7.5c-.4-.3-.5-.7-.4-1.1s.5-.7 1-.7h12.7L25 5.9c.1-.4.5-.7 1-.7s.8.3 1 .7L30.9 18h12.7c.4 0 .8.2 1 .6s0 .9-.4 1.1L34 27.1l3.9 12c.1.4 0 .9-.4 1.1s-.8.3-1.2 0L26 33l-10.2 7.4c-.2.1-.4.2-.6.2zM26 30.7c.2 0 .4.1.6.2l8.3 6.1-3.2-9.8c-.1-.4 0-.9.4-1.1l8.3-6.1H30.1c-.4 0-.8-.3-1-.7L26 9.5l-3.2 9.8c-.1.4-.5.7-1 .7H11.5l8.3 6.1c.4.3.5.7.4 1.1L17.1 37l8.3-6.1c.2-.1.4-.2.6-.2z";

let initialState = { username: "", user: "", repositories: RemoteData.NotAsked };
let reducer = (state, action) =>
  switch (action) {
    | ChangeUsername(username) => { ...state, username }
    | Loading =>
      let existingData =
        switch (state.repositories) {
        | NotAsked
        | Loading(_)
        | Failure(_) => []
        | Success(response) => response
        };
      {
        ...state,
        user: state.username,
        repositories: RemoteData.Loading(existingData),
      };
    | RepositoriesLoaded(response) => { ...state, repositories: RemoteData.Success(response) }
    | RepositoriesError(error) => { ...state, repositories: RemoteData.Failure(error) }
  };

let fetchRepository = (state, dispatch) => {
  Js.Promise.(
    Fetch.fetch(
      "https://api.github.com/users/"
      ++ state.username
      ++ "/repos?type=all&sort=updated",
    )
    |> then_(Fetch.Response.json)
    |> then_(json =>
          json
          |> Json.Decode.array(json =>
              Json.Decode.{
                id: json |> field("id", int),
                owner:
                  json
                  |> field("owner", owner => owner |> field("login", string)),
                name: json |> field("name", string),
                full_name: json |> field("full_name", string),
                stars: json |> field("stargazers_count", int),
                url: json |> field("html_url", string),
                description: json |> optional(field("description", string)),
                fork: json |> field("fork", bool),
              }
            )
          |> Array.to_list
          |> (
            repos => {
              dispatch(RepositoriesLoaded(repos));
              resolve();
            }
          )
        )
    |> ignore
  );
  dispatch(Loading);
};

let renderDescription = desc =>
  switch (desc) {
    | Some(str) => str
    | None => "No description, website, or topics provided."
  };

let repositoryItems = (user, repos) => repos |> List.map(repo =>
  <li key={string_of_int(repo.id)}>
    <div className="px-6 py-4">
      <div className="repo-item-container">
        <a
          href={repo.url}
          target="_blank">
          {React.string(repo.owner == user ? repo.name : repo.full_name)}
        </a>
        {repo.fork
          ? <div className="fork-container">
              <svg viewBox="0 0 10 16">
                <path fillRule="evenodd" d=forkPath />
              </svg>
            </div>
          : React.null}
      </div>
      <p className="font-mono text-pink-lighter text-xs">
        {repo.description |> renderDescription |> React.string}
      </p>
    </div>
    <div className="repository-stars">
      <span>
        {React.string(string_of_int(repo.stars))}
      </span>
      <svg viewBox="0 0 50 50">
        <path d=svgPath />
      </svg>
    </div>
  </li>
);

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);
  <div
    className="searching-container">
    <input
      placeholder="Enter a Github Username"
      value={state.username}
      onKeyDown={event =>
        if (ReactEvent.Keyboard.keyCode(event) === 13) {
          ReactEvent.Keyboard.preventDefault(event);
          fetchRepository(state, dispatch);
        }
      }
      onChange={event =>
        dispatch(ChangeUsername(ReactEvent.Form.target(event)##value))
      }
    />

    {switch (state.repositories) {
     | NotAsked => React.null
     | Failure(error) => <p> {React.string(error)} </p>
     | Loading(response)
     | Success(response) =>
       let isLoading = RemoteData.isLoading(state.repositories);
       <>
         {if (isLoading) {
            <div className="loading">
              {React.string("Loading...")}
            </div>;
          } else {
            React.null;
          }}
         {if (List.length(response) > 0) {
            <div className="list-item-container">
              <ul>
                {React.array(response |> repositoryItems(state.user) |> Array.of_list)}
              </ul>
            </div>;
          } else if (!isLoading) {
            <div className="no-user-found">
              {React.string(
                 state.user ++ " " ++ "doesn't have any public repository",
               )}
            </div>;
          } else {
            React.null;
          }}
       </>;
     }}
  </div>;
};