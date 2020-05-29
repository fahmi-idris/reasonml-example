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
  dispatch(RepositoriesLoaded([]));
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
    <div className="repo-item-container">
      <a
        href={repo.url}
        target="_blank">
        {React.string(repo.owner == user ? repo.name : repo.full_name)} 
        {repo.fork
          ? <span>{React.string("This is forked repository")}</span>
          : React.null}
      </a>
      <p>
        {repo.description |> renderDescription |> React.string}
      </p>
    </div>
    <div className="repository-stars">
      <span>
        {React.string(string_of_int(repo.stars) ++ " stars")}
      </span>
    </div>
  </li>
);

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState);
  <div className="searching-container">
    <div className="flexbox">
      <div className="search">
        <div className="input-container">
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
            className={state.username !== "" ? "active" : ""}
          />
        </div>
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
      </div>
    </div>
  </div>;
};