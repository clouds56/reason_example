type direction =
  | Up
  | Down
  | Left
  | Right;

type action =
  | NewGame
  | Move(direction)
  | Add(int, int, option(int))
  | GameOver;

type state = {
  board_size: int,
  score: int,
  last_delta_score: int,
  step: int,
  turn: bool,
  board: array(array(option(int))),
  ended: bool,
};

let component = ReasonReact.reducerComponent("Game_2048");

module ScoreBoard = {
  let component = ReasonReact.statelessComponent("Game_2048_ScoreBoard");
  let make = (~score, ~last_delta_score, _children) => {
    ...component,
    render: (_self) => {
      <div> (ReasonReact.stringToElement({j|score: $score (+$last_delta_score)|j})) </div>
    }
  };
};

let newGame(size) = { board_size: size, score: 0, last_delta_score: 0, step:0, turn:false, board: Array.make_matrix(size, size, None), ended: false };

let make = (_children) => {
  ...component,
  initialState: () => newGame(4),
  reducer: (action, state) => {
    let move = (_direction) =>
      ReasonReact.Update({...state, step: state.step+1, turn: false});
    switch action {
      | NewGame => ReasonReact.Update(newGame(4))
      | Move(direction) => move(direction)
      | Add(x, y, t) => {
          Js.log({j|Add($x,$y): $t|j});
          let board = [...state.board];
          board[x] = [...state.board[x]];
          board[x][y] = t;
          /* Js.log(state.board); */
          ReasonReact.Update({...state, step: state.step+1, turn: true, board: board, last_delta_score: 0 });
        }
      | GameOver => ReasonReact.Update({...state, ended: true})
    }
  },
  render: ({state: {step, score, last_delta_score, board}} as self) => {
    let message = {j|step: $step!|j};
    let board_elements = board |> Array.mapi((x, ln) =>
      <div key=({j|board_ln_$x|j})>
        (ln
          |> Array.mapi((y, i) => switch i {
              | Some(t) => <span key=({j|board_cell_$(x)_$(y)|j})> (ReasonReact.stringToElement({j|$t|j})) </span>
              | None => <span key=({j|board_cell_$(x)_$(y)|j})> (ReasonReact.stringToElement({j|x|j})) </span>
            })
          |> ReasonReact.arrayToElement)
      </div>);
    <div>
      <div onClick=(self.reduce((_) => Add(1,1,Some(2))))> (ReasonReact.stringToElement(message)) </div>
      <ScoreBoard score last_delta_score />
      (ReasonReact.arrayToElement(board_elements))
    </div>
  }
};
