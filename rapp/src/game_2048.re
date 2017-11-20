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
      let delta = if (last_delta_score<0) { {j|$last_delta_score|j} } else { {j|+$last_delta_score|j} };
      <div> (ReasonReact.stringToElement({j|score: $score ($delta)|j})) </div>
    }
  };
};

let newGame(size) = { board_size: size, score: 0, last_delta_score: 0, step:0, turn:false, board: Array.make_matrix(size, size, None), ended: false };

let moveBoard = (board, size, direction) => {
  let rec filter_adj(ln) = switch(ln) {
    | [Some(a), Some(b), ...tail] when a==b => [Some(a+b), ...filter_adj(tail)]
    | [a, ...tail] => [a, ...filter_adj(tail)]
    | [] => []
  };
  let to_array(n, ln) = {
    let arr = Array.make(n, None);
    List.iteri((i,x) => arr[i]=x, ln);
    arr
  };
  let board = switch direction {
    | Left => {
        board |> Array.map ((ln) => {
          ln |> Array.to_list |> List.filter ((!=)(None))
             |> filter_adj |> to_array(size)
        })
      }
    | Right => {
        board |> Array.map ((ln) => {
          ln |> Array.to_list |> List.rev |> List.filter ((!=)(None))
             |> filter_adj |> to_array(size) |> Array.to_list |> List.rev |> Array.of_list
        })
      }
    | _ => board
  };
  (board, -1)
};

let make = (_children) => {
  ...component,
  initialState: () => newGame(4),
  reducer: (action, state) => {
    let move = (board, size, direction) => {
      let (newBoard, _) as result = moveBoard(board, size, direction);
      if (newBoard == board) {
        None
      } else {
        Some(result)
      }
    };
    let add = (board, x, y, t) => {
      if (board[x][y] == t) {
        None
      } else {
        let board = [...board];
        board[x] = [...board[x]];
        board[x][y] = t;
        Some(board)
      }
    };
    switch action {
      | NewGame => ReasonReact.Update(newGame(4))
      | Move(direction) => {
          switch (move(state.board, state.board_size, direction)) {
            | Some((board, score)) => ReasonReact.Update({...state,
                step: state.step+1,
                board: board,
                score: state.score+score,
                last_delta_score: score,
                turn: false})
            | None => ReasonReact.SideEffects((_) => { /*let d = Printf.sprintf("%a", direction);*/ Js.log({j|info: can not move $direction|j}) })
          }
        }
      | Add(x, y, t) => {
          switch (add(state.board, x, y, t)) {
            | Some(board) => ReasonReact.UpdateWithSideEffects(
                {...state,
                  step: state.step+1,
                  board: board,
                  turn: true},
                (_) => Js.log({j|Add($x,$y): $t|j}));
            | None => NoUpdate
          }
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
      <button onClick=(self.reduce((_) => Move(Left)))> (ReasonReact.stringToElement({js|⬅️|js})) </button>
      <button onClick=(self.reduce((_) => Move(Up)))> (ReasonReact.stringToElement({js|⬆️|js})) </button>
      <button onClick=(self.reduce((_) => Move(Down)))> (ReasonReact.stringToElement({js|⬇️|js})) </button>
      <button onClick=(self.reduce((_) => Move(Right)))> (ReasonReact.stringToElement({js|➡️|js})) </button>
    </div>
  }
};
