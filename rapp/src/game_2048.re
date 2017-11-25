type direction =
  | Up
  | Down
  | Left
  | Right;

type action =
  | NewGame
  | Move(direction)
  | Add(int, int, option(int))
  | AddRandom
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

module Matrix = {
  /* let map_ = (raw_map, f, m) => {
    raw_map((m) => raw_map(f, m), m)
  };
  let map = (f, m) => map_(Array.map, f, m); */
  let map = (f, m) => {
    let raw_map = Array.map;
    raw_map((m) => raw_map(f, m), m)
  };
  let where = (f, m) => {
    let raw_fold = BatArray.fold_lefti;
    raw_fold((r, x, l) => raw_fold((r, y, i) => switch (f(i)) {
      | true => [(x,y), ...r]
      | false => r
    }, r, l), [], m)
  };
};

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

module GameBoard = {
  module Cell = {
    let component = ReasonReact.statelessComponent("Game_2048_GameBoard_Cell");
    let make = (~x, ~y, ~p, _children) => {
      ...component,
      render: (_self) => {
        let (class_, text, background) = switch p {
          | Some(t) => {
            ({j|gameboard_cell gameboard_cell_$t|j}, {j|$t|j}, switch t {
              | t when t <= 7 => { let i = Pervasives.float(t)/.8.0; {j|rgba(255,255,0, $i)|j}}
              | t when t <= 15 => { let i = Pervasives.float(t)/.16.0; {j|rgba(0,0,255, $i)|j}}
              | _ => "#202020"
            })
          }
          | None => ({j|gameboard_cell gameboard_cell_empty|j}, "", "#C0C0C0")
        };
        let style = ReactDOMRe.Style.make(~width="100px", ~height="100px", ~background={j|$background|j},
                        ~textAlign="center", ~fontSize="30px", ~fontFamily="sans-serif", ());
        <td style=(style) className=(class_) id=({j|board_cell_$(x)_$(y)|j})>
          (ReasonReact.stringToElement(text))
        </td>
      }
    }
  };

  let component = ReasonReact.statelessComponent("Game_2048_GameBoard");
  let make = (~board_size, ~board, _children) => {
    ...component,
    render: (_self) => {
      let board_elements = board |> Array.mapi((x, ln) =>
        <tr key=({j|board_ln_$x|j})>
          (ln
            |> Array.mapi((y, i) => <Cell x y p=(i) key=({j|board_cell_$(x)_$(y)|j}) />)
            |> ReasonReact.arrayToElement)
        </tr>);
      <table> <tbody> (board_elements |> ReasonReact.arrayToElement) </tbody> </table>
    }
  };
};

let newGame(size) = { board_size: size, score: 0, last_delta_score: 0, step:0, turn:false, board: Array.make_matrix(size, size, None), ended: false };

let moveBoard = (board, size, direction) => {
  let filter_adj_((sc0, h), (sc, tail)) = (sc + sc0, [h, ...tail]);
  let rec filter_adj(ln) = switch(ln) {
    | [Some(a), Some(b), ...tail] when a==b => filter_adj_((a, Some(a+1)), filter_adj(tail)) /*[Some(a+b), ...filter_adj(tail)]*/
    | [a, ...tail] => filter_adj_((0, a), filter_adj(tail)) /*[a, ...filter_adj(tail)]*/
    | [] => (0, [])
  };
  let to_array(n, ln) = {
    let arr = Array.make(n, None);
    List.iteri((i,x) => arr[i]=x, ln);
    arr
  };
  let process = (board, size, readarr, savearr) => {
    let newBoard = Array.make_matrix(size, size, None);
    let score = ref(0);
    for (x in 0 to size-1) {
      let (sc, ln) = readarr(board, x) |> Array.to_list |> List.filter ((!=)(None)) |> filter_adj;
      ln |> to_array(size) |> savearr(newBoard, x);
      score := score^ + sc;
    };
    (score^, newBoard)
  };
  let (score, board) = switch direction {
    | Left => process(board, size,
                      (b, x) => b[x],
                      (b, x, t) => b[x] = t)
    | Right => process(board, size,
                       (b, x) => BatArray.rev(b[x]),
                       (b, x, t) => b[x] = BatArray.rev(t))
    | Up => process(board, size,
                    (b, x) => Array.map((ln) => ln[x], b),
                    (b, x, t) => BatArray.iter2((ln, tt) => ln[x] = tt, b, t))
    | Down => process(board, size,
                      (b, x) => Array.map((ln) => ln[x], b) |> BatArray.rev,
                      (b, x, t) => BatArray.iter2((ln, tt) => ln[x] = tt, b, BatArray.rev(t)))
  };
  (board, score)
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
            | Some((board, score)) => ReasonReact.UpdateWithSideEffects({...state,
                step: state.step+1,
                board: board,
                score: state.score+score,
                last_delta_score: score,
                turn: false},
                (self) => {
                    Js.log("Side effect => AddRandom");self.reduce((_)=>AddRandom, ())})
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
      | AddRandom => {
          let i = Array.of_list(Matrix.where((==)(None), state.board));
          Js.log(Array.length(i));
          let j = Random.int(Array.length(i));
          let (x, y) = i[j];
          ReasonReact.SideEffects((self) => self.reduce((_)=>Add(x, y, Some(1)), ()))
        }
      | GameOver => ReasonReact.Update({...state, ended: true})
    }
  },
  render: ({state: {step, score, last_delta_score, board_size, board}} as self) => {
    let message = {j|step: $step!|j};
    <div>
      <div onClick=(self.reduce((_) => Add(1,1,Some(1))))>
        (ReasonReact.stringToElement(message))
      </div>
      <ScoreBoard score last_delta_score />
      <GameBoard board_size board />
      <button onClick=(self.reduce((_) => Move(Left)))> (ReasonReact.stringToElement({js|⬅️|js})) </button>
      <button onClick=(self.reduce((_) => Move(Up)))> (ReasonReact.stringToElement({js|⬆️|js})) </button>
      <button onClick=(self.reduce((_) => Move(Down)))> (ReasonReact.stringToElement({js|⬇️|js})) </button>
      <button onClick=(self.reduce((_) => Move(Right)))> (ReasonReact.stringToElement({js|➡️|js})) </button>
    </div>
  }
};
