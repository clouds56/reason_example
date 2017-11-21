type action =
  | A
  | B;

type state = {
  text: string
};

let component = ReasonReact.reducerComponent("Page");

let make = (~message, _children) => {
  ...component,
  initialState: () => {text: message},
  reducer: (action, state) => switch action {
    | A => {
        Js.log("actionA");
        ReasonReact.UpdateWithSideEffects({text: "hello, world"}, ({reduce}) => {
          Js.log("actionA => B");
          reduce((_)=>B, ())
        })
      }
    | B => {
        Js.log("actionB");
        ReasonReact.Update({text: state.text ++ "!"})
      }
  },
  render: (self) =>
    <div onClick=(self.reduce((_) => A))> (ReasonReact.stringToElement(self.state.text)) </div>
};
