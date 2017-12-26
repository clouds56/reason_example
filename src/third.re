[@bs.module "./third.jsx"] external third_class : ReasonReact.reactClass = "Game";

let make = (children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=third_class,
    ~props=Js.Obj.empty(),
    children
  );
