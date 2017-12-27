type action =
  | NoAction;

type dynamicHeight =
  | FixedHeight(int)
  | DynamicHeight((int) => int);

type state = {
  height: int,
  width: int,
  rowHeight: dynamicHeight,
};

module List = {
  [@bs.module "react-virtualized"] external virtualized_list : ReasonReact.reactClass = "List";

  let make = (~autoHeight=false, ~height: int, ~width: int, ~rowCount: int, ~rowHeight: dynamicHeight, ~rowRenderer, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=virtualized_list,
      ~props={"autoHeight": Js.Boolean.to_js_boolean(autoHeight),
        "height": height, "rowCount": rowCount, "rowHeight": rowHeight,
        "rowRenderer": rowRenderer, "width": width},
      children
    );
};

let component = ReasonReact.reducerComponent("ListView");
let make = (children) => {
  ...component,
  initialState: () => {height: 100, width: 100, rowHeight: FixedHeight(10)},
  reducer: (action, state) => {
    switch action {
      | NoAction => NoUpdate
    }
  },
  render: ({state: {height, width, rowHeight}}) => {
    let rowRenderer = (_) => { <div> (ReasonReact.stringToElement("hhh")) </div> };
    <List height width rowHeight rowCount=1 rowRenderer>
      children
    </List>
  }
};
