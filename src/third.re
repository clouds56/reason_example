type action =
  | NoAction;

type listItemStyle;
type listItem = {. "index": int, "style": ReactDOMRe.style, "key": string};
type dynamicHeight =
  | FixedHeight(int)
  | DynamicHeight((listItem) => int);

type state = {
  height: int,
  width: int,
  rowHeight: dynamicHeight,
};

module List = {
  [@bs.module "react-virtualized"] external virtualized_list : ReasonReact.reactClass = "List";

  let make = (~autoHeight=false, ~height: int, ~width: int, ~rowCount: int, ~rowHeight: dynamicHeight, ~rowRenderer, children) => {
    let wrap = (h) => ReasonReact.wrapJsForReason(
      ~reactClass=virtualized_list,
      ~props={"autoHeight": Js.Boolean.to_js_boolean(autoHeight),
        "height": height, "rowCount": rowCount, "rowHeight": h,
        "rowRenderer": rowRenderer, "width": width},
      children);
    switch rowHeight {
      | FixedHeight(h) => wrap(h)
      | DynamicHeight(h) => wrap(h)
    };
  }
};

let component = ReasonReact.reducerComponent("ListView");
let make = (children) => {
  ...component,
  /* initialState: () => {height: 100, width: 100, rowHeight: FixedHeight(10)}, */
  initialState: () => {height: 300, width: 1000, rowHeight: DynamicHeight((item) => {
    Js.log(item); 30
  })},
  reducer: (action, state) => {
    switch action {
      | NoAction => NoUpdate
    }
  },
  render: ({state: {height, width, rowHeight}}) => {
    let rowRenderer = (obj: listItem) => {
      let item_index = obj##index;
      <div style=(obj##style) key=(obj##key)> (ReasonReact.stringToElement({j|item: $(item_index)|j})) </div>
    };
    <List height width rowHeight rowCount=2 rowRenderer>
      children
    </List>
  }
};
