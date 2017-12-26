ReactDOMRe.renderToElementWithId(<Page message="Hello!" />, "index");
ReactDOMRe.renderToElementWithId(<Game_2048 />, "g0");
let third_component = Third.make(/* array */[]);
let third_element = ReasonReact.element(third_component);
ReactDOMRe.renderToElementWithId(third_element, "g3rd");
