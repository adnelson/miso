module VtreeJs = {
  type t('a) = {
    [@bs.as "type"] type_: [ | `vtext | `vnode ], text: option(string), ns: [ | `svg | `mathml | `html ], domRef: option(Webapi.Dom.Element.t), onDestroyed: option(unit => unit), onBeforeDestroyed: option(unit => unit), onCreated: option(unit => unit)
  };
  [@bs.val]
  external delegate:
    (Webapi.Dom.Element.t, array(Miso.event), (t('a) => unit) => unit) =>
    unit =
    "delegate";

  let make = (~type_, ~text=?, ~ns=`html, ~domRef=?, ~onDestroyed=?, ~onBeforeDestroyed=?, ~onCreated=?, ()) => {
    type_, text, ns, domRef, onDestroyed, onBeforeDestroyed, onCreated
  };

  let makeText = (~ns=?, text) => make(~type_=`vtext, ~text, ~ns?, ());
};

module App = Miso.App(VtreeJs);

App.startApp(
  ~initialModel=0,
  ~initialAction=None,
  ~update=
    (action, model) =>
      switch (action) {
      | Some(`Inc) => model + 1
      | Some(`Dec) => model - 1
      | None => model
      },
  ~view= n => VtreeJs.makeText(n->string_of_int),
  (),
);
