module VtreeJs = {
  type t('a) = {
    [@bs.as "type"]
    type_: [ | `vtext | `vnode],
    ns: [ | `svg | `mathml | `html],
    text: option(string),
    children: array(t('a)),
    domRef: option(Webapi.Dom.Element.t),
    onDestroyed: option(unit => unit),
    onBeforeDestroyed: option(unit => unit),
    onCreated: option(unit => unit),
  };
  [@bs.val]
  external delegate:
    (Webapi.Dom.Element.t, array(Miso.event), (t('a) => unit) => unit) =>
    unit =
    "delegate";

  let make =
      (
        ~type_,
        ~ns=`html,
        ~text=?,
        ~children=[||],
        ~domRef=?,
        ~onDestroyed=?,
        ~onBeforeDestroyed=?,
        ~onCreated=?,
        (),
      ) => {
    type_,
    ns,
    text,
    children,
    domRef,
    onDestroyed,
    onBeforeDestroyed,
    onCreated,
  };

  let makeText = (~ns=?, text) => make(~type_=`vtext, ~text, ~ns?, ());
};

module App = Miso.App(VtreeJs);

App.startApp(
  ~initialModel=0,
  ~initialAction=None,
  ~events=[|("onClick", Js.Nullable.undefined)|],
  ~update=
    (action, model) => {
      Js.log({"action": action, "model": model});
      switch (action) {
      | Some(`Inc) => model + 1
      | Some(`Dec) => model - 1
      | None => model
      };
    },
  ~view=n => VtreeJs.makeText(n->string_of_int),
  (),
);
