// Interface to Miso library

[%raw {|require("../../jsbits/util")|}];
[%raw {|require("../../jsbits/diff")|}];
[%raw {|require("../../jsbits/isomorphic")|}];
[%raw {|require("../../jsbits/delegate")|}];

Js.log("hello world!");

// TODO use a library
type node;
type event = (string, Js.Nullable.t(bool));

[@bs.val] external document: Webapi.Dom.Document.t = "document";

module type Vtree = {
  type t('action);

  let delegate:
    (Webapi.Dom.Element.t, array(event), (t('a) => unit) => unit) => unit;
};

module App = (Vtree: Vtree) => {
  let startApp:
    'model 'action.
    (
      ~initialModel: 'model,
      ~initialAction: 'action,
      ~update: ('action, 'model) => 'model,
      ~view: 'model => Vtree.t('action),
      ~events: array(event)=?,
      unit
    ) =>
    unit
   =
    (~initialModel, ~initialAction, ~update, ~view, ~events=[||], ()) => {
      switch (
        Webapi.Dom.(
          document
          |> Document.getElementsByTagName("body")
          |> HtmlCollection.toArray
        )
      ) {
      | [|(body: Webapi.Dom.Element.t)|] =>
        let mountPointElement = body;
        let globalVtreeRef = ref(initialModel->view);
        Vtree.delegate(
          mountPointElement,
          events,
          vtreeCallback => {
            Js.log("vtreeCallback");
            vtreeCallback(globalVtreeRef^);
          },
        );
        Js.log2("Started event loop!!!", Vtree.delegate);
      | _ => Js.Console.error("No body element was found")
      };
    };
};
