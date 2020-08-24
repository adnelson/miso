// Interface to Miso library

[%raw {|require("../../jsbits/util")|}];
[%raw {|require("../../jsbits/diff")|}];
[%raw {|require("../../jsbits/isomorphic")|}];
[%raw {|require("../../jsbits/delegate")|}];

Js.log("hello world!");

// TODO use a library
type node;
type event;

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
        let globalVtreeRef = ref(view(initialModel));
        Vtree.delegate(mountPointElement, events, vtreeCallback =>
          vtreeCallback(globalVtreeRef^)
        );
        Js.log("Started event loop!");
      | _ => Js.Console.error("No body element was found")
      };
      // Vtree.delegate(
      //  let del = delegate(Obj.magic(2), [||]);
      //  Js.log(del(_ => ()));
      // failwith("not implemented");
    };
};
