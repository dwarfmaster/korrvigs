import {EditorState} from "@codemirror/state"
import {EditorView, keymap} from "@codemirror/view"
import {defaultKeymap} from "@codemirror/commands"
import {markdown} from "@codemirror/lang-markdown"
import {oneDark} from "@codemirror/theme-one-dark"

let startState = EditorState.create({
  doc: "",
  extensions: [keymap.of(defaultKeymap), markdown().language, oneDark]
});

let view = new EditorView({
  state: startState,
  parent: document.getElementById("editor")
});

$.getJSON(window.location.href, function (text) {
  view.dispatch({
    changes: {from: 0, insert: text}
  });
});

const button = document.querySelector('#editor-save>button');
button.addEventListener('click', function () {
  let text = view.state.doc.toString();
  fetch(window.location.href, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded'
    },
    body: 'content='+encodeURIComponent(text)
  })
});
