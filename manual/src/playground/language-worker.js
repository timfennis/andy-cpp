// Answers the editor's questions about its text (diagnostics, completion,
// hover, definition) off the main thread. Analysing deeply nested code can
// overflow the stack and break the interpreter for good; here that only costs
// this worker, which the page then replaces.
import init, { Editor } from "./pkg/ndc_wasm.js";

const ready = init().then(() => new Editor());
let text = null;
let diagnostics = [];

self.onmessage = async ({ data: { id, op, source, line, character } }) => {
  let editor;
  try {
    editor = await ready;
  } catch (err) {
    self.postMessage({ id, error: `The interpreter failed to load: ${err}`, crashed: true });
    return;
  }
  try {
    if (source !== text) {
      diagnostics = JSON.parse(editor.set_text(source));
      text = source;
    }
    let result;
    switch (op) {
      // Every edit is analysed, as a language server sees every keystroke:
      // completion after `x.`, which doesn't parse, relies on the analysis of
      // the last text that did.
      case "update":
        result = null;
        break;
      case "diagnostics":
        result = diagnostics;
        break;
      case "completion":
        result = JSON.parse(editor.completion(line, character));
        break;
      case "hover":
        result = JSON.parse(editor.hover(line, character));
        break;
      case "definition":
        result = JSON.parse(editor.definition(line, character));
        break;
      default:
        throw new Error(`unknown request ${op}`);
    }
    self.postMessage({ id, result });
  } catch (err) {
    self.postMessage({ id, error: String(err), crashed: true });
  }
};
