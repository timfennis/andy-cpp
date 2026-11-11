// Andy C++ in the browser: highlighting, running programs in a worker, and a
// code editor. Shared by the playground page and the manual's code blocks.
import init, { highlight as highlightHtml } from "./pkg/ndc_wasm.js";

let loading = null;
let loaded = false;

/** Loads the interpreter once; `highlight` colours code from then on. */
export function load() {
  loading ??= init().then(() => {
    loaded = true;
  });
  return loading;
}

export function escape(text) {
  return text.replace(/[&<>"]/g, (c) => `&#${c.charCodeAt(0)};`);
}

/** `code` as HTML with a `tok-*` class on every piece, or escaped as it is
 * while the interpreter is still loading. */
export function highlight(code) {
  if (!loaded) return escape(code);
  try {
    return highlightHtml(code);
  } catch {
    return escape(code);
  }
}

/** Runs programs one after another in a worker, off the main thread, so a
 * program that never ends can be stopped without freezing the page. */
export class Runner {
  #worker = null;
  #waiting = [];

  /** Resolves to `{ output, value, error, millis }`, or `{ stopped: true }`. */
  run(code) {
    return new Promise((resolve) => {
      this.#start();
      this.#waiting.push(resolve);
      this.#worker.postMessage(code);
    });
  }

  get running() {
    return this.#waiting.length > 0;
  }

  /** Ends the program that is running and drops the ones waiting. */
  stop() {
    this.#finish({ stopped: true });
  }

  #start() {
    if (this.#worker) return;
    this.#worker = new Worker(new URL("worker.js", import.meta.url), { type: "module" });
    this.#worker.onmessage = ({ data }) => {
      if (data.crashed) {
        // The crash broke the worker's interpreter; start a fresh one next time.
        this.#finish(data);
      } else {
        this.#waiting.shift()?.(data);
      }
    };
    this.#worker.onerror = (e) => {
      e.preventDefault();
      this.#finish({ error: `The interpreter failed to start: ${e.message}` });
    };
  }

  #finish(result) {
    this.#worker?.terminate();
    this.#worker = null;
    for (const resolve of this.#waiting.splice(0)) resolve(result);
  }
}

/** What a run printed and produced, as HTML; empty when there was nothing. */
export function resultHtml({ output = "", value = "", error = null, stopped = false }) {
  if (stopped) return `<span class="ndc-muted">Stopped.</span>`;
  let html = escape(output);
  if (value) html += (output && !output.endsWith("\n") ? "\n" : "") + highlight(value) + "\n";
  if (error) html += `<span class="ndc-error">${escape(error)}</span>`;
  return html;
}

/** A link to the playground page that opens with `code` in the editor. */
export function playgroundUrl(code) {
  const bytes = new TextEncoder().encode(code);
  const base64 = btoa(String.fromCharCode(...bytes))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");
  return new URL(`index.html#code=${base64}`, import.meta.url).href;
}

/** The code a `playgroundUrl` link carries, if the page was opened with one. */
export function codeFromHash(hash = location.hash) {
  const base64 = new URLSearchParams(hash.slice(1)).get("code");
  if (!base64) return null;
  try {
    const binary = atob(base64.replace(/-/g, "+").replace(/_/g, "/"));
    return new TextDecoder().decode(Uint8Array.from(binary, (c) => c.charCodeAt(0)));
  } catch {
    return null;
  }
}

/** Turns `container` into a code editor: a transparent textarea over the
 * highlighted code, so the browser's own editing, selection and undo apply.
 * Ctrl/Cmd+Enter calls `onRun`. */
export function createEditor(container, { code = "", onRun = () => {} } = {}) {
  const shown = document.createElement("pre");
  shown.setAttribute("aria-hidden", "true");
  const input = document.createElement("textarea");
  Object.assign(input, { value: code, spellcheck: false, autocapitalize: "off", autocomplete: "off" });
  input.setAttribute("wrap", "off");
  input.setAttribute("aria-label", "Code");
  container.classList.add("ndc-editor");
  container.replaceChildren(shown, input);

  let pending = false;
  function render() {
    pending = false;
    // A trailing newline needs something after it, or the <pre> is a line
    // shorter than the textarea and the two scroll apart.
    shown.innerHTML = highlight(input.value) + (input.value.endsWith("\n") ? " " : "");
    syncScroll();
  }
  function syncScroll() {
    shown.scrollTop = input.scrollTop;
    shown.scrollLeft = input.scrollLeft;
  }
  input.addEventListener("input", () => {
    if (!pending) {
      pending = true;
      requestAnimationFrame(render);
    }
  });
  input.addEventListener("scroll", syncScroll);

  // execCommand("insertText") keeps the browser's undo history intact.
  const insert = (text) => document.execCommand("insertText", false, text);
  input.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
      e.preventDefault();
      onRun();
    } else if (e.key === "Enter" && !e.shiftKey && !e.altKey) {
      e.preventDefault();
      const lineStart = input.value.lastIndexOf("\n", input.selectionStart - 1) + 1;
      insert("\n" + input.value.slice(lineStart).match(/^[ \t]*/)[0]);
    } else if (e.key === "Tab" && !e.shiftKey) {
      e.preventDefault();
      insert("    ");
    }
  });

  render();
  load().then(render);
  return {
    get value() {
      return input.value;
    },
    set value(code) {
      input.value = code;
      render();
    },
    focus: () => input.focus(),
  };
}
