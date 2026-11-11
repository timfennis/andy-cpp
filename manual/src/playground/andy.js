// Andy C++ in the browser: highlighting, running programs in a worker, and a
// code editor. Shared by the playground page and the manual's code blocks.
import init, {
  highlight as highlightHtml,
  highlight_classes as highlightClasses,
  highlight_spans as highlightSpans,
} from "./pkg/ndc_wasm.js";

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

/** What an editor needs from the language: highlighting on the main thread,
 * and everything that needs a full analysis (diagnostics, completion, hover,
 * definition) from a worker. Positions are LSP positions, a line and a UTF-16
 * column, and answers are the language server protocol's types. Call after
 * `load()` has resolved. */
export function language() {
  const service = new LanguageService();
  return {
    /** The classes `spans` numbers its pieces by, without the `tok-` prefix. */
    classes: highlightClasses(),
    /** `[start, end, kind]` per highlighted piece, in UTF-16 offsets. */
    spans(code) {
      try {
        return highlightSpans(code);
      } catch {
        return new Uint32Array();
      }
    },
    highlight,
    /** Tells the worker about an edit, so it knows the last text that parsed. */
    update: (source) => service.request("update", source),
    diagnostics: (source) => service.request("diagnostics", source),
    completion: (source, line, character) => service.request("completion", source, line, character),
    hover: (source, line, character) => service.request("hover", source, line, character),
    definition: (source, line, character) => service.request("definition", source, line, character),
  };
}

/** Asks the language worker about a text. A request that crashes the worker,
 * for example by overflowing its stack, answers `null` and the next request
 * gets a fresh worker. */
class LanguageService {
  #worker = null;
  #pending = new Map();
  #next = 0;

  request(op, source, line, character) {
    return new Promise((resolve) => {
      const id = this.#next++;
      this.#pending.set(id, resolve);
      this.#start().postMessage({ id, op, source, line, character });
    });
  }

  #start() {
    if (!this.#worker) {
      this.#worker = new Worker(new URL("language-worker.js", import.meta.url), { type: "module" });
      this.#worker.onmessage = ({ data }) => {
        if (data.crashed) {
          this.#restart();
        }
        this.#pending.get(data.id)?.(data.result ?? null);
        this.#pending.delete(data.id);
      };
      this.#worker.onerror = (e) => {
        e.preventDefault();
        this.#restart();
      };
    }
    return this.#worker;
  }

  /** Ends the worker; whatever it still had to answer gets `null`. */
  #restart() {
    this.#worker?.terminate();
    this.#worker = null;
    for (const resolve of this.#pending.values()) resolve(null);
    this.#pending.clear();
  }
}

/** Runs programs one after another in a worker, off the main thread, so a
 * program that never ends can be stopped without freezing the page. The
 * worker gets one program at a time: stopping a program, or a crash, ends
 * that program only, and the ones queued behind it run on a fresh worker. */
export class Runner {
  #worker = null;
  /** `{ code, resolve }` per program; the first is the one running. */
  #queue = [];

  /** Resolves to `{ output, value, error, millis, crashed }`, or to
   * `{ stopped: true }` when `signal` aborts first. */
  run(code, signal) {
    return new Promise((resolve) => {
      const job = { code, resolve };
      if (signal?.aborted) return resolve({ stopped: true });
      signal?.addEventListener("abort", () => this.#cancel(job), { once: true });
      this.#queue.push(job);
      if (this.#queue.length === 1) this.#next();
    });
  }

  #next() {
    const job = this.#queue[0];
    if (!job) return;
    if (!this.#worker) {
      this.#worker = new Worker(new URL("worker.js", import.meta.url), { type: "module" });
      this.#worker.onmessage = ({ data }) => this.#done(data);
      this.#worker.onerror = (e) => {
        e.preventDefault();
        this.#done({ error: `The interpreter failed to start: ${e.message}`, crashed: true });
      };
    }
    this.#worker.postMessage(job.code);
  }

  /** The running program finished, crashed or was stopped. */
  #done(result) {
    if (result.crashed || result.stopped) {
      // The worker is busy or broken: start a fresh one for what's next.
      this.#worker?.terminate();
      this.#worker = null;
    }
    this.#queue.shift()?.resolve(result);
    this.#next();
  }

  #cancel(job) {
    const at = this.#queue.indexOf(job);
    if (at === 0) {
      this.#done({ stopped: true });
    } else if (at > 0) {
      this.#queue.splice(at, 1);
      job.resolve({ stopped: true });
    }
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
  let binary = "";
  for (const byte of new TextEncoder().encode(code)) binary += String.fromCharCode(byte);
  const base64 = btoa(binary)
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
 * Ctrl/Cmd+Enter calls `onRun`. Tab indents; Esc then Tab moves focus on. */
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
  // After Esc, Tab leaves the editor instead of indenting, so keyboard users
  // aren't trapped in it.
  let tabMovesFocus = false;
  input.addEventListener("keydown", (e) => {
    // Enter and Tab while an input method composes text belong to it.
    if (e.isComposing || e.keyCode === 229) return;
    const escaped = tabMovesFocus;
    tabMovesFocus = e.key === "Escape";
    if (e.key === "Tab" && escaped) return;

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
  // Without the interpreter the code stays plain; whoever called load() says why.
  load().then(render, () => {});
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
