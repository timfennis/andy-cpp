// The playground's code editor: CodeMirror with the interpreter's own
// highlighting, and diagnostics, completion, hover and go-to-definition from
// the language server's features. Optional vim mode. The page passes in
// `language` (see `language()` in ../src/playground/andy.js), so this bundle
// knows nothing about the interpreter itself.
import { Compartment, EditorState, Prec, RangeSetBuilder } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  ViewPlugin,
  drawSelection,
  dropCursor,
  highlightActiveLine,
  highlightActiveLineGutter,
  hoverTooltip,
  keymap,
  lineNumbers,
} from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands";
import { bracketMatching, indentUnit } from "@codemirror/language";
import {
  autocompletion,
  closeBrackets,
  closeBracketsKeymap,
  completionKeymap,
} from "@codemirror/autocomplete";
import { linter, lintKeymap } from "@codemirror/lint";
import { highlightSelectionMatches, searchKeymap } from "@codemirror/search";
import { Vim, vim } from "@replit/codemirror-vim";

/** Turns `parent` into a code editor holding `code`. `onRun` runs on
 * Ctrl/Cmd+Enter, and on `:w` in vim mode. */
export function createCodeEditor(parent, { code = "", onRun = () => {}, language, vimMode = false }) {
  const vimCompartment = new Compartment();
  const view = new EditorView({
    parent,
    state: EditorState.create({
      doc: code,
      extensions: [
        // Vim's keys go before every other keymap, as its docs ask.
        vimCompartment.of(vimMode ? vim({ status: true }) : []),
        Prec.highest(keymap.of([{ key: "Mod-Enter", run: () => (onRun(), true) }])),
        lineNumbers(),
        highlightActiveLineGutter(),
        highlightActiveLine(),
        history(),
        drawSelection(),
        dropCursor(),
        bracketMatching(),
        closeBrackets(),
        highlightSelectionMatches(),
        indentUnit.of("    "),
        EditorState.tabSize.of(4),
        highlighting(language),
        EditorView.updateListener.of((update) => {
          if (update.docChanged) language.update(update.state.doc.toString());
        }),
        linter((view) => diagnostics(view, language), { delay: 300 }),
        autocompletion({ override: [(context) => complete(context, language)] }),
        hoverTooltip((view, pos) => hover(view, pos, language), { hoverTime: 350 }),
        definitions(language),
        keymap.of([
          ...closeBracketsKeymap,
          ...defaultKeymap,
          ...searchKeymap,
          ...historyKeymap,
          ...completionKeymap,
          ...lintKeymap,
          indentWithTab,
        ]),
      ],
    }),
  });
  runOnWrite = onRun;

  return {
    get value() {
      return view.state.doc.toString();
    },
    set value(code) {
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: code } });
    },
    focus: () => view.focus(),
    setVimMode(on) {
      view.dispatch({ effects: vimCompartment.reconfigure(on ? vim({ status: true }) : []) });
    },
  };
}

// `:w` runs the program. Vim's ex commands are global, so they run the editor
// created last; a page has one.
let runOnWrite = () => {};
Vim.defineEx("write", "w", () => runOnWrite());

/** Colours the text with the interpreter's highlighter, as the REPL and the
 * manual do, instead of a second grammar. */
function highlighting(language) {
  const marks = language.classes.map((name) => Decoration.mark({ class: `tok-${name}` }));
  const decorate = (text) => {
    const spans = language.spans(text);
    const builder = new RangeSetBuilder();
    for (let i = 0; i + 2 < spans.length; i += 3) {
      if (spans[i] < spans[i + 1]) builder.add(spans[i], spans[i + 1], marks[spans[i + 2]]);
    }
    return builder.finish();
  };
  return ViewPlugin.fromClass(
    class {
      constructor(view) {
        this.decorations = decorate(view.state.doc.toString());
      }
      update(update) {
        if (update.docChanged) this.decorations = decorate(update.state.doc.toString());
      }
    },
    { decorations: (plugin) => plugin.decorations },
  );
}

/** The offset of an LSP position (line, UTF-16 column) in `doc`. */
function offset(doc, { line, character }) {
  if (line >= doc.lines) return doc.length;
  const { from, to } = doc.line(line + 1);
  return Math.min(from + character, to);
}

/** The LSP position of an offset in `doc`. */
function position(doc, pos) {
  const line = doc.lineAt(pos);
  return { line: line.number - 1, character: pos - line.from };
}

async function diagnostics(view, language) {
  const doc = view.state.doc;
  const found = (await language.diagnostics(doc.toString())) ?? [];
  return found.map((d) => ({
    from: offset(doc, d.range.start),
    to: offset(doc, d.range.end),
    severity: "error",
    message: d.message,
  }));
}

// LSP completion item kinds to the names CodeMirror styles them by.
const KINDS = { 2: "method", 3: "function", 5: "property", 6: "variable", 7: "class", 14: "keyword", 21: "constant", 22: "class" };

// The characters of a name, as the lexer reads them: Unicode letters and
// digits, `_`, and `?` (`get?`). Completion replaces the whole name, so
// `écl` becomes `éclair`, not `ééclair`.
const NAME = /[\p{Alphabetic}\p{N}_?]+/u;
const PARTIAL_NAME = /^[\p{Alphabetic}\p{N}_?]*$/u;

async function complete(context, language) {
  const word = context.matchBefore(NAME);
  const start = word ? word.from : context.pos;
  const afterDot = context.state.sliceDoc(start - 1, start) === ".";
  if (!word && !afterDot && !context.explicit) return null;

  // After `xs.lo`, ask about the position right after the dot: that's where
  // the language features look for the receiver whose type filters the list.
  const doc = context.state.doc;
  const { line, character } = position(doc, afterDot ? start : context.pos);
  const response = await language.completion(doc.toString(), line, character);
  const items = Array.isArray(response) ? response : (response?.items ?? []);
  return {
    from: start,
    validFor: PARTIAL_NAME,
    options: items.map((item) => ({
      label: item.label,
      type: KINDS[item.kind] ?? "text",
      detail: item.labelDetails
        ? `${item.labelDetails.detail ?? ""}${item.labelDetails.description ? ` -> ${item.labelDetails.description}` : ""}`
        : item.detail,
      info: documentation(item.documentation)
        ? () => markdown(documentation(item.documentation), language)
        : undefined,
    })),
  };
}

function documentation(doc) {
  if (!doc) return "";
  return typeof doc === "string" ? doc : (doc.value ?? "");
}

async function hover(view, pos, language) {
  const doc = view.state.doc;
  const { line, character } = position(doc, pos);
  const answer = await language.hover(doc.toString(), line, character);
  if (!answer) return null;
  const contents = Array.isArray(answer.contents) ? answer.contents : [answer.contents];
  const text = contents.map((c) => (typeof c === "string" ? c : c.value)).join("\n\n");
  return {
    pos: answer.range ? offset(doc, answer.range.start) : pos,
    end: answer.range ? offset(doc, answer.range.end) : pos,
    above: true,
    create: () => ({ dom: markdown(text, language) }),
  };
}

/** F12, or Ctrl/Cmd+click, jumps to where the name under the cursor is declared. */
function definitions(language) {
  const jump = async (view, pos) => {
    const doc = view.state.doc;
    const { line, character } = position(doc, pos);
    const range = await language.definition(doc.toString(), line, character);
    if (range) {
      view.dispatch({
        selection: { anchor: offset(doc, range.start), head: offset(doc, range.end) },
        scrollIntoView: true,
      });
    }
  };
  return [
    keymap.of([{ key: "F12", run: (view) => (jump(view, view.state.selection.main.head), true) }]),
    EditorView.domEventHandlers({
      mousedown(event, view) {
        if (!(event.ctrlKey || event.metaKey)) return false;
        const pos = view.posAtCoords(event);
        if (pos == null) return false;
        event.preventDefault();
        jump(view, pos);
        return true;
      },
    }),
  ];
}

/** The little markdown hover and completion docs use: ```ndc fences become
 * highlighted code, `inline code` stays code, the rest is paragraphs. */
function markdown(text, language) {
  const dom = document.createElement("div");
  dom.className = "ndc-docs";
  const fence = /```\w*\n?([\s\S]*?)```/g;
  let last = 0;
  for (const match of text.matchAll(fence)) {
    paragraphs(text.slice(last, match.index), dom);
    const pre = document.createElement("pre");
    pre.innerHTML = language.highlight(match[1].replace(/\n$/, ""));
    dom.append(pre);
    last = match.index + match[0].length;
  }
  paragraphs(text.slice(last), dom);
  return dom;
}

function paragraphs(text, dom) {
  for (const block of text.split(/\n\s*\n/)) {
    if (!block.trim()) continue;
    const p = document.createElement("p");
    block.split(/(`[^`]+`)/).forEach((part, i) => {
      if (i % 2) {
        const code = document.createElement("code");
        code.textContent = part.slice(1, -1);
        p.append(code);
      } else {
        p.append(part);
      }
    });
    dom.append(p);
  }
}
