// Andy C++ in the manual: highlights ```ndc code blocks with the interpreter's
// own highlighter, lets readers run them (unless marked ```ndc,norun), and
// keeps the embedded playground in the reader's theme.
(() => {
  const blocks = Array.from(document.querySelectorAll("pre > code.language-ndc"));
  const frames = Array.from(document.querySelectorAll("iframe.ndc-playground"));
  if (blocks.length === 0 && frames.length === 0) return;

  // path_to_root is set by mdBook on every page.
  const playground = new URL("playground/", new URL(path_to_root, location.href));

  const darkThemes = ["coal", "navy", "ayu"];
  const theme = () =>
    darkThemes.some((t) => document.documentElement.classList.contains(t)) ? "dark" : "light";
  const sendTheme = (frame) => frame.contentWindow?.postMessage({ theme: theme() }, location.origin);
  for (const frame of frames) {
    // Also on load, in case the theme changed while the playground loaded.
    frame.addEventListener("load", () => sendTheme(frame));
    frame.src = new URL(`index.html?theme=${theme()}`, playground).href;
  }
  new MutationObserver(() => frames.forEach(sendTheme)).observe(document.documentElement, {
    attributes: true,
    attributeFilter: ["class"],
  });

  if (blocks.length === 0) return;

  const icon = (path) =>
    `<span class="fa-svg"><svg viewBox="0 0 24 24" aria-hidden="true"><path d="${path}"/></svg></span>`;
  const PLAY = icon("M8 5v14l11-7z");
  const STOP = icon("M6 6h12v12H6z");
  const OPEN = icon(
    "M19 19H5V5h7V3H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7h-2v7zM14 3v2h3.59l-9.83 9.83 1.41 1.41L19 6.41V10h2V3h-7z",
  );

  function label(button, title, html) {
    button.title = title;
    button.setAttribute("aria-label", title);
    button.innerHTML = html;
    return button;
  }

  import(new URL("andy.js", playground).href).then(async (andy) => {
    await andy.load();
    // One worker for the page's examples; they queue behind each other.
    let runner = null;

    for (const code of blocks) {
      const source = code.textContent;
      code.innerHTML = andy.highlight(source);
      if (code.classList.contains("norun")) continue;

      const pre = code.parentElement;
      let buttons = pre.querySelector(".buttons");
      if (!buttons) {
        buttons = document.createElement("div");
        buttons.className = "buttons";
        pre.prepend(buttons);
      }
      const run = label(document.createElement("button"), "Run this code", PLAY);
      const open = label(document.createElement("button"), "Open in the playground", OPEN);
      buttons.prepend(run, open);

      open.addEventListener("click", () => window.open(andy.playgroundUrl(source), "_blank", "noopener"));

      // While this example runs or waits its turn, its button stops it.
      let running = null;
      run.addEventListener("click", async () => {
        if (running) {
          running.abort();
          return;
        }
        runner ??= new andy.Runner();
        let result = pre.querySelector(".result");
        if (!result) {
          result = document.createElement("code");
          result.className = "result ndc hljs";
          pre.append(result);
        }
        running = new AbortController();
        label(run, "Stop", STOP);
        result.innerHTML = `<span class="ndc-muted">Running…</span>`;

        const html = andy.resultHtml(await runner.run(source, running.signal));
        result.innerHTML = html || `<span class="ndc-muted">No output</span>`;
        running = null;
        label(run, "Run this code", PLAY);
      });
    }
  }).catch((err) => {
    console.warn("Andy C++ examples can't run: build the interpreter with ndc_wasm/build.sh.", err);
  });
})();
