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
  for (const frame of frames) {
    frame.src = new URL(`index.html?theme=${theme()}`, playground).href;
  }
  new MutationObserver(() => {
    for (const frame of frames) frame.contentWindow?.postMessage({ theme: theme() }, location.origin);
  }).observe(document.documentElement, { attributes: true, attributeFilter: ["class"] });

  if (blocks.length === 0) return;

  const icon = (path) =>
    `<span class="fa-svg"><svg viewBox="0 0 24 24" aria-hidden="true"><path d="${path}"/></svg></span>`;
  const PLAY = icon("M8 5v14l11-7z");
  const STOP = icon("M6 6h12v12H6z");
  const OPEN = icon(
    "M19 19H5V5h7V3H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2v-7h-2v7zM14 3v2h3.59l-9.83 9.83 1.41 1.41L19 6.41V10h2V3h-7z",
  );

  function button(title, html) {
    const b = document.createElement("button");
    b.title = title;
    b.setAttribute("aria-label", title);
    b.innerHTML = html;
    return b;
  }

  import(new URL("andy.js", playground).href).then(async (andy) => {
    await andy.load();
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
      const run = button("Run this code", PLAY);
      const open = button("Open in the playground", OPEN);
      buttons.prepend(run, open);

      open.addEventListener("click", () => window.open(andy.playgroundUrl(source), "_blank", "noopener"));

      let running = false;
      run.addEventListener("click", async () => {
        runner ??= new andy.Runner();
        if (running) {
          runner.stop();
          return;
        }
        let result = pre.querySelector(".result");
        if (!result) {
          result = document.createElement("code");
          result.className = "result ndc hljs";
          pre.append(result);
        }
        running = true;
        run.innerHTML = STOP;
        run.title = "Stop";
        result.innerHTML = `<span class="ndc-muted">Running…</span>`;

        const html = andy.resultHtml(await runner.run(source));
        result.innerHTML = html || `<span class="ndc-muted">No output</span>`;
        running = false;
        run.innerHTML = PLAY;
        run.title = "Run this code";
      });
    }
  }).catch((err) => {
    console.warn("Andy C++ examples can't run: build the interpreter with ndc_wasm/build.sh.", err);
  });
})();
