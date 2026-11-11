// Runs Andy C++ off the main thread, so a long or endless program can be
// stopped by terminating this worker without freezing the page.
import init, { run } from "./pkg/ndc_wasm.js";

const ready = init();

self.onmessage = async ({ data: code }) => {
  await ready;
  const started = performance.now();
  let reply;
  try {
    const result = run(code);
    reply = { output: result.output, value: result.value, error: result.error ?? null };
  } catch (panic) {
    // A Rust panic traps the wasm instance, so this worker is done for.
    reply = { output: "", value: "", error: `The interpreter crashed: ${panic}`, crashed: true };
  }
  self.postMessage({ ...reply, millis: performance.now() - started });
};
