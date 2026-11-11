/* tslint:disable */
/* eslint-disable */
/**
 * Runs a whole program in a fresh session.
 */
export function run(code: string): Evaluation;
/**
 * `source` as HTML, each piece in a `<span>` whose class says what it is:
 * `let x` becomes `<span class="tok-keyword">let</span> <span class="tok-variable">x</span>`.
 */
export function highlight(source: string): string;
/**
 * What one evaluation printed and produced.
 */
export class Evaluation {
  private constructor();
  free(): void;
  /**
   * What the code printed.
   */
  output: string;
  /**
   * The value of the last expression, as the REPL would show it; empty for
   * statements and unit.
   */
  value: string;
  /**
   * The rendered diagnostics, if the code failed.
   */
  get error(): string | undefined;
  /**
   * The rendered diagnostics, if the code failed.
   */
  set error(value: string | null | undefined);
  /**
   * True when the code failed past parsing, so the session forgot what
   * earlier evaluations declared.
   */
  started_over: boolean;
}
/**
 * A REPL-like session: each evaluation sees what earlier ones declared.
 */
export class Session {
  free(): void;
  constructor();
  eval(code: string): Evaluation;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_evaluation_free: (a: number, b: number) => void;
  readonly __wbg_get_evaluation_error: (a: number) => [number, number];
  readonly __wbg_get_evaluation_output: (a: number) => [number, number];
  readonly __wbg_get_evaluation_started_over: (a: number) => number;
  readonly __wbg_get_evaluation_value: (a: number) => [number, number];
  readonly __wbg_session_free: (a: number, b: number) => void;
  readonly __wbg_set_evaluation_error: (a: number, b: number, c: number) => void;
  readonly __wbg_set_evaluation_output: (a: number, b: number, c: number) => void;
  readonly __wbg_set_evaluation_started_over: (a: number, b: number) => void;
  readonly __wbg_set_evaluation_value: (a: number, b: number, c: number) => void;
  readonly highlight: (a: number, b: number) => [number, number];
  readonly run: (a: number, b: number) => number;
  readonly session_eval: (a: number, b: number, c: number) => number;
  readonly session_new: () => number;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __externref_table_alloc: () => number;
  readonly __wbindgen_export_2: WebAssembly.Table;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
*
* @returns {InitOutput}
*/
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
