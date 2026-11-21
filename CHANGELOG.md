# Changelog

## [0.5.0](https://github.com/timfennis/andy-cpp/compare/v0.4.1...v0.5.0) (2026-11-21)


### ⚠ BREAKING CHANGES

* **vm:** a named function can only be declared as a statement; inside an expression it is now an error.

### Features

* **lexer:** accept float literals with an exponent like 1e-12 🔬 ([#477](https://github.com/timfennis/andy-cpp/issues/477)) ([e2e1032](https://github.com/timfennis/andy-cpp/commit/e2e1032b35f1a80b86d0fc212694b06d63784e3e))
* **lexer:** support \u{...} Unicode escapes in strings 🦄 ([#478](https://github.com/timfennis/andy-cpp/issues/478)) ([751c60f](https://github.com/timfennis/andy-cpp/commit/751c60f962aaff3668643749f9d196d3a0be1644))


### Bug Fixes

* **analyser:** settle what the last fallbacks' solving makes 🧷 ([#484](https://github.com/timfennis/andy-cpp/issues/484)) ([90bfdbe](https://github.com/timfennis/andy-cpp/commit/90bfdbeb64a7ed5e05be39ad99b1e827ce4691fd))
* **lexer:** spell the invalid-escape error "Invalid escape sequence" ✏️ ([#471](https://github.com/timfennis/andy-cpp/issues/471)) ([94454c9](https://github.com/timfennis/andy-cpp/commit/94454c97c172d3e19680715a59f956e06fb32cbd))
* **lsp:** leave out types whose check failed instead of showing Any 🙈 ([#479](https://github.com/timfennis/andy-cpp/issues/479)) ([3aaa34e](https://github.com/timfennis/andy-cpp/commit/3aaa34e258ed5ab34a8bb93b1c575d3e6c823f66))
* **parser:** say what's missing instead of "Expected an expression" 🧭 ([#483](https://github.com/timfennis/andy-cpp/issues/483)) ([90d1364](https://github.com/timfennis/andy-cpp/commit/90d1364cba261f1381c6e76b2f27519024dec77c))
* **vm:** deepcopy copies each container once, keeping cycles and sharing 🔁 ([#482](https://github.com/timfennis/andy-cpp/issues/482)) ([edf2b70](https://github.com/timfennis/andy-cpp/commit/edf2b70a9345ecbb6f45d090e58e6f04f9ff3204))
* **vm:** iterate a string or map that changes during the loop 🔂 ([#537](https://github.com/timfennis/andy-cpp/issues/537)) ([c9106c8](https://github.com/timfennis/andy-cpp/commit/c9106c8eb315346c57affd76b7d25c1508c0c89c))


### Performance Improvements

* exit without freeing the program's values 🚪 ([#481](https://github.com/timfennis/andy-cpp/issues/481)) ([41fb803](https://github.com/timfennis/andy-cpp/commit/41fb803c67233af2f2db088016d2e834f7f5ad2b))
* read a tuple's element at a literal index in place 📦 ([#491](https://github.com/timfennis/andy-cpp/issues/491)) ([ed19ec5](https://github.com/timfennis/andy-cpp/commit/ed19ec585347a5c6038531876a233d54903749b6))
* **stdlib:** decode JSON integers without allocating 🔢 ([#474](https://github.com/timfennis/andy-cpp/issues/474)) ([3625aac](https://github.com/timfennis/andy-cpp/commit/3625aac84495b95bdef378b90ef8454a105dac37))
* **vm:** hash maps with FxHash and memo keys with 128 bits 🧮 ([#533](https://github.com/timfennis/andy-cpp/issues/533)) ([57ba180](https://github.com/timfennis/andy-cpp/commit/57ba180f10ae2f575d1d77f56a0c487e393d8fea))
* **vm:** read a local tuple's element at a literal index in one instruction 🎯 ([#492](https://github.com/timfennis/andy-cpp/issues/492)) ([8b6437b](https://github.com/timfennis/andy-cpp/commit/8b6437bea58f4675a4d6658ba67f77b7ede777ee))
* **vm:** read and write a list element in one instruction 📇 ([#534](https://github.com/timfennis/andy-cpp/issues/534)) ([25bf432](https://github.com/timfennis/andy-cpp/commit/25bf43229620a30d7b2c66caa4ba4fdfc1bcb77e))
* **vm:** replace the stack VM with a register VM 🚀 ([#523](https://github.com/timfennis/andy-cpp/issues/523)) ([a5ae39e](https://github.com/timfennis/andy-cpp/commit/a5ae39eb4f60dd6bd7ef0d7047220b1eb034529a))
* **vm:** unpack a pattern of locals straight into their slots 📥 ([#493](https://github.com/timfennis/andy-cpp/issues/493)) ([5b291ce](https://github.com/timfennis/andy-cpp/commit/5b291ce99a25adc554d0774f697f249c11bad3e8))
* **vm:** write natives as instructions and call the rest by their slot 🏎️ ([#485](https://github.com/timfennis/andy-cpp/issues/485)) ([a6dc860](https://github.com/timfennis/andy-cpp/commit/a6dc86030cdd52134d6f848778d380d492c9dad6))

## [0.4.1](https://github.com/timfennis/andy-cpp/compare/v0.4.0...v0.4.1) (2026-11-19)


### Bug Fixes

* hash with fixed seeds so every run iterates maps the same way 🌱 ([#468](https://github.com/timfennis/andy-cpp/issues/468)) ([808f87c](https://github.com/timfennis/andy-cpp/commit/808f87c0d23c8f94f85e3bf519739b6b61fe0859))
* **layout:** list each function's instances in the order they were made 🎲 ([#467](https://github.com/timfennis/andy-cpp/issues/467)) ([46320da](https://github.com/timfennis/andy-cpp/commit/46320da9a59c2cc368220b16acb920ad5b0bba12))

## [0.4.0](https://github.com/timfennis/andy-cpp/compare/v0.3.0...v0.4.0) (2026-11-19)


### ⚠ BREAKING CHANGES

* replace `as` casts with cast::<T>(x) and add type arguments 🐟 ([#450](https://github.com/timfennis/andy-cpp/issues/450))
* write types like their values: [T], (A, B), %{K: V}, fn(A) -> R 🧩 ([#449](https://github.com/timfennis/andy-cpp/issues/449))
* **stdlib:** return pairs as tuples and rename circular_pairwise 👯 ([#448](https://github.com/timfennis/andy-cpp/issues/448))

### Features

* add checked as-cast expression 🎯 ([#215](https://github.com/timfennis/andy-cpp/issues/215)) ([4d5eced](https://github.com/timfennis/andy-cpp/commit/4d5eced672a4766e2181ea94f4c6416e0107a57a))
* add JetBrains IDE support via TextMate bundle and LSP4IJ 🧩 ([#177](https://github.com/timfennis/andy-cpp/issues/177)) ([6e276f2](https://github.com/timfennis/andy-cpp/commit/6e276f2da9c06709485ec885ad50a73469379deb))
* add struct definitions with field access and assignment 🏗️ ([#197](https://github.com/timfennis/andy-cpp/issues/197)) ([295fa3c](https://github.com/timfennis/andy-cpp/commit/295fa3c3b41bdc500bff15ac959ab2d3fa177f58))
* **analyser:** look names up without looking at types 🔎 ([#283](https://github.com/timfennis/andy-cpp/issues/283)) ([f8c0ed8](https://github.com/timfennis/andy-cpp/commit/f8c0ed827126a431f024484e41e251f2aa3b5177))
* **analyser:** point to `++` and `<>` when `+` is given a string 🧶 ([#454](https://github.com/timfennis/andy-cpp/issues/454)) ([bc6e500](https://github.com/timfennis/andy-cpp/commit/bc6e5004e3d219afc41efe71cd801e606ee0b5f1))
* **cli:** add run phase timings ⏱️ ([#126](https://github.com/timfennis/andy-cpp/issues/126)) ([4d81c55](https://github.com/timfennis/andy-cpp/commit/4d81c55963fe6daf61fa5e6ee14fbd5352f566c9))
* **cli:** run a program passed on the command line with --eval 🧪 ([#314](https://github.com/timfennis/andy-cpp/issues/314)) ([9501560](https://github.com/timfennis/andy-cpp/commit/950156020d400c3eacfef236539d416a8cc459eb))
* **ext:** add tree-sitter grammar and refresh TextMate grammar 🌳 ([#165](https://github.com/timfennis/andy-cpp/issues/165)) ([5d1e8af](https://github.com/timfennis/andy-cpp/commit/5d1e8af3dbcb2e5f52f612395af6a441c80c2c0a))
* **fmt:** format Andy C++ code with ndc fmt 🧹 ([#307](https://github.com/timfennis/andy-cpp/issues/307)) ([6a49e65](https://github.com/timfennis/andy-cpp/commit/6a49e65dbe7f60844c1fb3d4b30053a0bf3e2fad))
* **lsp:** hover, document symbols, go-to-definition and smarter completion 🔍 ([#164](https://github.com/timfennis/andy-cpp/issues/164)) ([98be07d](https://github.com/timfennis/andy-cpp/commit/98be07dc880b892ced3bef645fd49ac17fcc20db))
* new type checker 🏷️ ([#291](https://github.com/timfennis/andy-cpp/issues/291)) ([be79b12](https://github.com/timfennis/andy-cpp/commit/be79b128b35dda0f309a3de0e509441b1f3702fb))
* **playground:** a real editor with completion, hover and vim mode ⌨️ ([#299](https://github.com/timfennis/andy-cpp/issues/299)) ([2a50672](https://github.com/timfennis/andy-cpp/commit/2a506728d7d2e07e6b36bf7081d70b37468605ba))
* read standard input with read_stdin 📥 ([#446](https://github.com/timfennis/andy-cpp/issues/446)) ([ac64124](https://github.com/timfennis/andy-cpp/commit/ac64124cbbbaeb6b9be9294cea93bc7e83439870))
* rebuild the analyser on a name resolver and a layout pass 🔍 ([#226](https://github.com/timfennis/andy-cpp/issues/226)) ([c3a26eb](https://github.com/timfennis/andy-cpp/commit/c3a26ebc7510b15635f05d89e6dda34fffeb7123))
* replace `as` casts with cast::&lt;T&gt;(x) and add type arguments 🐟 ([#450](https://github.com/timfennis/andy-cpp/issues/450)) ([2e6b3cb](https://github.com/timfennis/andy-cpp/commit/2e6b3cb1b6aa09acd89bc40148b80c3256879645))
* replace numeric subtyping with explicit modes 🔢 ([#210](https://github.com/timfennis/andy-cpp/issues/210)) ([5d07cbb](https://github.com/timfennis/andy-cpp/commit/5d07cbbdcda6fca53c404220f340863839e77c3c))
* **resolver:** make functions declared with fn constants 🔒 ([#282](https://github.com/timfennis/andy-cpp/issues/282)) ([13346fe](https://github.com/timfennis/andy-cpp/commit/13346fef3c6c036190bbd7a9222cf34c564d3ff4))
* **resolver:** suggest a similar name for an undeclared identifier 🔍 ([#452](https://github.com/timfennis/andy-cpp/issues/452)) ([1b5c4f7](https://github.com/timfennis/andy-cpp/commit/1b5c4f7be1ce97e54bb972d538dd7b67457485b2))
* **stdlib:** add list get? and unwrap_or 🎁 ([#167](https://github.com/timfennis/andy-cpp/issues/167)) ([4ed65cc](https://github.com/timfennis/andy-cpp/commit/4ed65ccedd414f359ea4f7a35aa78e83a32e68f4))
* **stdlib:** add list get/get?/index/index? accessors 🎯 ([#169](https://github.com/timfennis/andy-cpp/issues/169)) ([679dde8](https://github.com/timfennis/andy-cpp/commit/679dde850deb5700be03a506ec7467ce04b54421))
* **stdlib:** add scan, a fold that returns every step 🪜 ([#457](https://github.com/timfennis/andy-cpp/issues/457)) ([adfab79](https://github.com/timfennis/andy-cpp/commit/adfab79ba6471f38a1c24518b8e8ae4b03755513))
* **stdlib:** add string affix stripping functions ✂️ ([dea5c77](https://github.com/timfennis/andy-cpp/commit/dea5c77de9068ec35b8c24f0886e4ba0d6ada4e4))
* **stdlib:** name the text and index in parse and index errors 🔢 ([#453](https://github.com/timfennis/andy-cpp/issues/453)) ([3bb7954](https://github.com/timfennis/andy-cpp/commit/3bb795488ff1abba817f72aaa8c4ab1d8edd3adc))
* **stdlib:** return pairs as tuples and rename circular_pairwise 👯 ([#448](https://github.com/timfennis/andy-cpp/issues/448)) ([596d64a](https://github.com/timfennis/andy-cpp/commit/596d64a5a2600f8c8e74650eec1ff36c7b8cd31b))
* type annotations 📚 ([#131](https://github.com/timfennis/andy-cpp/issues/131)) ([4d272e3](https://github.com/timfennis/andy-cpp/commit/4d272e34397aa37ccdcfab530167e28f8a45d482))
* **vectorization:** broaden tuple operators and recover precise types 📐 ([#146](https://github.com/timfennis/andy-cpp/issues/146)) ([ae1e616](https://github.com/timfennis/andy-cpp/commit/ae1e6169768021c60affdbb6eae4137b6c3c7f90))
* **wasm:** run Andy C++ in the browser and in the manual 🕸️ ([#293](https://github.com/timfennis/andy-cpp/issues/293)) ([299f192](https://github.com/timfennis/andy-cpp/commit/299f1925728a1b506e87d93573374bd6a357d857))
* write types like their values: [T], (A, B), %{K: V}, fn(A) -&gt; R 🧩 ([#449](https://github.com/timfennis/andy-cpp/issues/449)) ([2f48c19](https://github.com/timfennis/andy-cpp/commit/2f48c1957133c5e504f3b3ee20c287c191e9028a))


### Bug Fixes

* **analyser:** commit an operator whose overload takes tuples whole 🩹 ([#442](https://github.com/timfennis/andy-cpp/issues/442)) ([76e95a0](https://github.com/timfennis/andy-cpp/commit/76e95a06aea71781acb31c83e2b6546ace28d7a8))
* **analyser:** fixed unpacking error 📦 ([5d9835c](https://github.com/timfennis/andy-cpp/commit/5d9835cf468f96787ba764264ea6e20c95328950))
* **analyser:** infer what a recursive branch returns from the other branch 🔁 ([#456](https://github.com/timfennis/andy-cpp/issues/456)) ([2372607](https://github.com/timfennis/andy-cpp/commit/2372607523d6f0a0da9c96a189bb7cc8c83ed748))
* **analyser:** pick most-specific overload in scope 🎯 ([#158](https://github.com/timfennis/andy-cpp/issues/158)) ([4bfa9f1](https://github.com/timfennis/andy-cpp/commit/4bfa9f173728cdfc9a1190d28a6cb440e5f10e70))
* **analyser:** reject assigning to a tuple element 🩹 ([#294](https://github.com/timfennis/andy-cpp/issues/294)) ([7b2826b](https://github.com/timfennis/andy-cpp/commit/7b2826b542abb52e8602f3d83f2cd50f68e81050))
* **analyser:** resolve struct type names through lexical scopes 🔭 ([#208](https://github.com/timfennis/andy-cpp/issues/208)) ([9c7a3c5](https://github.com/timfennis/andy-cpp/commit/9c7a3c50e5ae593886f500b52e053440197a0c59))
* **analyser:** widen Binding::Dynamic result to Any 🪟 ([#140](https://github.com/timfennis/andy-cpp/issues/140)) ([3f2dbc6](https://github.com/timfennis/andy-cpp/commit/3f2dbc695f479721e6cf1db94fcc2f92337c1b38))
* **build:** add versions for workspace path dependencies ([64bd5e7](https://github.com/timfennis/andy-cpp/commit/64bd5e71cd8de248983cb24c4f333f215f276866))
* **ci:** run checks for stacked pull requests 🔧 ([#196](https://github.com/timfennis/andy-cpp/issues/196)) ([abb4842](https://github.com/timfennis/andy-cpp/commit/abb48429cc62fbbbefd0be37ea153e3178d72747))
* clippy warnings 📎 ([acc2990](https://github.com/timfennis/andy-cpp/commit/acc2990e03bb2757b288946668de0caf86db3613))
* **core:** promote to BigInt instead of panicking on neg/abs of i64::MIN 💥 ([#172](https://github.com/timfennis/andy-cpp/issues/172)) ([331a89b](https://github.com/timfennis/andy-cpp/commit/331a89b4937f632df14b64f71d4faf1255786fb6))
* **core:** return error instead of panicking on integer division by zero 💥 ([#168](https://github.com/timfennis/andy-cpp/issues/168)) ([d83a480](https://github.com/timfennis/andy-cpp/commit/d83a48000f227846acec30c3b67a0c0970341054))
* **core:** return error instead of panicking on negative power edge cases 💥 ([#171](https://github.com/timfennis/andy-cpp/issues/171)) ([b925b60](https://github.com/timfennis/andy-cpp/commit/b925b6018d461537942b02bb70bc5714d6145f46))
* **ext:** make tree-sitter highlighting work in Helix 🚁 ([#166](https://github.com/timfennis/andy-cpp/issues/166)) ([22939e0](https://github.com/timfennis/andy-cpp/commit/22939e0204adddc8284f320f4a4f50b85ab5399e))
* fixed augmented assignment 🦾 ([#192](https://github.com/timfennis/andy-cpp/issues/192)) ([1a091ee](https://github.com/timfennis/andy-cpp/commit/1a091eec8ab44b412fb6f7fbf312c0547e9a4aee))
* harden struct parsing, copying, and tooling after review 🛡️ ([#204](https://github.com/timfennis/andy-cpp/issues/204)) ([1320fce](https://github.com/timfennis/andy-cpp/commit/1320fcea8a64d78624d6461d9c5aebb0e45f1e56))
* **interpreter:** optimize file execution ⚡ ([#221](https://github.com/timfennis/andy-cpp/issues/221)) ([bf10862](https://github.com/timfennis/andy-cpp/commit/bf1086286883f1c2e5d40e3ead55b089d682339b))
* **parser:** accept trailing commas in lists and map comprehensions 🪶 ([#302](https://github.com/timfennis/andy-cpp/issues/302)) ([4baecb8](https://github.com/timfennis/andy-cpp/commit/4baecb8c95ede3d2512a309b0f1601427df31870))
* **parser:** report a missing `;` where it goes 🎯 ([#455](https://github.com/timfennis/andy-cpp/issues/455)) ([ff739ae](https://github.com/timfennis/andy-cpp/commit/ff739ae6beb6d09e9beecc55812dad0cd68f8aef))
* pass print and other built-ins taking any arguments as callbacks 🖨️ ([#444](https://github.com/timfennis/andy-cpp/issues/444)) ([c4fb340](https://github.com/timfennis/andy-cpp/commit/c4fb34073b68b91443c68de8c8088ab944455504))
* **stdlib:** error instead of panicking on reversed slice ranges 🔪 ([#174](https://github.com/timfennis/andy-cpp/issues/174)) ([7090596](https://github.com/timfennis/andy-cpp/commit/7090596c0ad5ac8900171547c7a7a8f867724c7a))
* **stdlib:** preserve string types in prefixes and suffixes 🧵 ([#218](https://github.com/timfennis/andy-cpp/issues/218)) ([2ed1d1b](https://github.com/timfennis/andy-cpp/commit/2ed1d1b33563a42a85292e4f57910d0b12a88e5a))
* **stdlib:** return error instead of panicking on out-of-range combinations size 🧮 ([#173](https://github.com/timfennis/andy-cpp/issues/173)) ([8863df1](https://github.com/timfennis/andy-cpp/commit/8863df13383c557f0498c558e6cac3f8139c17cc))
* **stdlib:** return error instead of panicking on windows size 0 🪟 ([#170](https://github.com/timfennis/andy-cpp/issues/170)) ([5fc2d9c](https://github.com/timfennis/andy-cpp/commit/5fc2d9c2e28d02de0b8dd6170caa43e34d102ebb))
* **stdlib:** use UTF-8 boundaries for string assignment 🧵 ([#219](https://github.com/timfennis/andy-cpp/issues/219)) ([4b35009](https://github.com/timfennis/andy-cpp/commit/4b350092f3133cf7e36874923ae3d77c3ad2fa51))
* **vm:** stop a program whose calls go a million deep 🪆 ([#390](https://github.com/timfennis/andy-cpp/issues/390)) ([5fa6555](https://github.com/timfennis/andy-cpp/commit/5fa6555f9cc4cdabccaf88da39166997dd2e8e53))


### Performance Improvements

* **analyser:** group a literal's values by hash, not a scan 🧺 ([#464](https://github.com/timfennis/andy-cpp/issues/464)) ([aa37635](https://github.com/timfennis/andy-cpp/commit/aa376357bae98362d635db538ae9dfe5ce533fc7))
* **analyser:** summarise literal bounds instead of copying them 🧮 ([#465](https://github.com/timfennis/andy-cpp/issues/465)) ([e22911b](https://github.com/timfennis/andy-cpp/commit/e22911b896d3d0d173ee3da0b32f36afc2993d9b))
* **core:** parse signatures without allocating a string per token 🏎️ ([#460](https://github.com/timfennis/andy-cpp/issues/460)) ([c21fff8](https://github.com/timfennis/andy-cpp/commit/c21fff8fc07869cfdedac70362f21f903bb94f8b))
* **interpreter:** skip work a one-shot run never uses 🗑️ ([#461](https://github.com/timfennis/andy-cpp/issues/461)) ([4c5d141](https://github.com/timfennis/andy-cpp/commit/4c5d141063ed8978449b7e6be4aa7a7f1a768951))
* **macros:** parse signatures while the macro expands 🧩 ([#459](https://github.com/timfennis/andy-cpp/issues/459)) ([497b2ec](https://github.com/timfennis/andy-cpp/commit/497b2ec442877b40fa2d86fa92ea903fdb725890))
* make natives on first use 🥚 ([#466](https://github.com/timfennis/andy-cpp/issues/466)) ([1b4b647](https://github.com/timfennis/andy-cpp/commit/1b4b647a591dff25ceff613f56b4d2c779005062))
* **stdlib:** drop per-call Vec alloc for List/String/Deque indexing 📇 ([#160](https://github.com/timfennis/andy-cpp/issues/160)) ([560e0c3](https://github.com/timfennis/andy-cpp/commit/560e0c3f75faff70b45e90054224896b58c373f0))
* **stdlib:** make enumerate return a lazy iterator 🦥 ([#138](https://github.com/timfennis/andy-cpp/issues/138)) ([a818a9a](https://github.com/timfennis/andy-cpp/commit/a818a9a6ab8407ae1b8462e4c834ab9f6cb1ddf2))
* **vm:** add peephole pass to elide dead Load; Pop pairs ✂️ ([#163](https://github.com/timfennis/andy-cpp/issues/163)) ([38837bf](https://github.com/timfennis/andy-cpp/commit/38837bfce34479d91808f805cedcc58a3acabfcb))
* **vm:** borrow callee name in vec dispatch instead of allocating 🪢 ([#155](https://github.com/timfennis/andy-cpp/issues/155)) ([b8d1c05](https://github.com/timfennis/andy-cpp/commit/b8d1c0504eb04994f3e70da658adb63698cf7430))
* **vm:** cache string character indexes with char_index 🧵 ([#223](https://github.com/timfennis/andy-cpp/issues/223)) ([2bec7a8](https://github.com/timfennis/andy-cpp/commit/2bec7a8a562de761f0e7624195849136f33efb58))
* **vm:** defer GetIterator type stringification to error path 🦥 ([#147](https://github.com/timfennis/andy-cpp/issues/147)) ([87e2e1b](https://github.com/timfennis/andy-cpp/commit/87e2e1bf2c4c2312b9b1d8a40751a251ee14ef18))
* **vm:** lazily clone parent upvalues on Closure 🪺 ([#156](https://github.com/timfennis/andy-cpp/issues/156)) ([5c896d4](https://github.com/timfennis/andy-cpp/commit/5c896d424f928ffbf5df4d268f7b600109de13eb))
* **vm:** preallocate map literal HashMap with known pair count 🗺️ ([#157](https://github.com/timfennis/andy-cpp/issues/157)) ([7afd812](https://github.com/timfennis/andy-cpp/commit/7afd8121b6c98ca7e46c4a1d46c32371d5ba19f2))
* **vm:** swap Box for Rc in Function::Memoized inner function 🧠 ([#159](https://github.com/timfennis/andy-cpp/issues/159)) ([cb978df](https://github.com/timfennis/andy-cpp/commit/cb978df89c7ceb27b67777c94ab50182a6a7fcc8))


### Miscellaneous Chores

* release the type checker as 0.4.0 🏷️ ([#441](https://github.com/timfennis/andy-cpp/issues/441)) ([d3dbfd5](https://github.com/timfennis/andy-cpp/commit/d3dbfd50de3473eb2bf045285dcfd23d3aded837))
