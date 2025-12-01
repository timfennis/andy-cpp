[1mdiff --git a/Cargo.lock b/Cargo.lock[m
[1mindex 86f90f9..1c8d6c0 100644[m
[1m--- a/Cargo.lock[m
[1m+++ b/Cargo.lock[m
[36m@@ -32,13 +32,22 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "aho-corasick"[m
[31m-version = "1.1.3"[m
[32m+[m[32mversion = "1.1.4"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8e60d3430d3a69478ad0993f19238d2df97c507009a52b3c10addcd7f6bcb916"[m
[32m+[m[32mchecksum = "ddd31a130427c27518df266943a5308ed92d4b226cc639f5a8f1002816174301"[m
 dependencies = [[m
  "memchr",[m
 ][m
 [m
[32m+[m[32m[[package]][m
[32m+[m[32mname = "alloca"[m
[32m+[m[32mversion = "0.4.0"[m
[32m+[m[32msource = "registry+https://github.com/rust-lang/crates.io-index"[m
[32m+[m[32mchecksum = "e5a7d05ea6aea7e9e64d25b9156ba2fee3fdd659e34e41063cd2fc7cd020d7f4"[m
[32m+[m[32mdependencies = [[m
[32m+[m[32m "cc",[m
[32m+[m[32m][m
[32m+[m
 [[package]][m
 name = "anes"[m
 version = "0.1.6"[m
[36m@@ -47,9 +56,9 @@[m [mchecksum = "4b46cbb362ab8752921c97e041f5e366ee6297bd428a31275b9fcf1e380f7299"[m
 [m
 [[package]][m
 name = "anstream"[m
[31m-version = "0.6.18"[m
[32m+[m[32mversion = "0.6.21"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8acc5369981196006228e28809f761875c0327210a891e941f4c683b3a99529b"[m
[32m+[m[32mchecksum = "43d5b281e737544384e969a5ccad3f1cdd24b48086a0fc1b2a5262a26b8f4f4a"[m
 dependencies = [[m
  "anstyle",[m
  "anstyle-parse",[m
[36m@@ -62,26 +71,26 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "anstyle"[m
[31m-version = "1.0.10"[m
[32m+[m[32mversion = "1.0.13"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "55cc3b69f167a1ef2e161439aa98aed94e6028e5f9a59be9a6ffb47aef1651f9"[m
[32m+[m[32mchecksum = "5192cca8006f1fd4f7237516f40fa183bb07f8fbdfedaa0036de5ea9b0b45e78"[m
 [m
 [[package]][m
 name = "anstyle-parse"[m
[31m-version = "0.2.6"[m
[32m+[m[32mversion = "0.2.7"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "3b2d16507662817a6a20a9ea92df6652ee4f94f914589377d69f3b21bc5798a9"[m
[32m+[m[32mchecksum = "4e7644824f0aa2c7b9384579234ef10eb7efb6a0deb83f9630a49594dd9c15c2"[m
 dependencies = [[m
  "utf8parse",[m
 ][m
 [m
 [[package]][m
 name = "anstyle-query"[m
[31m-version = "1.1.2"[m
[32m+[m[32mversion = "1.1.5"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "79947af37f4177cfead1110013d678905c37501914fba0efea834c3fe9a8d60c"[m
[32m+[m[32mchecksum = "40c48f72fd53cd289104fc64099abca73db4166ad86ea0b4341abe65af83dadc"[m
 dependencies = [[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "windows-sys 0.61.2",[m
 ][m
 [m
 [[package]][m
[36m@@ -97,9 +106,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "anyhow"[m
[31m-version = "1.0.98"[m
[32m+[m[32mversion = "1.0.100"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "e16d2d3311acee920a9eb8d33b8cbc1787ce4a264e85f964c2404b969bdcd487"[m
[32m+[m[32mchecksum = "a23eb6b1614318a8071c9b2521f36b424b2c83db5eb3a0fead4a6c0809af6e61"[m
 [m
 [[package]][m
 name = "async-trait"[m
[36m@@ -125,9 +134,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "autocfg"[m
[31m-version = "1.4.0"[m
[32m+[m[32mversion = "1.5.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "ace50bade8e6234aa140d9a2f552bbee1db4d353f69b8217bc503490fc1a9f26"[m
[32m+[m[32mchecksum = "c08606f8c3cbf4ce6ec8e28fb0014a2c086708fe954eaa885384a6165172e7e8"[m
 [m
 [[package]][m
 name = "backtrace"[m
[36m@@ -171,9 +180,9 @@[m [mchecksum = "bef38d45163c2f1dde094a7dfd33ccf595c92905c8f8f4fdc18d06fb1037718a"[m
 [m
 [[package]][m
 name = "bitflags"[m
[31m-version = "2.9.1"[m
[32m+[m[32mversion = "2.10.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1b8e56985ec62d17e9c1001dc89c88ecd7dc08e47eba5ec7c29c7b5eeecde967"[m
[32m+[m[32mchecksum = "812e12b5285cc515a9c72a5c1d3b6d46a19dac5acfef5265968c166106e31dd3"[m
 [m
 [[package]][m
 name = "block-buffer"[m
[36m@@ -186,9 +195,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "bumpalo"[m
[31m-version = "3.17.0"[m
[32m+[m[32mversion = "3.19.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1628fb46dfa0b37568d12e5edd512553eccf6a22a78e8bde00bb4aed84d5bdbf"[m
[32m+[m[32mchecksum = "46c5e41b57b8bba42a04676d81cb89e9ee8e859a1a66f80a5a72e1cb76b34d43"[m
 [m
 [[package]][m
 name = "bytes"[m
[36m@@ -202,11 +211,21 @@[m [mversion = "0.3.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
 checksum = "37b2a672a2cb129a2e41c10b1224bb368f9f37a2b16b612598138befd7b37eb5"[m
 [m
[32m+[m[32m[[package]][m
[32m+[m[32mname = "cc"[m
[32m+[m[32mversion = "1.2.48"[m
[32m+[m[32msource = "registry+https://github.com/rust-lang/crates.io-index"[m
[32m+[m[32mchecksum = "c481bdbf0ed3b892f6f806287d72acd515b352a4ec27a208489b8c1bc839633a"[m
[32m+[m[32mdependencies = [[m
[32m+[m[32m "find-msvc-tools",[m
[32m+[m[32m "shlex",[m
[32m+[m[32m][m
[32m+[m
 [[package]][m
 name = "cfg-if"[m
[31m-version = "1.0.0"[m
[32m+[m[32mversion = "1.0.4"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "baf1de4339761588bc0619e3cbc0120ee582ebb74b53b4efbf79117bd2da40fd"[m
[32m+[m[32mchecksum = "9330f8b2ff13f34540b44e946ef35111825727b38d33286ef986142615121801"[m
 [m
 [[package]][m
 name = "cfg_aliases"[m
[36m@@ -243,9 +262,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "clap"[m
[31m-version = "4.5.39"[m
[32m+[m[32mversion = "4.5.53"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "fd60e63e9be68e5fb56422e397cf9baddded06dae1d2e523401542383bc72a9f"[m
[32m+[m[32mchecksum = "c9e340e012a1bf4935f5282ed1436d1489548e8f72308207ea5df0e23d2d03f8"[m
 dependencies = [[m
  "clap_builder",[m
  "clap_derive",[m
[36m@@ -253,9 +272,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "clap_builder"[m
[31m-version = "4.5.39"[m
[32m+[m[32mversion = "4.5.53"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "89cc6392a1f72bbeb820d71f32108f61fdaf18bc526e1d23954168a67759ef51"[m
[32m+[m[32mchecksum = "d76b5d13eaa18c901fd2f7fca939fefe3a0727a953561fefdf3b2922b8569d00"[m
 dependencies = [[m
  "anstream",[m
  "anstyle",[m
[36m@@ -265,9 +284,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "clap_derive"[m
[31m-version = "4.5.32"[m
[32m+[m[32mversion = "4.5.49"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "09176aae279615badda0765c0c0b3f6ed53f4709118af73cf4655d85d1530cd7"[m
[32m+[m[32mchecksum = "2a0b5487afeab2deb2ff4e03a807ad1a03ac532ff5a2cee5d86884440c7f7671"[m
 dependencies = [[m
  "heck",[m
  "proc-macro2",[m
[36m@@ -277,9 +296,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "clap_lex"[m
[31m-version = "0.7.4"[m
[32m+[m[32mversion = "0.7.6"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "f46ad14479a25103f283c0f10005961cf086d8dc42205bb44c46ac563475dca6"[m
[32m+[m[32mchecksum = "a1d728cc89cf3aee9ff92b05e62b19ee65a02b5702cff7d5a377e32c6ae29d8d"[m
 [m
 [[package]][m
 name = "clipboard-win"[m
[36m@@ -292,9 +311,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "colorchoice"[m
[31m-version = "1.0.3"[m
[32m+[m[32mversion = "1.0.4"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "5b63caa9aa9397e2d9480a9b13673856c78d8ac123288526c37d7839f2a86990"[m
[32m+[m[32mchecksum = "b05b61dc5112cbb17e4b6cd61790d9845d13888356391624cbe7e41efeac1e75"[m
 [m
 [[package]][m
 name = "convert_case"[m
[36m@@ -325,10 +344,11 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "criterion"[m
[31m-version = "0.6.0"[m
[32m+[m[32mversion = "0.8.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "3bf7af66b0989381bd0be551bd7cc91912a655a58c6918420c9527b1fd8b4679"[m
[32m+[m[32mchecksum = "a0dfe5e9e71bdcf4e4954f7d14da74d1cdb92a3a07686452d1509652684b1aab"[m
 dependencies = [[m
[32m+[m[32m "alloca",[m
  "anes",[m
  "cast",[m
  "ciborium",[m
[36m@@ -337,6 +357,7 @@[m [mdependencies = [[m
  "itertools 0.13.0",[m
  "num-traits",[m
  "oorandom",[m
[32m+[m[32m "page_size",[m
  "plotters",[m
  "rayon",[m
  "regex",[m
[36m@@ -348,19 +369,19 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "criterion-plot"[m
[31m-version = "0.5.0"[m
[32m+[m[32mversion = "0.8.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "6b50826342786a51a89e2da3a28f1c32b06e387201bc2d19791f622c673706b1"[m
[32m+[m[32mchecksum = "5de36c2bee19fba779808f92bf5d9b0fa5a40095c277aba10c458a12b35d21d6"[m
 dependencies = [[m
  "cast",[m
[31m- "itertools 0.10.5",[m
[32m+[m[32m "itertools 0.13.0",[m
 ][m
 [m
 [[package]][m
 name = "crokey"[m
[31m-version = "1.2.0"[m
[32m+[m[32mversion = "1.3.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "5282b45c96c5978c8723ea83385cb9a488b64b7d175733f48d07bf9da514a863"[m
[32m+[m[32mchecksum = "51360853ebbeb3df20c76c82aecf43d387a62860f1a59ba65ab51f00eea85aad"[m
 dependencies = [[m
  "crokey-proc_macros",[m
  "crossterm",[m
[36m@@ -371,9 +392,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "crokey-proc_macros"[m
[31m-version = "1.2.0"[m
[32m+[m[32mversion = "1.3.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "2ea0218d3fedf0797fa55676f1964ef5d27103d41ed0281b4bbd2a6e6c3d8d28"[m
[32m+[m[32mchecksum = "3bf1a727caeb5ee5e0a0826a97f205a9cf84ee964b0b48239fef5214a00ae439"[m
 dependencies = [[m
  "crossterm",[m
  "proc-macro2",[m
[36m@@ -444,7 +465,7 @@[m [mversion = "0.29.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
 checksum = "d8b9f2e4c67f833b660cdb0a3523065869fb35570177239812ed4c905aeff87b"[m
 dependencies = [[m
[31m- "bitflags 2.9.1",[m
[32m+[m[32m "bitflags 2.10.0",[m
  "crossterm_winapi",[m
  "derive_more",[m
  "document-features",[m
[36m@@ -605,9 +626,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "document-features"[m
[31m-version = "0.2.11"[m
[32m+[m[32mversion = "0.2.12"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "95249b50c6c185bee49034bcb378a49dc2b5dff0be90ff6616d31d64febab05d"[m
[32m+[m[32mchecksum = "d4b8a88685455ed29a21542a33abd9cb6510b6b129abadabdcef0f4c55bc8f61"[m
 dependencies = [[m
  "litrs",[m
 ][m
[36m@@ -626,12 +647,12 @@[m [mchecksum = "c34f04666d835ff5d62e058c3995147c06f42fe86ff053337632bca83e42702d"[m
 [m
 [[package]][m
 name = "errno"[m
[31m-version = "0.3.12"[m
[32m+[m[32mversion = "0.3.14"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "cea14ef9355e3beab063703aa9dab15afd25f0667c341310c1e5274bb1d0da18"[m
[32m+[m[32mchecksum = "39cab71617ae0d63f51a36d69f866391735b51691dbda63cf6f96d042b63efeb"[m
 dependencies = [[m
  "libc",[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "windows-sys 0.61.2",[m
 ][m
 [m
 [[package]][m
[36m@@ -661,6 +682,12 @@[m [mdependencies = [[m
  "windows-sys 0.59.0",[m
 ][m
 [m
[32m+[m[32m[[package]][m
[32m+[m[32mname = "find-msvc-tools"[m
[32m+[m[32mversion = "0.1.5"[m
[32m+[m[32msource = "registry+https://github.com/rust-lang/crates.io-index"[m
[32m+[m[32mchecksum = "3a3076410a55c90011c298b04d0cfa770b00fa04e1e3c97d3f6c9de105a03844"[m
[32m+[m
 [[package]][m
 name = "fnv"[m
 version = "1.0.7"[m
[36m@@ -765,14 +792,14 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "getrandom"[m
[31m-version = "0.3.3"[m
[32m+[m[32mversion = "0.3.4"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "26145e563e54f2cadc477553f1ec5ee650b00862f0a58bcd12cbdc5f0ea2d2f4"[m
[32m+[m[32mchecksum = "899def5c37c4fd7b2664648c28120ecec138e4d395b459e5ca34f9cce2dd77fd"[m
 dependencies = [[m
  "cfg-if",[m
  "libc",[m
  "r-efi",[m
[31m- "wasi 0.14.2+wasi-0.2.4",[m
[32m+[m[32m "wasip2",[m
 ][m
 [m
 [[package]][m
[36m@@ -783,12 +810,13 @@[m [mchecksum = "07e28edb80900c19c28f1072f2e8aeca7fa06b23cd4169cefe1af5aa3260783f"[m
 [m
 [[package]][m
 name = "half"[m
[31m-version = "2.4.1"[m
[32m+[m[32mversion = "2.7.1"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "6dd08c532ae367adf81c312a4580bc67f1d0fe8bc9c460520283f4c0ff277888"[m
[32m+[m[32mchecksum = "6ea2d84b969582b4b1864a92dc5d27cd2b77b622a8d79306834f1be5ba20d84b"[m
 dependencies = [[m
  "cfg-if",[m
  "crunchy",[m
[32m+[m[32m "zerocopy",[m
 ][m
 [m
 [[package]][m
[36m@@ -811,11 +839,11 @@[m [mchecksum = "2304e00983f87ffb38b55b444b5e3b60a884b5d30c0fca7d82fe33449bbe55ea"[m
 [m
 [[package]][m
 name = "home"[m
[31m-version = "0.5.11"[m
[32m+[m[32mversion = "0.5.12"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "589533453244b0995c858700322199b2becb13b627df2851f64a2775d024abcf"[m
[32m+[m[32mchecksum = "cc627f471c528ff0c4a49e1d5e60450c8f6461dd6d10ba9dcd3a61d3dff7728d"[m
 dependencies = [[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "windows-sys 0.61.2",[m
 ][m
 [m
 [[package]][m
[36m@@ -940,18 +968,9 @@[m [mchecksum = "7655c9839580ee829dfacba1d1278c2b7883e50a277ff7541299489d6bdfdc45"[m
 [m
 [[package]][m
 name = "is_terminal_polyfill"[m
[31m-version = "1.70.1"[m
[32m+[m[32mversion = "1.70.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "7943c866cc5cd64cbc25b2e01621d07fa8eb2a1a23160ee81ce38704e97b8ecf"[m
[31m-[m
[31m-[[package]][m
[31m-name = "itertools"[m
[31m-version = "0.10.5"[m
[31m-source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "b0fd2260e829bddf4cb6ea802289de2f86d6a7a690192fbe91b3f46e0f2c8473"[m
[31m-dependencies = [[m
[31m- "either",[m
[31m-][m
[32m+[m[32mchecksum = "a6cb138bb79a146c1bd460005623e142ef0181e3d0219cb493e02f7d08a35695"[m
 [m
 [[package]][m
 name = "itertools"[m
[36m@@ -979,9 +998,9 @@[m [mchecksum = "4a5f13b858c8d314ee3e8f639011f7ccefe71f97f96e50151fb991f267928e2c"[m
 [m
 [[package]][m
 name = "js-sys"[m
[31m-version = "0.3.77"[m
[32m+[m[32mversion = "0.3.79"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1cfaf33c695fc6e08064efbc1f72ec937429614f25eef83af942d0e227c3a28f"[m
[32m+[m[32mchecksum = "6247da8b8658ad4e73a186e747fcc5fc2a29f979d6fe6269127fdb5fd08298d0"[m
 dependencies = [[m
  "once_cell",[m
  "wasm-bindgen",[m
[36m@@ -989,9 +1008,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "lazy-regex"[m
[31m-version = "3.4.1"[m
[32m+[m[32mversion = "3.4.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "60c7310b93682b36b98fa7ea4de998d3463ccbebd94d935d6b48ba5b6ffa7126"[m
[32m+[m[32mchecksum = "191898e17ddee19e60bccb3945aa02339e81edd4a8c50e21fd4d48cdecda7b29"[m
 dependencies = [[m
  "lazy-regex-proc_macros",[m
  "once_cell",[m
[36m@@ -1000,9 +1019,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "lazy-regex-proc_macros"[m
[31m-version = "3.4.1"[m
[32m+[m[32mversion = "3.4.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "4ba01db5ef81e17eb10a5e0f2109d1b3a3e29bac3070fdbd7d156bf7dbd206a1"[m
[32m+[m[32mchecksum = "c35dc8b0da83d1a9507e12122c80dea71a9c7c613014347392483a83ea593e04"[m
 dependencies = [[m
  "proc-macro2",[m
  "quote",[m
[36m@@ -1012,15 +1031,15 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "libc"[m
[31m-version = "0.2.172"[m
[32m+[m[32mversion = "0.2.177"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "d750af042f7ef4f724306de029d18836c26c1765a54a6a3f094cbd23a7267ffa"[m
[32m+[m[32mchecksum = "2874a2af47a2325c2001a6e6fad9b16a53b802102b528163885171cf92b15976"[m
 [m
 [[package]][m
 name = "linux-raw-sys"[m
[31m-version = "0.9.4"[m
[32m+[m[32mversion = "0.11.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "cd945864f07fe9f5371a27ad7b52a172b4b499999f1d97574c9fa68373937e12"[m
[32m+[m[32mchecksum = "df1d3c3b53da64cf5760482273a98e575c651a67eec7f77df96b5b642de8f039"[m
 [m
 [[package]][m
 name = "litemap"[m
[36m@@ -1030,25 +1049,24 @@[m [mchecksum = "6373607a59f0be73a39b6fe456b8192fcc3585f602af20751600e974dd455e77"[m
 [m
 [[package]][m
 name = "litrs"[m
[31m-version = "0.4.1"[m
[32m+[m[32mversion = "1.0.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "b4ce301924b7887e9d637144fdade93f9dfff9b60981d4ac161db09720d39aa5"[m
[32m+[m[32mchecksum = "11d3d7f243d5c5a8b9bb5d6dd2b1602c0cb0b9db1621bafc7ed66e35ff9fe092"[m
 [m
 [[package]][m
 name = "lock_api"[m
[31m-version = "0.4.12"[m
[32m+[m[32mversion = "0.4.14"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "07af8b9cdd281b7915f413fa73f29ebd5d55d0d3f0155584dade1ff18cea1b17"[m
[32m+[m[32mchecksum = "224399e74b87b5f3557511d98dff8b14089b3dadafcab6bb93eab67d3aace965"[m
 dependencies = [[m
[31m- "autocfg",[m
  "scopeguard",[m
 ][m
 [m
 [[package]][m
 name = "log"[m
[31m-version = "0.4.27"[m
[32m+[m[32mversion = "0.4.28"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "13dc2df351e3202783a1fe0d44375f7295ffb4049267b0f3018346dc122a1d94"[m
[32m+[m[32mchecksum = "34080505efa8e45a4b816c349525ebe327ceaa8559756f0356cba97ef3bf7432"[m
 [m
 [[package]][m
 name = "lsp-types"[m
[36m@@ -1065,15 +1083,15 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "md5"[m
[31m-version = "0.7.0"[m
[32m+[m[32mversion = "0.8.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "490cc448043f947bae3cbee9c203358d62dbee0db12107a74be5c30ccfd09771"[m
[32m+[m[32mchecksum = "ae960838283323069879657ca3de837e9f7bbb4c7bf6ea7f1b290d5e9476d2e0"[m
 [m
 [[package]][m
 name = "memchr"[m
[31m-version = "2.7.4"[m
[32m+[m[32mversion = "2.7.6"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "78ca9ab1a0babb1e7d5695e3530886289c18cf2f87ec19a575a0abdce112e3a3"[m
[32m+[m[32mchecksum = "f52b00d39961fc5b2736ea853c9cc86238e165017a493d1d5c8eac6bdc4cc273"[m
 [m
 [[package]][m
 name = "miette"[m
[36m@@ -1107,9 +1125,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "minimad"[m
[31m-version = "0.13.1"[m
[32m+[m[32mversion = "0.14.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "a9c5d708226d186590a7b6d4a9780e2bdda5f689e0d58cd17012a298efd745d2"[m
[32m+[m[32mchecksum = "df8b688969b16915f3ecadc7829d5b7779dee4977e503f767f34136803d5c06f"[m
 dependencies = [[m
  "once_cell",[m
 ][m
[36m@@ -1125,14 +1143,14 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "mio"[m
[31m-version = "1.0.4"[m
[32m+[m[32mversion = "1.1.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "78bed444cc8a2160f01cbcf811ef18cac863ad68ae8ca62092e8db51d51c761c"[m
[32m+[m[32mchecksum = "69d83b0086dc8ecf3ce9ae2874b2d1290252e2a30720bea58a5c6639b0092873"[m
 dependencies = [[m
  "libc",[m
  "log",[m
[31m- "wasi 0.11.0+wasi-snapshot-preview1",[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "wasi",[m
[32m+[m[32m "windows-sys 0.61.2",[m
 ][m
 [m
 [[package]][m
[36m@@ -1213,7 +1231,7 @@[m [mversion = "0.30.1"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
 checksum = "74523f3a35e05aba87a1d978330aef40f67b0304ac79c1c00b294c9830543db6"[m
 dependencies = [[m
[31m- "bitflags 2.9.1",[m
[32m+[m[32m "bitflags 2.10.0",[m
  "cfg-if",[m
  "cfg_aliases",[m
  "libc",[m
[36m@@ -1315,24 +1333,34 @@[m [mchecksum = "b410bbe7e14ab526a0e86877eb47c6996a2bd7746f027ba551028c925390e4e9"[m
 [m
 [[package]][m
 name = "ordered-float"[m
[31m-version = "5.0.0"[m
[32m+[m[32mversion = "5.1.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "e2c1f9f56e534ac6a9b8a4600bdf0f530fb393b5f393e7b4d03489c3cf0c3f01"[m
[32m+[m[32mchecksum = "7f4779c6901a562440c3786d08192c6fbda7c1c2060edd10006b05ee35d10f2d"[m
 dependencies = [[m
  "num-traits",[m
 ][m
 [m
 [[package]][m
 name = "owo-colors"[m
[31m-version = "4.2.1"[m
[32m+[m[32mversion = "4.2.3"[m
[32m+[m[32msource = "registry+https://github.com/rust-lang/crates.io-index"[m
[32m+[m[32mchecksum = "9c6901729fa79e91a0913333229e9ca5dc725089d1c363b2f4b4760709dc4a52"[m
[32m+[m
[32m+[m[32m[[package]][m
[32m+[m[32mname = "page_size"[m
[32m+[m[32mversion = "0.6.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "26995317201fa17f3656c36716aed4a7c81743a9634ac4c99c0eeda495db0cec"[m
[32m+[m[32mchecksum = "30d5b2194ed13191c1999ae0704b7839fb18384fa22e49b57eeaa97d79ce40da"[m
[32m+[m[32mdependencies = [[m
[32m+[m[32m "libc",[m
[32m+[m[32m "winapi",[m
[32m+[m[32m][m
 [m
 [[package]][m
 name = "parking_lot"[m
[31m-version = "0.12.3"[m
[32m+[m[32mversion = "0.12.5"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "f1bf18183cf54e8d6059647fc3063646a1801cf30896933ec2311622cc4b9a27"[m
[32m+[m[32mchecksum = "93857453250e3077bd71ff98b6a65ea6621a19bb0f559a85248955ac12c45a1a"[m
 dependencies = [[m
  "lock_api",[m
  "parking_lot_core",[m
[36m@@ -1340,15 +1368,15 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "parking_lot_core"[m
[31m-version = "0.9.10"[m
[32m+[m[32mversion = "0.9.12"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1e401f977ab385c9e4e3ab30627d6f26d00e2c73eef317493c4ec6d468726cf8"[m
[32m+[m[32mchecksum = "2621685985a2ebf1c516881c026032ac7deafcda1a2c9b7850dc81e3dfcb64c1"[m
 dependencies = [[m
  "cfg-if",[m
  "libc",[m
  "redox_syscall",[m
  "smallvec",[m
[31m- "windows-targets 0.52.6",[m
[32m+[m[32m "windows-link",[m
 ][m
 [m
 [[package]][m
[36m@@ -1463,27 +1491,27 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "proc-macro2"[m
[31m-version = "1.0.95"[m
[32m+[m[32mversion = "1.0.103"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "02b3e5e68a3a1a02aad3ec490a98007cbc13c37cbe84a3cd7b8e406d76e7f778"[m
[32m+[m[32mchecksum = "5ee95bc4ef87b8d5ba32e8b7714ccc834865276eab0aed5c9958d00ec45f49e8"[m
 dependencies = [[m
  "unicode-ident",[m
 ][m
 [m
 [[package]][m
 name = "quote"[m
[31m-version = "1.0.40"[m
[32m+[m[32mversion = "1.0.42"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1885c039570dc00dcb4ff087a89e185fd56bae234ddc7f056a945bf36467248d"[m
[32m+[m[32mchecksum = "a338cc41d27e6cc6dce6cefc13a0729dfbb81c262b1f519331575dd80ef3067f"[m
 dependencies = [[m
  "proc-macro2",[m
 ][m
 [m
 [[package]][m
 name = "r-efi"[m
[31m-version = "5.2.0"[m
[32m+[m[32mversion = "5.3.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "74765f6d916ee2faa39bc8e68e4f3ed8949b48cccdac59983d287a7cb71ce9c5"[m
[32m+[m[32mchecksum = "69cdb34c158ceb288df11e18b4bd39de994f6657d83847bdffdbd7f346754b0f"[m
 [m
 [[package]][m
 name = "radix_trie"[m
[36m@@ -1497,9 +1525,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "rand"[m
[31m-version = "0.9.1"[m
[32m+[m[32mversion = "0.9.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "9fbfd9d094a40bf3ae768db9361049ace4c0e04a4fd6b359518bd7b73a73dd97"[m
[32m+[m[32mchecksum = "6db2770f06117d490610c7488547d543617b21bfa07796d7a12f6f1bd53850d1"[m
 dependencies = [[m
  "rand_chacha",[m
  "rand_core",[m
[36m@@ -1526,9 +1554,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "rayon"[m
[31m-version = "1.10.0"[m
[32m+[m[32mversion = "1.11.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "b418a60154510ca1a002a752ca9714984e21e4241e804d32555251faf8b78ffa"[m
[32m+[m[32mchecksum = "368f01d005bf8fd9b1206fb6fa653e6c4a81ceb1466406b81792d87c5677a58f"[m
 dependencies = [[m
  "either",[m
  "rayon-core",[m
[36m@@ -1536,9 +1564,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "rayon-core"[m
[31m-version = "1.12.1"[m
[32m+[m[32mversion = "1.13.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1465873a3dfdaa8ae7cb14b4383657caab0b3e8a0aa9ae8e04b044854c8dfce2"[m
[32m+[m[32mchecksum = "22e18b0f0062d30d4230b2e85ff77fdfe4326feb054b9783a3460d8435c8ab91"[m
 dependencies = [[m
  "crossbeam-deque",[m
  "crossbeam-utils",[m
[36m@@ -1546,18 +1574,18 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "redox_syscall"[m
[31m-version = "0.5.12"[m
[32m+[m[32mversion = "0.5.17"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "928fca9cf2aa042393a8325b9ead81d2f0df4cb12e1e24cef072922ccd99c5af"[m
[32m+[m[32mchecksum = "5407465600fb0548f1442edf71dd20683c6ed326200ace4b1ef0763521bb3b77"[m
 dependencies = [[m
[31m- "bitflags 2.9.1",[m
[32m+[m[32m "bitflags 2.10.0",[m
 ][m
 [m
 [[package]][m
 name = "regex"[m
[31m-version = "1.11.1"[m
[32m+[m[32mversion = "1.12.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "b544ef1b4eac5dc2db33ea63606ae9ffcfac26c1416a2806ae0bf5f56b201191"[m
[32m+[m[32mchecksum = "843bc0191f75f3e22651ae5f1e72939ab2f72a4bc30fa80a066bd66edefc24d4"[m
 dependencies = [[m
  "aho-corasick",[m
  "memchr",[m
[36m@@ -1567,9 +1595,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "regex-automata"[m
[31m-version = "0.4.9"[m
[32m+[m[32mversion = "0.4.13"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "809e8dc61f6de73b46c85f4c96486310fe304c434cfa43669d7b40f711150908"[m
[32m+[m[32mchecksum = "5276caf25ac86c8d810222b3dbb938e512c55c6831a10f3e6ed1c93b84041f1c"[m
 dependencies = [[m
  "aho-corasick",[m
  "memchr",[m
[36m@@ -1578,9 +1606,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "regex-syntax"[m
[31m-version = "0.8.5"[m
[32m+[m[32mversion = "0.8.8"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "2b15c43186be67a4fd63bee50d0303afffcef381492ebe2c5d87f324e1b8815c"[m
[32m+[m[32mchecksum = "7a2d987857b319362043e95f5353c0535c1f58eec5336fdfcf626430af7def58"[m
 [m
 [[package]][m
 name = "rustc-demangle"[m
[36m@@ -1590,30 +1618,30 @@[m [mchecksum = "719b953e2095829ee67db738b3bfa9fa368c94900df327b3f07fe6e794d2fe1f"[m
 [m
 [[package]][m
 name = "rustix"[m
[31m-version = "1.0.7"[m
[32m+[m[32mversion = "1.1.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "c71e83d6afe7ff64890ec6b71d6a69bb8a610ab78ce364b3352876bb4c801266"[m
[32m+[m[32mchecksum = "cd15f8a2c5551a84d56efdc1cd049089e409ac19a3072d5037a17fd70719ff3e"[m
 dependencies = [[m
[31m- "bitflags 2.9.1",[m
[32m+[m[32m "bitflags 2.10.0",[m
  "errno",[m
  "libc",[m
  "linux-raw-sys",[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "windows-sys 0.61.2",[m
 ][m
 [m
 [[package]][m
 name = "rustversion"[m
[31m-version = "1.0.21"[m
[32m+[m[32mversion = "1.0.22"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8a0d197bd2c9dc6e53b84da9556a69ba4cdfab8619eb41a8bd1cc2027a0f6b1d"[m
[32m+[m[32mchecksum = "b39cdef0fa800fc44525c84ccb54a029961a8215f9619753635a9c0d2538d46d"[m
 [m
 [[package]][m
 name = "rustyline"[m
[31m-version = "16.0.0"[m
[32m+[m[32mversion = "17.0.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "62fd9ca5ebc709e8535e8ef7c658eb51457987e48c98ead2be482172accc408d"[m
[32m+[m[32mchecksum = "e902948a25149d50edc1a8e0141aad50f54e22ba83ff988cf8f7c9ef07f50564"[m
 dependencies = [[m
[31m- "bitflags 2.9.1",[m
[32m+[m[32m "bitflags 2.10.0",[m
  "cfg-if",[m
  "clipboard-win",[m
  "fd-lock",[m
[36m@@ -1625,9 +1653,9 @@[m [mdependencies = [[m
  "radix_trie",[m
  "rustyline-derive",[m
  "unicode-segmentation",[m
[31m- "unicode-width 0.2.0",[m
[32m+[m[32m "unicode-width 0.2.2",[m
  "utf8parse",[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "windows-sys 0.60.2",[m
 ][m
 [m
 [[package]][m
[36m@@ -1664,24 +1692,34 @@[m [mchecksum = "94143f37725109f92c262ed2cf5e59bce7498c01bcc1502d7b9afe439a4e9f49"[m
 [m
 [[package]][m
 name = "self_cell"[m
[31m-version = "1.2.0"[m
[32m+[m[32mversion = "1.2.1"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "0f7d95a54511e0c7be3f51e8867aa8cf35148d7b9445d44de2f943e2b206e749"[m
[32m+[m[32mchecksum = "16c2f82143577edb4921b71ede051dac62ca3c16084e918bf7b40c96ae10eb33"[m
 [m
 [[package]][m
 name = "serde"[m
[31m-version = "1.0.219"[m
[32m+[m[32mversion = "1.0.228"[m
[32m+[m[32msource = "registry+https://github.com/rust-lang/crates.io-index"[m
[32m+[m[32mchecksum = "9a8e94ea7f378bd32cbbd37198a4a91436180c5bb472411e48b5ec2e2124ae9e"[m
[32m+[m[32mdependencies = [[m
[32m+[m[32m "serde_core",[m
[32m+[m[32m "serde_derive",[m
[32m+[m[32m][m
[32m+[m
[32m+[m[32m[[package]][m
[32m+[m[32mname = "serde_core"[m
[32m+[m[32mversion = "1.0.228"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "5f0e2c6ed6606019b4e29e69dbaba95b11854410e5347d525002456dbbb786b6"[m
[32m+[m[32mchecksum = "41d385c7d4ca58e59fc732af25c3983b67ac852c1a25000afe1175de458b67ad"[m
 dependencies = [[m
  "serde_derive",[m
 ][m
 [m
 [[package]][m
 name = "serde_derive"[m
[31m-version = "1.0.219"[m
[32m+[m[32mversion = "1.0.228"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "5b0276cf7f2c73365f7157c8123c21cd9a50fbbd844757af28ca1f5925fc2a00"[m
[32m+[m[32mchecksum = "d540f220d3187173da220f885ab66608367b6574e925011a9353e4badda91d79"[m
 dependencies = [[m
  "proc-macro2",[m
  "quote",[m
[36m@@ -1690,14 +1728,15 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "serde_json"[m
[31m-version = "1.0.140"[m
[32m+[m[32mversion = "1.0.145"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "20068b6e96dc6c9bd23e01df8827e6c7e1f2fddd43c21810382803c136b99373"[m
[32m+[m[32mchecksum = "402a6f66d8c709116cf22f558eab210f5a50187f702eb4d7e5ef38d9a7f1c79c"[m
 dependencies = [[m
  "itoa",[m
  "memchr",[m
  "ryu",[m
  "serde",[m
[32m+[m[32m "serde_core",[m
 ][m
 [m
 [[package]][m
[36m@@ -1722,11 +1761,17 @@[m [mdependencies = [[m
  "digest",[m
 ][m
 [m
[32m+[m[32m[[package]][m
[32m+[m[32mname = "shlex"[m
[32m+[m[32mversion = "1.3.0"[m
[32m+[m[32msource = "registry+https://github.com/rust-lang/crates.io-index"[m
[32m+[m[32mchecksum = "0fda2ff0d084019ba4d7c6f371c95d8fd75ce3524c3cb8fb653a3023f6323e64"[m
[32m+[m
 [[package]][m
 name = "signal-hook"[m
[31m-version = "0.3.17"[m
[32m+[m[32mversion = "0.3.18"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8621587d4798caf8eb44879d42e56b9a93ea5dcd315a6487c357130095b62801"[m
[32m+[m[32mchecksum = "d881a16cf4426aa584979d30bd82cb33429027e42122b169753d6ef1085ed6e2"[m
 dependencies = [[m
  "libc",[m
  "signal-hook-registry",[m
[36m@@ -1734,9 +1779,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "signal-hook-mio"[m
[31m-version = "0.2.4"[m
[32m+[m[32mversion = "0.2.5"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "34db1a06d485c9142248b7a054f034b349b212551f3dfd19c94d45a754a217cd"[m
[32m+[m[32mchecksum = "b75a19a7a740b25bc7944bdee6172368f988763b744e3d4dfe753f6b4ece40cc"[m
 dependencies = [[m
  "libc",[m
  "mio",[m
[36m@@ -1745,9 +1790,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "signal-hook-registry"[m
[31m-version = "1.4.5"[m
[32m+[m[32mversion = "1.4.7"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "9203b8055f63a2a00e2f593bb0510367fe707d7ff1e5c872de2f537b339e5410"[m
[32m+[m[32mchecksum = "7664a098b8e616bdfcc2dc0e9ac44eb231eedf41db4e9fe95d8d32ec728dedad"[m
 dependencies = [[m
  "libc",[m
 ][m
[36m@@ -1760,9 +1805,9 @@[m [mchecksum = "7a2ae44ef20feb57a68b23d846850f861394c2e02dc425a50098ae8c90267589"[m
 [m
 [[package]][m
 name = "smallvec"[m
[31m-version = "1.15.0"[m
[32m+[m[32mversion = "1.15.1"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8917285742e9f3e1683f0a9c4e6b57960b7314d0b08d30d1ecd426713ee2eee9"[m
[32m+[m[32mchecksum = "67b1b7a3b5fe4f1376887184045fcf45c69e92af734b7aaddc05fb777b6fbd03"[m
 [m
 [[package]][m
 name = "socket2"[m
[36m@@ -1815,9 +1860,9 @@[m [mchecksum = "b7401a30af6cb5818bb64852270bb722533397edcfc7344954a38f420819ece2"[m
 [m
 [[package]][m
 name = "syn"[m
[31m-version = "2.0.101"[m
[32m+[m[32mversion = "2.0.111"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8ce2b7fc941b3a24138a0a7cf8e858bfc6a992e7978a068a5c760deb0ed43caf"[m
[32m+[m[32mchecksum = "390cc9a294ab71bdb1aa2e99d13be9c753cd2d7bd6560c77118597410c4d2e87"[m
 dependencies = [[m
  "proc-macro2",[m
  "quote",[m
[36m@@ -1843,9 +1888,9 @@[m [mchecksum = "55937e1799185b12863d447f42597ed69d9928686b8d88a1df17376a097d8369"[m
 [m
 [[package]][m
 name = "termimad"[m
[31m-version = "0.33.0"[m
[32m+[m[32mversion = "0.34.1"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "b23646cdde4c2e9d6f2621424751b48653e5755f50163b2ff5ce3b32ebf17104"[m
[32m+[m[32mchecksum = "889a9370996b74cf46016ce35b96c248a9ac36d69aab1d112b3e09bc33affa49"[m
 dependencies = [[m
  "coolor",[m
  "crokey",[m
[36m@@ -1882,23 +1927,23 @@[m [msource = "registry+https://github.com/rust-lang/crates.io-index"[m
 checksum = "c13547615a44dc9c452a8a534638acdf07120d4b6847c8178705da06306a3057"[m
 dependencies = [[m
  "unicode-linebreak",[m
[31m- "unicode-width 0.2.0",[m
[32m+[m[32m "unicode-width 0.2.2",[m
 ][m
 [m
 [[package]][m
 name = "thiserror"[m
[31m-version = "2.0.12"[m
[32m+[m[32mversion = "2.0.17"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "567b8a2dae586314f7be2a752ec7474332959c6460e02bde30d702a66d488708"[m
[32m+[m[32mchecksum = "f63587ca0f12b72a0600bcba1d40081f830876000bb46dd2337a3051618f4fc8"[m
 dependencies = [[m
  "thiserror-impl",[m
 ][m
 [m
 [[package]][m
 name = "thiserror-impl"[m
[31m-version = "2.0.12"[m
[32m+[m[32mversion = "2.0.17"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "7f7cf42b4507d8ea322120659672cf1b9dbb93f8f2d4ecfd6e51350ff5b17a1d"[m
[32m+[m[32mchecksum = "3ff15c8ecd7de3849db632e14d18d2571fa09dfc5ed93479bc4485c7a517c913"[m
 dependencies = [[m
  "proc-macro2",[m
  "quote",[m
[36m@@ -2065,9 +2110,9 @@[m [mchecksum = "1dccffe3ce07af9386bfd29e80c0ab1a8205a2fc34e4bcd40364df902cfa8f3f"[m
 [m
 [[package]][m
 name = "unicode-ident"[m
[31m-version = "1.0.18"[m
[32m+[m[32mversion = "1.0.22"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "5a5f39404a5da50712a4c1eecf25e90dd62b613502b7e925fd4e4d19b5c96512"[m
[32m+[m[32mchecksum = "9312f7c4f6ff9069b165498234ce8be658059c6728633667c526e27dc2cf1df5"[m
 [m
 [[package]][m
 name = "unicode-linebreak"[m
[36m@@ -2089,9 +2134,9 @@[m [mchecksum = "7dd6e30e90baa6f72411720665d41d89b9a3d039dc45b8faea1ddd07f617f6af"[m
 [m
 [[package]][m
 name = "unicode-width"[m
[31m-version = "0.2.0"[m
[32m+[m[32mversion = "0.2.2"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1fc81956842c57dac11422a97c3b8195a1ff727f06e85c84ed2e8aa277c9a0fd"[m
[32m+[m[32mchecksum = "b4ac048d71ede7ee76d585517add45da530660ef4390e49b098733c6e897f254"[m
 [m
 [[package]][m
 name = "unicode-xid"[m
[36m@@ -2141,36 +2186,37 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "wasi"[m
[31m-version = "0.11.0+wasi-snapshot-preview1"[m
[32m+[m[32mversion = "0.11.1+wasi-snapshot-preview1"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "9c8d87e72b64a3b4db28d11ce29237c246188f4f51057d65a7eab63b7987e423"[m
[32m+[m[32mchecksum = "ccf3ec651a847eb01de73ccad15eb7d99f80485de043efb2f370cd654f4ea44b"[m
 [m
 [[package]][m
[31m-name = "wasi"[m
[31m-version = "0.14.2+wasi-0.2.4"[m
[32m+[m[32mname = "wasip2"[m
[32m+[m[32mversion = "1.0.1+wasi-0.2.4"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "9683f9a5a998d873c0d21fcbe3c083009670149a8fab228644b8bd36b2c48cb3"[m
[32m+[m[32mchecksum = "0562428422c63773dad2c345a1882263bbf4d65cf3f42e90921f787ef5ad58e7"[m
 dependencies = [[m
[31m- "wit-bindgen-rt",[m
[32m+[m[32m "wit-bindgen",[m
 ][m
 [m
 [[package]][m
 name = "wasm-bindgen"[m
[31m-version = "0.2.100"[m
[32m+[m[32mversion = "0.2.102"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1edc8929d7499fc4e8f0be2262a241556cfc54a0bea223790e71446f2aab1ef5"[m
[32m+[m[32mchecksum = "4ad224d2776649cfb4f4471124f8176e54c1cca67a88108e30a0cd98b90e7ad3"[m
 dependencies = [[m
  "cfg-if",[m
  "once_cell",[m
  "rustversion",[m
  "wasm-bindgen-macro",[m
[32m+[m[32m "wasm-bindgen-shared",[m
 ][m
 [m
 [[package]][m
 name = "wasm-bindgen-backend"[m
[31m-version = "0.2.100"[m
[32m+[m[32mversion = "0.2.102"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "2f0a0651a5c2bc21487bde11ee802ccaf4c51935d0d3d42a6101f98161700bc6"[m
[32m+[m[32mchecksum = "3a1364104bdcd3c03f22b16a3b1c9620891469f5e9f09bc38b2db121e593e732"[m
 dependencies = [[m
  "bumpalo",[m
  "log",[m
[36m@@ -2182,9 +2228,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "wasm-bindgen-macro"[m
[31m-version = "0.2.100"[m
[32m+[m[32mversion = "0.2.102"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "7fe63fc6d09ed3792bd0897b314f53de8e16568c2b3f7982f468c0bf9bd0b407"[m
[32m+[m[32mchecksum = "0d7ab4ca3e367bb1ed84ddbd83cc6e41e115f8337ed047239578210214e36c76"[m
 dependencies = [[m
  "quote",[m
  "wasm-bindgen-macro-support",[m
[36m@@ -2192,9 +2238,9 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "wasm-bindgen-macro-support"[m
[31m-version = "0.2.100"[m
[32m+[m[32mversion = "0.2.102"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "8ae87ea40c9f689fc23f209965b6fb8a99ad69aeeb0231408be24920604395de"[m
[32m+[m[32mchecksum = "4a518014843a19e2dbbd0ed5dfb6b99b23fb886b14e6192a00803a3e14c552b0"[m
 dependencies = [[m
  "proc-macro2",[m
  "quote",[m
[36m@@ -2205,18 +2251,18 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "wasm-bindgen-shared"[m
[31m-version = "0.2.100"[m
[32m+[m[32mversion = "0.2.102"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "1a05d73b933a847d6cccdda8f838a22ff101ad9bf93e33684f39c1f5f0eece3d"[m
[32m+[m[32mchecksum = "255eb0aa4cc2eea3662a00c2bbd66e93911b7361d5e0fcd62385acfd7e15dcee"[m
 dependencies = [[m
  "unicode-ident",[m
 ][m
 [m
 [[package]][m
 name = "web-sys"[m
[31m-version = "0.3.77"[m
[32m+[m[32mversion = "0.3.79"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "33b6dd2ef9186f1f2072e409e99cd22a975331a6b3591b12c764e0e55c60d5d2"[m
[32m+[m[32mchecksum = "50462a022f46851b81d5441d1a6f5bac0b21a1d72d64bd4906fbdd4bf7230ec7"[m
 dependencies = [[m
  "js-sys",[m
  "wasm-bindgen",[m
[36m@@ -2240,11 +2286,11 @@[m [mchecksum = "ac3b87c63620426dd9b991e5ce0329eff545bccbbb34f3be09ff6fb6ab51b7b6"[m
 [m
 [[package]][m
 name = "winapi-util"[m
[31m-version = "0.1.9"[m
[32m+[m[32mversion = "0.1.11"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "cf221c93e13a30d793f7645a0e7762c55d169dbb0a49671918a2319d289b10bb"[m
[32m+[m[32mchecksum = "c2a7b1c03c876122aa43f3020e6c3c3ee5c05081c9a00739faf7503aeba10d22"[m
 dependencies = [[m
[31m- "windows-sys 0.59.0",[m
[32m+[m[32m "windows-sys 0.61.2",[m
 ][m
 [m
 [[package]][m
[36m@@ -2416,13 +2462,10 @@[m [msource = "registry+https://github.com/rust-lang/crates.io-index"[m
 checksum = "d6bbff5f0aada427a1e5a6da5f1f98158182f26556f345ac9e04d36d0ebed650"[m
 [m
 [[package]][m
[31m-name = "wit-bindgen-rt"[m
[31m-version = "0.39.0"[m
[32m+[m[32mname = "wit-bindgen"[m
[32m+[m[32mversion = "0.46.0"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "6f42320e61fe2cfd34354ecb597f86f413484a798ba44a8ca1165c58d42da6c1"[m
[31m-dependencies = [[m
[31m- "bitflags 2.9.1",[m
[31m-][m
[32m+[m[32mchecksum = "f17a85883d4e6d00e8a97c586de764dabcc06133f7f1d55dce5cdc070ad7fe59"[m
 [m
 [[package]][m
 name = "writeable"[m
[36m@@ -2455,18 +2498,18 @@[m [mdependencies = [[m
 [m
 [[package]][m
 name = "zerocopy"[m
[31m-version = "0.8.25"[m
[32m+[m[32mversion = "0.8.31"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "a1702d9583232ddb9174e01bb7c15a2ab8fb1bc6f227aa1233858c351a3ba0cb"[m
[32m+[m[32mchecksum = "fd74ec98b9250adb3ca554bdde269adf631549f51d8a8f8f0a10b50f1cb298c3"[m
 dependencies = [[m
  "zerocopy-derive",[m
 ][m
 [m
 [[package]][m
 name = "zerocopy-derive"[m
[31m-version = "0.8.25"[m
[32m+[m[32mversion = "0.8.31"[m
 source = "registry+https://github.com/rust-lang/crates.io-index"[m
[31m-checksum = "28a6e20d751156648aa063f3800b706ee209a32c0b4d9f24be3d980b01be55ef"[m
[32m+[m[32mchecksum = "d8a8d209fdf45cf5138cbb5a506f6b52522a25afccc534d1475dad8e31105c6a"[m
 dependencies = [[m
  "proc-macro2",[m
  "quote",[m
[1mdiff --git a/Cargo.toml b/Cargo.toml[m
[1mindex 0a73332..cb6ddd6 100644[m
[1m--- a/Cargo.toml[m
[1m+++ b/Cargo.toml[m
[36m@@ -10,12 +10,11 @@[m [mlicense = "MIT"[m
 [m
 [workspace.dependencies][m
 ahash = { version = "0.8.12" }[m
[31m-anyhow = "1.0.98"[m
[31m-clap = { version = "4.5.39", features = ["derive"] }[m
[31m-criterion = { version = "0.6.0", features = ["html_reports"] }[m
[31m-derive_more = { version = "2.0.1", features = ["deref", "deref_mut", "from", "display", "constructor"] }[m
[32m+[m[32manyhow = "1.0.100"[m
[32m+[m[32mclap = { version = "4.5.53", features = ["derive"] }[m
[32m+[m[32mcriterion = { version = "0.8.0", features = ["html_reports"] }[m
 derive_builder = { version = "0.20.2" }[m
[31m-strsim = "0.11.1"[m
[32m+[m[32mderive_more = { version = "2.0.1", features = ["deref", "deref_mut", "from", "display", "constructor"] }[m
 factorial = "0.4.0"[m
 itertools = "0.14.0"[m
 miette = { version = "7.6.0", features = ["fancy"] }[m
[36m@@ -23,14 +22,17 @@[m [mndc_lib = { path = "ndc_lib" }[m
 ndc_macros = { path = "ndc_macros" }[m
 num = "0.4.3"[m
 once_cell = "1.21.3"[m
[31m-ordered-float = "5.0.0"[m
[31m-owo-colors = "4.2.1"[m
[31m-rand = "0.9.1"[m
[32m+[m[32mordered-float = "5.1.0"[m
[32m+[m[32mowo-colors = "4.2.3"[m
[32m+[m[32mrand = "0.9.2"[m
 rand_chacha = "0.9.0"[m
[31m-regex = "1.11.1"[m
[31m-rustyline = { version = "16.0.0", features = ["derive"] }[m
[32m+[m[32mregex = "1.12.2"[m
[32m+[m[32mrustyline = { version = "17.0.2", features = ["derive"] }[m
 ryu = "1.0.20"[m
[31m-self_cell = "1.2.0"[m
[31m-serde_json = { version = "1.0.140", features = ["arbitrary_precision"] }[m
[32m+[m[32mself_cell = "1.2.1"[m
[32m+[m[32mserde_json = { version = "1.0.145", features = ["arbitrary_precision"] }[m
[32m+[m[32mstrsim = "0.11.1"[m
 tap = "1.0.1"[m
[31m-thiserror = "2.0.12"[m
[32m+[m[32mthiserror = "2.0.17"[m
[32m+[m[32mtokio = { version = "1.48.0", features = ["full"] }[m
[32m+[m[32mtower-lsp = "0.20.0"[m
[1mdiff --git a/ndc_bin/Cargo.toml b/ndc_bin/Cargo.toml[m
[1mindex 4ecb676..3b6a65f 100644[m
[1m--- a/ndc_bin/Cargo.toml[m
[1m+++ b/ndc_bin/Cargo.toml[m
[36m@@ -18,10 +18,6 @@[m [mndc_lib.workspace = true[m
 owo-colors.workspace = true[m
 rustyline.workspace = true[m
 tap.workspace = true[m
[31m-termimad = "0.33.0"[m
[32m+[m[32mtermimad = "0.34.1"[m
 ndc_lsp = { path = "../ndc_lsp", optional = true }[m
[31m-tokio = { version = "1.48.0", optional = true, features = ["full"] }[m
[31m-[m
[31m-[features][m
[31m-default = ["lsp"][m
[31m-lsp = ["ndc_lsp", "tokio"][m
\ No newline at end of file[m
[32m+[m[32mtokio.workspace = true[m
[1mdiff --git a/ndc_bin/src/main.rs b/ndc_bin/src/main.rs[m
[1mindex e52a3f1..d9014f3 100644[m
[1m--- a/ndc_bin/src/main.rs[m
[1m+++ b/ndc_bin/src/main.rs[m
[36m@@ -153,7 +153,6 @@[m [mfn main() -> anyhow::Result<()> {[m
     Ok(())[m
 }[m
 [m
[31m-#[cfg(feature = "lsp")][m
 fn start_lsp() {[m
     #[allow([m
         clippy::expect_used,[m
[1mdiff --git a/ndc_lib/Cargo.toml b/ndc_lib/Cargo.toml[m
[1mindex 06fa29a..28b01be 100644[m
[1m--- a/ndc_lib/Cargo.toml[m
[1m+++ b/ndc_lib/Cargo.toml[m
[36m@@ -25,7 +25,7 @@[m [mtap.workspace = true[m
 thiserror.workspace = true[m
 [m
 # Crypto[m
[31m-md5 = { version = "0.7.0", optional = true }[m
[32m+[m[32mmd5 = { version = "0.8.0", optional = true }[m
 sha1 = { version = "0.10.6", optional = true }[m
 [m
 [features][m
[1mdiff --git a/ndc_lsp/Cargo.toml b/ndc_lsp/Cargo.toml[m
[1mindex 54c61d7..f394291 100644[m
[1m--- a/ndc_lsp/Cargo.toml[m
[1m+++ b/ndc_lsp/Cargo.toml[m
[36m@@ -7,5 +7,5 @@[m [mversion.workspace = true[m
 [dependencies][m
 tokio = { version = "1.48.0", features = ["full"] }[m
 ndc_lib.workspace = true[m
[31m-tower-lsp = "0.20.0"[m
[32m+[m[32mtower-lsp.workspace = true[m
 [m
[1mdiff --git a/ndc_macros/Cargo.toml b/ndc_macros/Cargo.toml[m
[1mindex 58fa9bc..9cf65f1 100644[m
[1m--- a/ndc_macros/Cargo.toml[m
[1m+++ b/ndc_macros/Cargo.toml[m
[36m@@ -9,7 +9,7 @@[m [mlicense = "MIT"[m
 proc-macro = true[m
 [m
 [dependencies][m
[31m-proc-macro2 = "1.0.95"[m
[31m-quote = "1.0.40"[m
[31m-syn = { version = "2.0.101", features = ["full", "extra-traits"] }[m
[32m+[m[32mproc-macro2 = "1.0.103"[m
[32m+[m[32mquote = "1.0.42"[m
[32m+[m[32msyn = { version = "2.0.111", features = ["full", "extra-traits"] }[m
 itertools.workspace = true[m
