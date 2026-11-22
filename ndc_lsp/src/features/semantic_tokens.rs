//! Semantic highlighting: what each name is, from what the analysis resolved
//! it to, which an editor's grammar can't tell. A parameter, a local, one of
//! the program's functions and a built-in look alike to a grammar, and a
//! shadowing `len` is a variable, not the built-in. The grammar still colours
//! everything else: keywords, strings, numbers and comments.

use ls_types::{
    Range, SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend,
};

use crate::features::definition::Meanings;
use crate::names::{Declares, Name, Place, Refers};
use crate::resolved::Definition;
use crate::state::DocumentState;

/// The token types, in the order the legend gives their indexes.
const TYPES: [SemanticTokenType; 7] = [
    SemanticTokenType::FUNCTION,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::TYPE,
];

/// The token modifiers, in the order of their bits.
const MODIFIERS: [SemanticTokenModifier; 3] = [
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::MODIFICATION,
    SemanticTokenModifier::DEFAULT_LIBRARY,
];

const DECLARATION: u32 = 1;
const MODIFICATION: u32 = 1 << 1;
const DEFAULT_LIBRARY: u32 = 1 << 2;

/// What the server tells the client its tokens' numbers mean.
pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TYPES.to_vec(),
        token_modifiers: MODIFIERS.to_vec(),
    }
}

/// The tokens of every name in the document whose kind is known, encoded as
/// the protocol wants them: each relative to the one before it, in UTF-16.
pub fn tokens(state: &DocumentState) -> Vec<SemanticToken> {
    let meanings = Meanings::new(state);
    let mut tokens = Vec::new();
    let (mut line, mut start) = (0, 0);
    for name in state.names.iter() {
        let text = &state.analysed_text()[name.span.offset()..name.span.end()];
        // An operator is a name the analysis resolves too, as `*` in
        // `x * 2`, but the grammar colours operators.
        if !text.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            continue;
        }
        let Some((token_type, modifiers)) = classify(state, &meanings, name) else {
            continue;
        };
        // A name that changed since the analysis is left to the grammar.
        let Some(Range {
            start: position, ..
        }) = state.name_range(name.span)
        else {
            continue;
        };
        let delta_line = position.line - line;
        let delta_start = if delta_line == 0 {
            position.character - start
        } else {
            position.character
        };
        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length: u32::try_from(text.encode_utf16().count()).unwrap_or(u32::MAX),
            token_type,
            token_modifiers_bitset: modifiers,
        });
        (line, start) = (position.line, position.character);
    }
    tokens
}

/// The token type, as its index in [`TYPES`], and the modifiers of `name`;
/// `None` when what it is isn't known, as for a name nothing declares.
fn classify(state: &DocumentState, meanings: &Meanings<'_>, name: &Name) -> Option<(u32, u32)> {
    let index = |typ: SemanticTokenType| {
        TYPES
            .iter()
            .position(|t| *t == typ)
            .and_then(|i| u32::try_from(i).ok())
    };
    let definitions = meanings.of(name);
    // A name that means a declaration is what the declaration declares; all
    // its overloads declare one kind of thing.
    if let Some(&first) = definitions.first() {
        let kind = state.names.kind(first)?;
        let declares = match name.refers {
            Refers::Declaration(_) => true,
            Refers::Node(node) => definitions.contains(&Definition::Node(node)),
        };
        let modifiers = match (name.write, declares) {
            (true, true) => DECLARATION,
            (true, false) => MODIFICATION,
            (false, _) => 0,
        };
        let typ = match kind {
            Declares::Function => SemanticTokenType::FUNCTION,
            Declares::Struct => SemanticTokenType::STRUCT,
            Declares::Field => SemanticTokenType::PROPERTY,
            Declares::TypeParameter => SemanticTokenType::TYPE_PARAMETER,
            Declares::Parameter => SemanticTokenType::PARAMETER,
            Declares::Variable => SemanticTokenType::VARIABLE,
        };
        return Some((index(typ)?, modifiers));
    }
    // One that means none is a built-in, if it resolved at all.
    let Refers::Node(node) = name.refers else {
        return None;
    };
    state.resolved.definitions_of(node)?;
    let typ = match name.place {
        Place::Code => SemanticTokenType::FUNCTION,
        Place::Type => SemanticTokenType::TYPE,
        Place::Member => SemanticTokenType::PROPERTY,
    };
    Some((index(typ)?, DEFAULT_LIBRARY))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Each token of `source` as its text, type and modifiers.
    fn tokens_of(source: &str) -> Vec<(String, SemanticTokenType, Vec<SemanticTokenModifier>)> {
        let state = DocumentState::analysed(source);
        let (mut line, mut start) = (0u32, 0u32);
        tokens(&state)
            .into_iter()
            .map(|token| {
                line += token.delta_line;
                start = if token.delta_line == 0 {
                    start + token.delta_start
                } else {
                    token.delta_start
                };
                let text: Vec<u16> = source
                    .lines()
                    .nth(line as usize)
                    .expect("a line")
                    .encode_utf16()
                    .collect();
                let text = String::from_utf16_lossy(
                    &text[start as usize..(start + token.length) as usize],
                );
                let modifiers = MODIFIERS
                    .iter()
                    .enumerate()
                    .filter(|(bit, _)| token.token_modifiers_bitset & (1 << bit) != 0)
                    .map(|(_, modifier)| modifier.clone())
                    .collect();
                (text, TYPES[token.token_type as usize].clone(), modifiers)
            })
            .collect()
    }

    fn token(
        text: &str,
        typ: SemanticTokenType,
        modifiers: &[SemanticTokenModifier],
    ) -> (String, SemanticTokenType, Vec<SemanticTokenModifier>) {
        (text.to_string(), typ, modifiers.to_vec())
    }

    use SemanticTokenModifier as M;
    use SemanticTokenType as T;

    #[test]
    fn names_are_what_they_were_declared_as() {
        let source = "fn double(x: Int) { x * 2 }\nlet total = 0;\ntotal += double(total);";
        assert_eq!(
            tokens_of(source),
            [
                token("double", T::FUNCTION, &[M::DECLARATION]),
                token("x", T::PARAMETER, &[M::DECLARATION]),
                token("Int", T::TYPE, &[M::DEFAULT_LIBRARY]),
                token("x", T::PARAMETER, &[]),
                token("total", T::VARIABLE, &[M::DECLARATION]),
                token("total", T::VARIABLE, &[M::MODIFICATION]),
                token("double", T::FUNCTION, &[]),
                token("total", T::VARIABLE, &[]),
            ]
        );
    }

    #[test]
    fn structs_fields_and_type_parameters() {
        let source = "struct P { x: Int }\nfn first<T>(xs: [T]) -> T { xs[0] }\nlet p: P = P(1);\np.x = p.x;";
        let found = tokens_of(source);
        assert!(found.contains(&token("P", T::STRUCT, &[M::DECLARATION])));
        assert!(found.contains(&token("x", T::PROPERTY, &[M::DECLARATION])));
        assert!(found.contains(&token("T", T::TYPE_PARAMETER, &[M::DECLARATION])));
        assert!(found.contains(&token("T", T::TYPE_PARAMETER, &[])));
        assert!(found.contains(&token("P", T::STRUCT, &[])));
        assert!(found.contains(&token("x", T::PROPERTY, &[M::MODIFICATION])));
        assert!(found.contains(&token("x", T::PROPERTY, &[])));
    }

    #[test]
    fn a_built_in_is_marked_as_one_unless_a_name_hides_it() {
        assert_eq!(
            tokens_of("print(len([1]));"),
            [
                token("print", T::FUNCTION, &[M::DEFAULT_LIBRARY]),
                token("len", T::FUNCTION, &[M::DEFAULT_LIBRARY]),
            ]
        );
        let found = tokens_of("let len = 3;\nprint(len);");
        assert_eq!(found[2], token("len", T::VARIABLE, &[]));
    }

    #[test]
    fn a_name_nothing_declares_gets_no_token() {
        let state = DocumentState::analysed("print(missing);");
        assert_eq!(tokens(&state).len(), 1, "only `print`");
    }

    #[test]
    fn positions_count_utf16_units() {
        // `é` is one UTF-16 unit and `😀` two, but two and four bytes.
        let found = tokens_of("let s = \"é😀\"; let t = s;");
        assert_eq!(
            found,
            [
                token("s", T::VARIABLE, &[M::DECLARATION]),
                token("t", T::VARIABLE, &[M::DECLARATION]),
                token("s", T::VARIABLE, &[]),
            ]
        );
    }
}
