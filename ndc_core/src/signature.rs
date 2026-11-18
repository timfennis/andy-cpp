//! Built-in function signatures written in the language's own type syntax,
//! such as `<T>([T], T) -> Option<T>`.

use crate::StaticType;

impl StaticType {
    /// The function type a built-in's signature describes:
    /// `<T>([T], T) -> Option<T>` declares the type parameter `T`, two
    /// parameters and a return type. Without `-> …` the function returns
    /// `()`. `(*)` takes any number of arguments of any type, and
    /// `fn(Int) -> Int` is a function type.
    pub fn signature(text: &str) -> Result<Self, String> {
        let mut parser = SignatureParser {
            tokens: tokenize(text)?,
            position: 0,
            generics: Vec::new(),
        };
        let signature = parser.signature()?;
        if let Some(token) = parser.peek() {
            return Err(format!("unexpected '{token}' after the signature"));
        }
        // Explicit type arguments, as in `f::<Int, String>(…)`, bind the
        // type parameters in the order they're declared, which is the order
        // they're first used in, so the type alone says it.
        let used = signature.type_parameters();
        if !parser
            .generics
            .iter()
            .copied()
            .eq(used.iter().map(AsRef::as_ref))
        {
            return Err(format!(
                "declare the type parameters of '{text}' in the order they're first used in: <{}>",
                used.join(", ")
            ));
        }
        Ok(signature)
    }
}

/// The tokens of `text`, as slices of it: `[T]` is `[`, `T` and `]`.
fn tokenize(text: &str) -> Result<Vec<&str>, String> {
    let mut tokens = Vec::new();
    let mut chars = text.char_indices().peekable();
    while let Some((start, c)) = chars.next() {
        match c {
            c if c.is_whitespace() => {}
            '<' | '>' | '(' | ')' | '[' | ']' | '}' | ':' | ',' | '*' => {
                tokens.push(&text[start..start + 1]);
            }
            '%' if chars.next_if(|&(_, next)| next == '{').is_some() => {
                tokens.push(&text[start..start + 2]);
            }
            '-' if chars.next_if(|&(_, next)| next == '>').is_some() => {
                tokens.push(&text[start..start + 2]);
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut end = start + c.len_utf8();
                while let Some((index, next)) =
                    chars.next_if(|&(_, next)| next.is_alphanumeric() || next == '_')
                {
                    end = index + next.len_utf8();
                }
                tokens.push(&text[start..end]);
            }
            other => return Err(format!("unexpected '{other}' in signature '{text}'")),
        }
    }
    Ok(tokens)
}

struct SignatureParser<'a> {
    tokens: Vec<&'a str>,
    position: usize,
    /// The type parameters the signature declared, such as `T` in `<T>`.
    generics: Vec<&'a str>,
}

impl<'a> SignatureParser<'a> {
    fn peek(&self) -> Option<&'a str> {
        self.tokens.get(self.position).copied()
    }

    fn next(&mut self) -> Result<&'a str, String> {
        let token = self
            .peek()
            .ok_or_else(|| "the signature ends too early".to_string())?;
        self.position += 1;
        Ok(token)
    }

    fn eat(&mut self, token: &str) -> bool {
        let found = self.peek() == Some(token);
        if found {
            self.position += 1;
        }
        found
    }

    fn expect(&mut self, token: &str) -> Result<(), String> {
        let found = self.next()?;
        if found == token {
            Ok(())
        } else {
            Err(format!("expected '{token}' but found '{found}'"))
        }
    }

    /// `<T, U>(…) -> R`, where the type parameters and the return type are
    /// optional.
    fn signature(&mut self) -> Result<StaticType, String> {
        if self.eat("<") {
            loop {
                let name = self.next()?;
                if self.generics.contains(&name) {
                    return Err(format!("type parameter '{name}' is declared twice"));
                }
                self.generics.push(name);
                if !self.eat(",") {
                    break;
                }
            }
            self.expect(">")?;
        }
        self.function()
    }

    /// `(…) -> R` after a function's name or after `Fn`.
    fn function(&mut self) -> Result<StaticType, String> {
        self.expect("(")?;
        let parameters = if self.eat("*") {
            self.expect(")")?;
            None
        } else {
            Some(self.list(")")?)
        };
        let return_type = if self.eat("->") {
            self.typ()?
        } else {
            StaticType::unit()
        };
        Ok(StaticType::Function {
            parameters,
            return_type: Box::new(return_type),
        })
    }

    /// Types separated by commas up to `close`, which is consumed.
    fn list(&mut self, close: &str) -> Result<Vec<StaticType>, String> {
        let mut types = Vec::new();
        while !self.eat(close) {
            types.push(self.typ()?);
            if !self.eat(",") {
                self.expect(close)?;
                break;
            }
        }
        Ok(types)
    }

    fn typ(&mut self) -> Result<StaticType, String> {
        let token = self.next()?;
        match token {
            // `()` is unit, `(A, B)` and `(A,)` are tuples, and `(A)` is just `A`.
            "(" => {
                let start = self.position;
                let mut elements = self.list(")")?;
                let trailing_comma = self.tokens[start..self.position].ends_with(&[",", ")"]);
                if elements.len() == 1 && !trailing_comma {
                    Ok(elements.remove(0))
                } else {
                    Ok(StaticType::Tuple(elements))
                }
            }
            "[" => {
                let element = self.typ()?;
                self.expect("]")?;
                Ok(StaticType::List(Box::new(element)))
            }
            "%{" => {
                let key = self.typ()?;
                let value = if self.eat(":") {
                    self.typ()?
                } else {
                    StaticType::unit()
                };
                self.expect("}")?;
                Ok(StaticType::Map {
                    key: Box::new(key),
                    value: Box::new(value),
                })
            }
            "fn" => self.function(),
            name if self.generics.contains(&name) => Ok(StaticType::Generic(name.into())),
            name => {
                let arguments = if self.eat("<") {
                    self.list(">")?
                } else {
                    Vec::new()
                };
                StaticType::from_name_and_args(name, arguments).map_err(|error| error.to_string())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn function(parameters: Option<Vec<StaticType>>, return_type: StaticType) -> StaticType {
        StaticType::Function {
            parameters,
            return_type: Box::new(return_type),
        }
    }

    fn generic(name: &str) -> StaticType {
        StaticType::Generic(name.into())
    }

    #[test]
    fn a_signature_declares_its_parameters_and_return_type() {
        assert_eq!(
            StaticType::signature("<T>([T], T) -> Option<T>"),
            Ok(function(
                Some(vec![StaticType::List(Box::new(generic("T"))), generic("T")]),
                StaticType::Option(Box::new(generic("T"))),
            ))
        );
        assert_eq!(
            StaticType::signature("(String, Int) -> Bool"),
            Ok(function(
                Some(vec![StaticType::String, StaticType::Int]),
                StaticType::Bool
            ))
        );
    }

    #[test]
    fn a_signature_without_a_return_type_returns_unit() {
        assert_eq!(
            StaticType::signature("()"),
            Ok(function(Some(vec![]), StaticType::unit()))
        );
    }

    #[test]
    fn tuples_function_types_and_variadics_parse() {
        assert_eq!(
            StaticType::signature("<K, V>(%{K: V}) -> [(K, V)]"),
            Ok(function(
                Some(vec![StaticType::Map {
                    key: Box::new(generic("K")),
                    value: Box::new(generic("V")),
                }]),
                StaticType::List(Box::new(StaticType::Tuple(vec![
                    generic("K"),
                    generic("V")
                ]))),
            ))
        );
        assert_eq!(
            StaticType::signature("<T, U>([T], fn(T) -> U) -> [U]"),
            Ok(function(
                Some(vec![
                    StaticType::List(Box::new(generic("T"))),
                    function(Some(vec![generic("T")]), generic("U")),
                ]),
                StaticType::List(Box::new(generic("U"))),
            ))
        );
        assert_eq!(
            StaticType::signature("(*) -> Any"),
            Ok(function(None, StaticType::Any))
        );
        assert_eq!(
            StaticType::signature("((Int,), (Int))"),
            Ok(function(
                Some(vec![
                    StaticType::Tuple(vec![StaticType::Int]),
                    StaticType::Int
                ]),
                StaticType::unit()
            ))
        );
    }

    #[test]
    fn a_malformed_signature_is_rejected() {
        for text in [
            "([T])",
            "<T, T>(T)",
            "(Int",
            "(Int) ->",
            "(Int) -> Int extra",
            "(Int; Int)",
        ] {
            assert!(StaticType::signature(text).is_err(), "{text}");
        }
    }

    #[test]
    fn type_parameters_are_declared_in_the_order_they_are_first_used_in() {
        assert!(StaticType::signature("<T, A>(Iterable<T>, A) -> A").is_ok());
        let error = StaticType::signature("<A, T>(Iterable<T>, A) -> A").unwrap_err();
        assert!(error.contains("<T, A>"), "{error}");
        // One that's never used can't be bound by a call's arguments.
        assert!(StaticType::signature("<T>() -> Int").is_err());
    }
}
