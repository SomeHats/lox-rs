use colored::Colorize;
use itertools::Itertools;
use std::{fmt::Display, str::FromStr};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SymbolParseError {
    #[error("Symbol parse error at {0}")]
    UnexpectedCharacter(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol(Vec<SymbolPart>);
impl Symbol {
    pub fn parts(&self) -> &[SymbolPart] {
        &self.0
    }
    pub fn strip_hash(mut self) -> Self {
        if let Some(SymbolPart::Simple(last)) = self.0.last() {
            if last.starts_with('h') && last[1..].chars().all(|c| c.is_ascii_hexdigit()) {
                self.0.pop();
            }
        }
        self
    }
    pub fn simplify(self) -> Self {
        let mut retained_parts = vec![];
        for part in self.0.into_iter().rev() {
            match part {
                SymbolPart::Simple(name) => {
                    let first_is_upper = name.chars().next().unwrap_or('a').is_uppercase();
                    retained_parts.push(SymbolPart::Simple(name));
                    if first_is_upper {
                        break;
                    }
                }
                SymbolPart::Generic(name, params) => {
                    let first_is_upper = name.chars().next().unwrap_or('a').is_uppercase();
                    retained_parts.push(SymbolPart::Generic(
                        name,
                        params.into_iter().map(Symbol::simplify).collect(),
                    ));
                    if first_is_upper {
                        break;
                    }
                }
                SymbolPart::Alias(a, b) => {
                    retained_parts.push(SymbolPart::Alias(a.simplify(), b.simplify()))
                }
                SymbolPart::Impl(a, Some(b)) => {
                    retained_parts.push(SymbolPart::impl_for(a.simplify(), b.simplify()))
                }
                SymbolPart::Impl(a, None) => retained_parts.push(SymbolPart::impl_(a.simplify())),
                SymbolPart::Array(a) => retained_parts.push(SymbolPart::Array(a.simplify())),
            }
        }

        retained_parts.reverse();
        Symbol(retained_parts)
    }
}
impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.parts().iter().map(|part| part.to_string()).join("::")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolPart {
    Simple(String),
    Generic(String, Vec<Symbol>),
    Alias(Symbol, Symbol),
    Impl(Symbol, Option<Symbol>),
    Array(Symbol),
}
impl SymbolPart {
    pub fn simple(name: &str) -> Self {
        SymbolPart::Simple(name.to_string())
    }
    pub fn impl_(name: Symbol) -> Self {
        SymbolPart::Impl(name, None)
    }
    pub fn impl_for(name: Symbol, impl_: Symbol) -> Self {
        SymbolPart::Impl(name, Some(impl_))
    }
}
impl Display for SymbolPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolPart::Simple(name) => write!(f, "{}", name),
            SymbolPart::Generic(name, generics) => {
                write!(
                    f,
                    "{}<{}>",
                    name,
                    generics.iter().map(|symbol| symbol.to_string()).join(",")
                )
            }
            SymbolPart::Alias(from, to) => {
                write!(f, "<{} as {}>", from, to)
            }
            SymbolPart::Impl(trt, fr) => match fr {
                Some(fr) => write!(f, "<impl {} for {}>", trt, fr),
                None => write!(f, "<impl {}>", trt),
            },
            SymbolPart::Array(inner) => write!(f, "[{}]", inner),
        }
    }
}

fn advance(symbol: &mut &str) -> char {
    match symbol.chars().next() {
        Some(ch) => {
            *symbol = &symbol[ch.len_utf8()..];
            ch
        }
        None => '\0',
    }
}
fn peek(symbol: &str) -> char {
    symbol.chars().next().unwrap_or('\0')
}
fn consume(symbol: &mut &str, expected: char) -> Result<(), SymbolParseError> {
    if expected == peek(symbol) {
        advance(symbol);
        Ok(())
    } else {
        Err(SymbolParseError::UnexpectedCharacter(symbol.to_string()))
    }
}
fn parse_symbol_inner(symbol: &mut &str) -> Result<Symbol, SymbolParseError> {
    let mut parts = vec![];
    loop {
        match peek(symbol) {
            '\0' | ' ' | '>' | ']' => break,
            '[' => {
                consume(symbol, '[')?;
                let array = parse_symbol_inner(symbol)?;
                parts.push(SymbolPart::Array(array));
                consume(symbol, ']')?;
            }
            '<' => {
                advance(symbol);

                if peek(symbol) == 'i' {
                    consume(symbol, 'i')?;
                    consume(symbol, 'm')?;
                    consume(symbol, 'p')?;
                    consume(symbol, 'l')?;
                    consume(symbol, ' ')?;
                    let first_symbol = parse_symbol_inner(symbol)?;
                    if symbol.starts_with(" for") {
                        consume(symbol, ' ')?;
                        consume(symbol, 'f')?;
                        consume(symbol, 'o')?;
                        consume(symbol, 'r')?;
                        consume(symbol, ' ')?;
                        let second_symbol = parse_symbol_inner(symbol)?;
                        parts.push(SymbolPart::Impl(first_symbol, Some(second_symbol)));
                    } else {
                        parts.push(SymbolPart::Impl(first_symbol, None));
                    }
                } else {
                    let first_symbol = parse_symbol_inner(symbol)?;
                    consume(symbol, ' ')?;
                    consume(symbol, 'a')?;
                    consume(symbol, 's')?;
                    consume(symbol, ' ')?;
                    let next_symbol = parse_symbol_inner(symbol)?;
                    parts.push(SymbolPart::Alias(first_symbol, next_symbol));
                }

                consume(symbol, '>')?;
            }
            ':' => {
                consume(symbol, ':')?;
                consume(symbol, ':')?;
            }
            _ => {
                let mut part_name = String::new();
                let mut generics = None;
                loop {
                    match peek(symbol) {
                        '\0' | ' ' | ':' | '>' | ']' => break,
                        '<' => {
                            advance(symbol);
                            let mut generic_parts = vec![];
                            loop {
                                match peek(symbol) {
                                    '\0' => break,
                                    '>' => {
                                        advance(symbol);
                                        break;
                                    }
                                    ',' => {
                                        advance(symbol);
                                    }
                                    _ => {
                                        generic_parts.push(parse_symbol_inner(symbol)?);
                                    }
                                }
                            }
                            generics = Some(generic_parts);
                            break;
                        }
                        ch => {
                            advance(symbol);
                            part_name.push(ch)
                        }
                    }
                }

                if let Some(generics) = generics {
                    parts.push(SymbolPart::Generic(part_name, generics));
                } else {
                    parts.push(SymbolPart::Simple(part_name));
                }
            }
        }
    }
    Ok(Symbol(parts))
}

impl FromStr for Symbol {
    type Err = SymbolParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut symbol = s;
        parse_symbol_inner(&mut symbol)
    }
}

pub fn minitrace() {
    if !cfg!(feature = "minitrace") {
        return;
    }
    let trace = backtrace::Backtrace::new()
        .frames()
        .iter()
        .map(|frame| frame.symbols().first().unwrap())
        .map(|symbol| {
            (
                symbol
                    .name()
                    .map(|name| name.to_string())
                    .unwrap_or_else(|| "UNKNOWN".to_string()),
                symbol,
            )
        })
        .map(|(symbol, full)| (symbol.parse::<Symbol>().unwrap().strip_hash(), full))
        .filter(|(symbol, _)| {
            let parts = symbol.parts();
            if parts.get(0) == Some(&SymbolPart::simple("backtrace")) {
                return false;
            }
            if parts.get(0) == Some(&SymbolPart::simple("lox_rs")) {
                if parts.get(1) == Some(&SymbolPart::simple("backtrace")) {
                    return false;
                }
                if parts.get(1) == Some(&SymbolPart::simple("vm_interpreter"))
                    && parts.get(2) == Some(&SymbolPart::simple("gc"))
                {
                    return false;
                }
            }
            true
        })
        .take(10)
        .enumerate()
        .map(|(idx, (name, full))| {
            format!(
                "{}: {} {}",
                idx,
                name.simplify(),
                format!(
                    "{}:{}:{}",
                    full.filename()
                        .map_or("<unknown>".to_string(), |f| f.to_string_lossy().to_string()),
                    full.lineno().unwrap_or(0),
                    full.colno().unwrap_or(0),
                )
                .dimmed()
            )
        })
        .join("\n");
    println!("{}", trace);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_symbol() {
        fn roundtrip(symbol: &str) {
            let parsed: Symbol = symbol.parse().unwrap();
            let parsed_str = parsed.to_string();
            assert_eq!(symbol, parsed_str);
        }

        roundtrip("foo::bar::Baz");
        roundtrip("__pthread_start");
        roundtrip("foo::bar<T>::Baz");
        roundtrip("<alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once::hac5a6dbdf1c21893");
        roundtrip("core::ops::function::FnOnce::call_once{{vtable.shim}}::h773d20bd17ec6f60");
        roundtrip("std::thread::Builder::spawn_unchecked_::{{closure}}::h4d0538416da45588");
        roundtrip("<core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once::h13d5c93c56fb4da5");
        roundtrip("core::ops::function::impls::<impl core::ops::function::FnMut<A> for &F>::call_mut::hc83c01df6275bb60");
        roundtrip("core::ptr::drop_in_place<[lox_rs::vm_interpreter::chunk::ConstantValue]>::h6001b125d581d982");
        roundtrip("lox_rs::vm_interpreter::disassembler::<impl lox_rs::vm_interpreter::chunk::Chunk>::disassemble_instruction_at::hce72765a1d4ebcc6")
    }
}
