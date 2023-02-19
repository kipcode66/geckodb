use std::{num::ParseIntError, str::FromStr};

use lazy_static::lazy_static;
use regex::{Error as RegexError, Regex};

#[derive(Debug, Clone)]
pub enum ParseError {
    Regex(RegexError),
    Num(ParseIntError),
}

impl From<RegexError> for ParseError {
    fn from(value: RegexError) -> Self {
        ParseError::Regex(value)
    }
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        ParseError::Num(value)
    }
}

lazy_static! {
    static ref SYMBOL_REGEX: Regex = Regex::new(r"^\s+([[:xdigit:]]{8})\s+([[:xdigit:]]{6})\s+([[:xdigit:]]{8})\s+4\s+(.+?)(?:\s+([^\.\s]+\.[aA]))?(?:\s+([^\.\s]+\.[oO]))\s*$").unwrap();
    static ref SECTION_REGEX: Regex = Regex::new(r"^(\.[[:word:]]+)\s+section\s+layout\s*$").unwrap();
}

#[derive(Debug, Clone)]
pub struct Section {
    pub name: String,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub section: Section,
    pub section_offset: u32,
    pub symbol_size: u32,
    pub symbol_addr: u32,
    pub symbol_name: String,
    pub symbol_file: String,
}

pub fn parse_symbols(text: &str) -> Result<Vec<Symbol>, ParseError> {
    let mut symbols: Vec<Symbol> = Vec::new();
    let mut current_section: Section = Section { name: "".into(), id: 0 };
    let mut current_id: usize = 0;
    for line in text.lines() {
        if let Some(section_match) = SECTION_REGEX.captures(line) {
            current_id += 1;
            current_section = match section_match.get(1).map(|m| m.as_str()) {
                Some(section_name) => Section {
                    id: current_id,
                    name: section_name.into()
                },
                None => current_section,
            };
        }
        if let Some(symbol_match) = SYMBOL_REGEX.captures(line) {
            symbols.push(Symbol {
                section: current_section.clone(),
                section_offset: u32::from_str_radix(&symbol_match[1], 16)?,
                symbol_size: u32::from_str_radix(&symbol_match[2], 16)?,
                symbol_addr: u32::from_str_radix(&symbol_match[3], 16)?,
                symbol_name: (&symbol_match[4]).into(),
                symbol_file: (&symbol_match[6]).into(),
            });
        }
    }
    return Ok(symbols);
}
