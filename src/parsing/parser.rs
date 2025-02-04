use pest::iterators::Pair;
use std::fs;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"] // relative to src
struct PestCortexParser;

pub struct CortexParser;

impl CortexParser {
    
}
