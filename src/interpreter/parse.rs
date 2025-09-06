use nom::{
    branch::alt, bytes::complete::{tag, take_while, take_while1}, character::complete::multispace0, combinator::{all_consuming, map, opt, value}, multi::{many0, separated_list0, separated_list1}, sequence::{delimited, preceded, terminated}, IResult, Parser
};
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;

use crate::interpreter::ast::{ClassCell, ClassTable, Declaration, Function, ProgramFile, Statement};

#[derive(Debug, PartialEq, Clone, Eq)]
enum Word {
    Class,
    Fn,
    Token(String),
    Integer(BigInt),
    U32(u32),
}

fn colon(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag(":"), multispace0)).parse(input)
}

fn comma(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag(","), multispace0)).parse(input)
}

fn semicolon(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag(";"), multispace0)).parse(input)
}

fn open_brace(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("{"), multispace0)).parse(input)
}

fn close_brace(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("}"), multispace0)).parse(input)
}

fn open_paren(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("("), multispace0)).parse(input)
}

fn close_paren(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag(")"), multispace0)).parse(input)
}

fn arrow(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("->"), multispace0)).parse(input)
}

enum Precision {
    Big,
    U32,
}

fn parse_number(mut string: &str) -> Option<Word> {
    let precision = if string.ends_with("u32") {
        string = string.trim_end_matches("u32");
        Precision::U32
    } else {
        Precision::Big
    };

    let n = if string.starts_with("0x") {
        match BigInt::parse_bytes(string[2..].as_bytes(), 16) {
            Some(n) => n,
            None => return None,
        }
    } else if string.chars().next().unwrap().is_ascii_digit() {
        match string.parse::<BigInt>() {
            Ok(n) => n,
            Err(_) => return None,
        }
    } else {
        return None;
    };

    match precision {
        Precision::U32 => Some(Word::U32(n.to_u32().unwrap())), // Safe unwrap because we checked above
        Precision::Big => Some(Word::Integer(n)),
    }
}

fn unquoted_word(input: &str) -> IResult<&str, Word> {
    let (input, t) = terminated(
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        multispace0,
    )
    .parse(input)?;
    match t {
        "class" => Ok((input, Word::Class)),
        "fn" => Ok((input, Word::Fn)),
        _ => {
            if t.chars().next().unwrap().is_ascii_digit() {
                if let Some(n) = parse_number(t) {
                    Ok((input, n))
                } else {
                    Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Digit,
                    )))
                }
            } else {
                Ok((input, Word::Token(t.to_owned())))
            }
        }
    }
}

fn quoted_word(input: &str) -> IResult<&str, Word> {
    let (input, t) = terminated(
        nom::sequence::delimited(tag("'"), take_while(|c| c != '\''), tag("'")),
        multispace0,
    )
    .parse(input)?;
    Ok((input, Word::Token(t.to_owned())))
}

fn word(input: &str) -> IResult<&str, Word> {
    alt((quoted_word, unquoted_word)).parse(input)
}

fn specific_word(expected: Word) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input: &str| {
        let (input, t) = word(input)?;
        if t == expected {
            Ok((input, ()))
        } else {
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )))
        }
    }
}

fn class_cell(input: &str) -> IResult<&str, ClassCell> {
    let (input, t) = opt(word).parse(input)?;
    match t {
        None => Ok((input, ClassCell::Empty)),
        Some(Word::Token(x)) => Ok((input, ClassCell::Token(x))),
        Some(Word::Integer(n)) => Ok((input, ClassCell::Integer(n))),
        Some(Word::U32(n)) => Ok((input, ClassCell::U32(n))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn class_row(input: &str) -> IResult<&str, Vec<ClassCell>> {
    let (input, result) = separated_list1(colon, class_cell).parse(input)?;
    // Don't allow a single empty cell (this would correspond to an empty input)
    if result == vec![ClassCell::Empty] {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    } else {
        Ok((input, result))
    }
}

fn class_table(input: &str) -> IResult<&str, ClassTable> {
    let (input, _) = specific_word(Word::Class).parse(input)?;
    let (input, header) = class_row(input)?;
    let (input, body) =
        delimited(open_brace, opt(terminated(separated_list1(comma, class_row), opt(comma))), close_brace).parse(input)?;
    Ok((input, ClassTable { header, body: body.unwrap_or_default() }))
}

fn expr_statement(input: &str) -> IResult<&str, Statement> {
    let (input, expr) = class_cell(input)?;
    Ok((input, Statement::Expr(expr)))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    alt((expr_statement,)).parse(input)
}

fn function(input: &str) -> IResult<&str, Function> {
    let (input, _) = specific_word(Word::Fn).parse(input)?;
    let (input, header) = class_row(input)?;
    let (input, params) = delimited(open_paren, separated_list0(comma, class_row), close_paren).parse(input)?;
    let (input, ret) = preceded(arrow, class_row).parse(input)?;
    let (input, body) = delimited(open_brace, separated_list1(semicolon, statement), close_brace).parse(input)?;
    Ok((input, Function { header, params, ret, body }))
}

fn declaration(input: &str) -> IResult<&str, Declaration> {
    alt((
        map(class_table, Declaration::Class),
        map(function, Declaration::Fn)
    )).parse(input)
}

fn program_file(input: &str) -> IResult<&str, ProgramFile> {
    let (input, declarations) = all_consuming(many0(declaration)).parse(input)?;
    Ok((input, ProgramFile { declarations }))
}

pub fn parse(input: &str) -> ProgramFile {
    let (_, program_file) = program_file(input).expect("Parse error");
    program_file
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn class_cell_token() {
        let input = "my_token";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("my_token".to_owned()));
    }

    #[test]
    fn class_cell_token_space() {
        let input = "my_token ";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("my_token".to_owned()));
    }

    #[test]
    fn class_cell_int() {
        let input = "42";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Integer(BigInt::from(42)));
    }

    #[test]
    fn class_cell_hex() {
        let input = "0x2A";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Integer(BigInt::from(42)));
    }

    #[test]
    fn class_cell_hex_lower() {
        let input = "0x2a";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Integer(BigInt::from(42)));
    }

    #[test]
    fn class_cell_hex_empty_error() {
        let input = "0x";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn class_cell_u32() {
        let input = "42u32";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::U32(42));
    }

    #[test]
    fn class_cell_hex_u32() {
        let input = "0x2Au32";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::U32(42));
    }

    #[test]
    fn class_cell_int_suffix_error() {
        let input = "42abc";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn class_cell_int_prefix() {
        let input = "abc42";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("abc42".to_owned()));
    }

    #[test]
    fn class_cell_underscore_int() {
        let input = "_42";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("_42".to_owned()));
    }

    #[test]
    fn class_cell_quoted_class() {
        let input = "'class'";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("class".to_owned()));
    }

    #[test]
    fn class_cell_quoted_empty() {
        let input = "''";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("".to_owned()));
    }

    #[test]
    fn class_cell_quoted_number() {
        let input = "'42'";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("42".to_owned()));
    }

    #[test]
    fn class_cell_quoted_space() {
        let input = "' '";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token(" ".to_owned()));
    }

    #[test]
    fn class_cell_quoted_punctuation() {
        let input = "'!@#$%^&*().-'";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.unwrap().1 == ClassCell::Token("!@#$%^&*().-".to_owned()));
    }

    #[test]
    fn class_row_2() {
        let input = "my_token:42";
        let result = all_consuming(class_row).parse(input);
        assert!(
            result.unwrap().1
                == vec![
                    ClassCell::Token("my_token".to_owned()),
                    ClassCell::Integer(BigInt::from(42)),
                ]
        );
    }

    #[test]
    fn class_row_1() {
        let input = "my_token";
        let result = all_consuming(class_row).parse(input);
        assert!(result.unwrap().1 == vec![ClassCell::Token("my_token".to_owned()),]);
    }

    #[test]
    fn class_row_0_error() {
        let input = "";
        let result = all_consuming(class_row).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn class_row_prefix_colon() {
        let input = ":my_token:42";
        let result = all_consuming(class_row).parse(input);
        assert!(
            result.unwrap().1
                == vec![
                    ClassCell::Empty,
                    ClassCell::Token("my_token".to_owned()),
                    ClassCell::Integer(BigInt::from(42)),
                ]
        );
    }

    #[test]
    fn class_row_suffix_colon() {
        let input = "my_token:42:";
        let result = all_consuming(class_row).parse(input);
        assert!(
            result.unwrap().1
                == vec![
                    ClassCell::Token("my_token".to_owned()),
                    ClassCell::Integer(BigInt::from(42)),
                    ClassCell::Empty,
                ]
        );
    }

    #[test]
    fn class_row_adjacent_colon() {
        let input = "my_token::42";
        let result = all_consuming(class_row).parse(input);
        assert!(
            result.unwrap().1
                == vec![
                    ClassCell::Token("my_token".to_owned()),
                    ClassCell::Empty,
                    ClassCell::Integer(BigInt::from(42)),
                ]
        );
    }

    #[test]
    fn class_row_just_colon() {
        let input = ":";
        let result = all_consuming(class_row).parse(input);
        assert!(result.unwrap().1 == vec![ClassCell::Empty, ClassCell::Empty,]);
    }

    #[test]
    fn class_table_empty() {
        let input = "class foo {}";
        let result = all_consuming(class_table).parse(input);
        assert!(
            result.unwrap().1
                == ClassTable {
                    header: vec![ClassCell::Token("foo".to_owned()),],
                    body: vec![],
                }
        );
    }

    #[test]
    fn class_table_one_row() {
        let input = "class foo { my_field:i32 }";
        let result = all_consuming(class_table).parse(input);
        assert!(
            result.unwrap().1
                == ClassTable {
                    header: vec![ClassCell::Token("foo".to_owned()),],
                    body: vec![vec![
                        ClassCell::Token("my_field".to_owned()),
                        ClassCell::Token("i32".to_owned()),
                    ]],
                }
        );
    }

    #[test]
    fn class_table_one_row_comma() {
        let input = "class foo { my_field:i32, }";
        let result = all_consuming(class_table).parse(input);
        assert!(
            result.unwrap().1
                == ClassTable {
                    header: vec![ClassCell::Token("foo".to_owned()),],
                    body: vec![vec![
                        ClassCell::Token("my_field".to_owned()),
                        ClassCell::Token("i32".to_owned()),
                    ]],
                }
        );
    }

    #[test]
    fn class_table_just_comma_error() {
        let input = "class foo { , }";
        let result = all_consuming(class_table).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn class_table_two_rows() {
        let input = "class foo { my_field:i32, my_other_field:f64 }";
        let result = all_consuming(class_table).parse(input);
        assert!(
            result.unwrap().1
                == ClassTable {
                    header: vec![ClassCell::Token("foo".to_owned()),],
                    body: vec![vec![
                        ClassCell::Token("my_field".to_owned()),
                        ClassCell::Token("i32".to_owned()),
                    ], vec![
                        ClassCell::Token("my_other_field".to_owned()),
                        ClassCell::Token("f64".to_owned()),
                    ]],
                }
        );
    }

    #[test]
    fn class_table_metaclass_empty() {
        let input = "class foo:bar {} ";
        let result = all_consuming(class_table).parse(input);
        assert!(
            result.unwrap().1
                == ClassTable {
                    header: vec![ClassCell::Token("foo".to_owned()),ClassCell::Token("bar".to_owned())],
                    body: vec![],
                }
        );
    }

    #[test]
    fn statement_expr() {
        let input = "2";
        let result = all_consuming(statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::Expr(ClassCell::Integer(BigInt::from(2)))
        );
    }

    #[test]
    fn fn_expr() {
        let input = "fn my_function() -> u32 { 2 }";
        let result = all_consuming(function).parse(input);
        assert!(
            result.unwrap().1
                == Function {
                    header: vec![ClassCell::Token("my_function".to_owned()),],
                    params: vec![],
                    ret: vec![ClassCell::Token("u32".to_owned()),],
                    body: vec![Statement::Expr(ClassCell::Integer(BigInt::from(2)))],
                }
        );
    }

    #[test]
    fn fn_args() {
        let input = "fn my_function(arg1: i32, arg2: f64) -> u32 { 2 }";
        let result = all_consuming(function).parse(input);
        assert!(
            result.unwrap().1
                == Function {
                    header: vec![ClassCell::Token("my_function".to_owned()),],
                    params: vec![
                        vec![ClassCell::Token("arg1".to_owned()), ClassCell::Token("i32".to_owned())],
                        vec![ClassCell::Token("arg2".to_owned()), ClassCell::Token("f64".to_owned())],
                    ],
                    ret: vec![ClassCell::Token("u32".to_owned()),],
                    body: vec![Statement::Expr(ClassCell::Integer(BigInt::from(2)))],
                }
        );
    }
}
