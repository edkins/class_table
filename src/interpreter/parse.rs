use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::multispace0,
    combinator::{all_consuming, map, not, opt, value},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, preceded, terminated},
};
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;

use crate::interpreter::ast::{
    ClassTable, Declaration, Expression, Function, Impl, ProgramFile, Statement, Trait,
};

#[derive(Debug, PartialEq, Clone, Eq)]
enum Word {
    Class,
    Fn,
    Let,
    Mut,
    For,
    If,
    Else,
    Null,
    True,
    False,
    Impl,
    Loop,
    Trait,
    SelfKeyword,
    Token(String),
    Integer(BigInt),
    U32(u32),
    Str(String),
}

fn colon(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag(":"), multispace0)).parse(input)
}

fn comma(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag(","), multispace0)).parse(input)
}

fn dot(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("."), multispace0)).parse(input)
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

fn open_square(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("["), multispace0)).parse(input)
}

fn close_square(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("]"), multispace0)).parse(input)
}

fn arrow(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("->"), multispace0)).parse(input)
}

fn equals(input: &str) -> IResult<&str, ()> {
    value(
        (),
        terminated(terminated(tag("="), not(tag("="))), multispace0),
    )
    .parse(input)
    //    value((), terminated(tag("="), multispace0)).parse(input)
}

fn or(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("||"), multispace0)).parse(input)
}

fn and(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("&&"), multispace0)).parse(input)
}

fn eqeq(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("=="), multispace0)).parse(input)
}

fn noteq(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("!="), multispace0)).parse(input)
}

fn plus(input: &str) -> IResult<&str, ()> {
    value(
        (),
        terminated(terminated(tag("+"), not(tag("="))), multispace0),
    )
    .parse(input)
}

fn plusequal(input: &str) -> IResult<&str, ()> {
    value((), terminated(tag("+="), multispace0)).parse(input)
}

fn lt(input: &str) -> IResult<&str, ()> {
    value(
        (),
        terminated(terminated(tag("<"), not(tag("="))), multispace0),
    )
    .parse(input)
}

fn gt(input: &str) -> IResult<&str, ()> {
    value(
        (),
        terminated(terminated(tag(">"), not(tag("="))), multispace0),
    )
    .parse(input)
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

    let n = if let Some(rest) = string.strip_prefix("0x") {
        BigInt::parse_bytes(rest.as_bytes(), 16)?
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
        "let" => Ok((input, Word::Let)),
        "mut" => Ok((input, Word::Mut)),
        "for" => Ok((input, Word::For)),
        "if" => Ok((input, Word::If)),
        "else" => Ok((input, Word::Else)),
        "null" => Ok((input, Word::Null)),
        "true" => Ok((input, Word::True)),
        "false" => Ok((input, Word::False)),
        "impl" => Ok((input, Word::Impl)),
        "loop" => Ok((input, Word::Loop)),
        "trait" => Ok((input, Word::Trait)),
        "self" => Ok((input, Word::SelfKeyword)),
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

fn double_quoted_word(input: &str) -> IResult<&str, Word> {
    let (input, t) = terminated(
        nom::sequence::delimited(tag("\""), take_while(|c| c != '\"'), tag("\"")),
        multispace0,
    )
    .parse(input)?;
    Ok((input, Word::Str(t.to_owned())))
}

fn word(input: &str) -> IResult<&str, Word> {
    alt((quoted_word, double_quoted_word, unquoted_word)).parse(input)
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

fn expression_word(input: &str) -> IResult<&str, Expression> {
    let (input, t) = word.parse(input)?;
    match t {
        Word::Token(x) => Ok((input, Expression::Token(x))),
        Word::Integer(n) => Ok((input, Expression::Integer(n))),
        Word::U32(n) => Ok((input, Expression::U32(n))),
        Word::Str(s) => Ok((input, Expression::Str(s))),
        Word::Null => Ok((input, Expression::Null)),
        Word::True => Ok((input, Expression::Bool(true))),
        Word::False => Ok((input, Expression::Bool(false))),
        Word::SelfKeyword => Ok((input, Expression::SelfKeyword)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn expr_list(input: &str) -> IResult<&str, Expression> {
    let (input, elements) = delimited(
        open_square,
        separated_list0(comma, expression),
        close_square,
    )
    .parse(input)?;
    Ok((input, Expression::List(elements)))
}

fn expression_tight(input: &str) -> IResult<&str, Expression> {
    alt((
        delimited(open_paren, expression, close_paren),
        expr_list,
        expr_block,
        expression_word,
    ))
    .parse(input)
}

enum ExpressionSuffix {
    MethodCall(String, Vec<Expression>),
    Field(String),
    Build(Vec<Vec<Expression>>),
    Subscript(Vec<Vec<Expression>>),
    Call(Vec<Expression>),
}

impl ExpressionSuffix {
    fn apply(self, base: Expression) -> Expression {
        match self {
            ExpressionSuffix::MethodCall(name, args) => {
                Expression::MethodCall(Box::new(base), name, args)
            }
            ExpressionSuffix::Field(name) => Expression::FieldAccess(Box::new(base), name),
            ExpressionSuffix::Build(fields) => Expression::Build(Box::new(base), fields),
            ExpressionSuffix::Subscript(indices) => Expression::Subscript(Box::new(base), indices),
            ExpressionSuffix::Call(args) => Expression::Call(Box::new(base), args),
        }
    }
}

fn expression_method_call_suffix(input: &str) -> IResult<&str, ExpressionSuffix> {
    let (input, field) = preceded(dot, word).parse(input)?;
    let (input, args) =
        delimited(open_paren, separated_list0(comma, expression), close_paren).parse(input)?;
    match field {
        Word::Token(name) => Ok((input, ExpressionSuffix::MethodCall(name, args))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn expression_field_suffix(input: &str) -> IResult<&str, ExpressionSuffix> {
    let (input, field) = preceded(dot, word).parse(input)?;
    match field {
        Word::Token(name) => Ok((input, ExpressionSuffix::Field(name))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn expression_build_suffix(input: &str) -> IResult<&str, ExpressionSuffix> {
    map(expression_table, ExpressionSuffix::Build).parse(input)
}

fn expression_subscript_suffix(input: &str) -> IResult<&str, ExpressionSuffix> {
    let (input, indices) =
        delimited(open_square, expression_table_contents, close_square).parse(input)?;
    Ok((input, ExpressionSuffix::Subscript(indices)))
}

fn expression_call_suffix(input: &str) -> IResult<&str, ExpressionSuffix> {
    let (input, args) =
        delimited(open_paren, separated_list0(comma, expression), close_paren).parse(input)?;
    Ok((input, ExpressionSuffix::Call(args)))
}

fn term_suffix(curly: bool) -> impl Fn(&str) -> IResult<&str, ExpressionSuffix> {
    move |input| {
        if curly {
            alt((
                expression_method_call_suffix,
                expression_field_suffix,
                expression_subscript_suffix,
                expression_call_suffix,
                expression_build_suffix,
            ))
            .parse(input)
        } else {
            alt((
                expression_method_call_suffix,
                expression_field_suffix,
                expression_subscript_suffix,
                expression_call_suffix,
            ))
            .parse(input)
        }
    }
}

fn expression_suffixed(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    move |input| {
        let (input, mut base) = expression_tight(input)?;
        let (input, suffixes) = many0(term_suffix(curly)).parse(input)?;
        for suffix in suffixes.into_iter() {
            base = suffix.apply(base);
        }
        Ok((input, base))
    }
}

fn expression_term(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    move |input| {
        if curly {
            alt((expression_if, expression_suffixed(curly))).parse(input)
        } else {
            expression_suffixed(curly).parse(input)
        }
    }
}

struct BinopSuffix {
    binop: String,
    rhs: Expression,
}

impl BinopSuffix {
    fn apply(self, lhs: Expression) -> Expression {
        match &self.binop as &str {
            "&&" => Expression::And(Box::new(lhs), Box::new(self.rhs)),
            "||" => Expression::Or(Box::new(lhs), Box::new(self.rhs)),
            _ => Expression::Call(Box::new(Expression::Token(self.binop)), vec![lhs, self.rhs]),
        }
    }

    fn mapper(binop: &str) -> impl Fn(Expression) -> BinopSuffix {
        move |rhs| BinopSuffix {
            binop: binop.to_owned(),
            rhs,
        }
    }
}

fn addition_suffix(curly: bool) -> impl Fn(&str) -> IResult<&str, BinopSuffix> {
    move |input| {
        map(
            preceded(plus, expression_term(curly)),
            BinopSuffix::mapper("+"),
        )
        .parse(input)
    }
}

fn expression_addition(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    move |input| {
        let (input, mut base) = expression_term(curly)(input)?;
        let (input, suffixes) = many0(addition_suffix(curly)).parse(input)?;
        for suffix in suffixes.into_iter() {
            base = suffix.apply(base);
        }
        Ok((input, base))
    }
}

fn rel_suffix(curly: bool) -> impl Fn(&str) -> IResult<&str, BinopSuffix> {
    move |input| {
        alt((
            map(
                preceded(eqeq, expression_addition(curly)),
                BinopSuffix::mapper("=="),
            ),
            map(
                preceded(noteq, expression_addition(curly)),
                BinopSuffix::mapper("!="),
            ),
            map(
                preceded(lt, expression_addition(curly)),
                BinopSuffix::mapper("<"),
            ),
            map(
                preceded(gt, expression_addition(curly)),
                BinopSuffix::mapper(">"),
            ),
        ))
        .parse(input)
    }
}

fn expression_rel(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    move |input| {
        let (input, mut base) = expression_addition(curly)(input)?;
        let (input, suffixes) = opt(rel_suffix(curly)).parse(input)?;
        for suffix in suffixes.into_iter() {
            base = suffix.apply(base);
        }
        Ok((input, base))
    }
}

fn and_suffix(curly: bool) -> impl Fn(&str) -> IResult<&str, BinopSuffix> {
    move |input| {
        map(
            preceded(and, expression_rel(curly)),
            BinopSuffix::mapper("&&"),
        )
        .parse(input)
    }
}

fn expression_and(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    move |input| {
        let (input, mut base) = expression_rel(curly)(input)?;
        let (input, suffixes) = many0(and_suffix(curly)).parse(input)?;
        for suffix in suffixes.into_iter() {
            base = suffix.apply(base);
        }
        Ok((input, base))
    }
}

fn or_suffix(curly: bool) -> impl Fn(&str) -> IResult<&str, BinopSuffix> {
    move |input| {
        map(
            preceded(or, expression_and(curly)),
            BinopSuffix::mapper("||"),
        )
        .parse(input)
    }
}

fn expression_or(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    move |input| {
        let (input, mut base) = expression_and(curly)(input)?;
        let (input, suffixes) = many0(or_suffix(curly)).parse(input)?;
        for suffix in suffixes.into_iter() {
            base = suffix.apply(base);
        }
        Ok((input, base))
    }
}

fn expression_base(curly: bool) -> impl Fn(&str) -> IResult<&str, Expression> {
    expression_or(curly)
}

fn expression(input: &str) -> IResult<&str, Expression> {
    expression_base(true)(input)
}

fn class_cell(input: &str) -> IResult<&str, Expression> {
    let (input, base) = opt(expression_base(false)).parse(input)?;
    Ok((input, base.unwrap_or(Expression::Empty)))
}

fn class_row(input: &str) -> IResult<&str, Vec<Expression>> {
    let (input, result) = separated_list1(colon, class_cell).parse(input)?;
    // Don't allow a single empty cell (this would correspond to an empty input)
    if result == vec![Expression::Empty] {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    } else {
        Ok((input, result))
    }
}

fn expression_table_contents(input: &str) -> IResult<&str, Vec<Vec<Expression>>> {
    map(
        opt(terminated(separated_list1(comma, class_row), opt(comma))),
        Option::unwrap_or_default,
    )
    .parse(input)
}

fn expression_table(input: &str) -> IResult<&str, Vec<Vec<Expression>>> {
    delimited(open_brace, expression_table_contents, close_brace).parse(input)
}

fn class_table(input: &str) -> IResult<&str, ClassTable> {
    let (input, _) = specific_word(Word::Class).parse(input)?;
    let (input, header) = class_row(input)?;
    let (input, body) = expression_table(input)?;
    Ok((input, ClassTable { header, body }))
}

struct AssignSuffix {
    pub op: String,
    pub value: Expression,
}

impl AssignSuffix {
    pub fn apply(self, expr: Expression) -> Statement {
        Statement::Assign(self.op, expr, self.value)
    }

    pub fn mapper(op: &str) -> impl Fn(Expression) -> AssignSuffix {
        move |value| AssignSuffix {
            op: op.to_owned(),
            value,
        }
    }
}

fn assign_suffix(input: &str) -> IResult<&str, AssignSuffix> {
    alt((
        map(preceded(equals, expression), AssignSuffix::mapper("=")),
        map(preceded(plusequal, expression), AssignSuffix::mapper("+=")),
    ))
    .parse(input)
}

fn assign_statement(input: &str) -> IResult<&str, Statement> {
    let (input, expr) = expression(input)?;
    let (input, suffix) = assign_suffix.parse(input)?;
    Ok((input, suffix.apply(expr)))
}

fn let_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = specific_word(Word::Let).parse(input)?;
    let (input, var) = class_row(input)?;
    let (input, _) = equals(input)?;
    let (input, expr) = expression(input)?;
    Ok((input, Statement::Let(var, expr, false)))
}

fn let_mut_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = specific_word(Word::Let).parse(input)?;
    let (input, _) = specific_word(Word::Mut).parse(input)?;
    let (input, var) = class_row(input)?;
    let (input, _) = equals(input)?;
    let (input, expr) = expression(input)?;
    Ok((input, Statement::Let(var, expr, true)))
}

fn for_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = specific_word(Word::For).parse(input)?;
    let (input, header) = class_row(input)?;
    let (input, body) = statement_block(input)?;
    Ok((input, Statement::For(header, body)))
}

fn loop_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = specific_word(Word::Loop).parse(input)?;
    let (input, body) = statement_block(input)?;
    Ok((input, Statement::Loop(body)))
}

fn expression_if(input: &str) -> IResult<&str, Expression> {
    let original_input = input;
    let (input, _) = specific_word(Word::If).parse(input)?;
    let (input, cond) = class_cell(input)?;
    let (input, then_branch) = expr_block(input)?;
    let (input, else_branch) = opt(preceded(
        specific_word(Word::Else),
        alt((expr_block, expression_if)),
    ))
    .parse(input)?;
    println!("Parsed if expression: {}", original_input);
    Ok((
        input,
        Expression::If(
            Box::new(cond),
            Box::new(then_branch),
            Box::new(else_branch.unwrap_or(Expression::Empty)),
        ),
    ))
}

// fn statement(input: &str) -> IResult<&str, Statement> {
//     alt((
//         let_mut_statement,
//         let_statement,
//         for_statement,
//         expr_or_assign_statement,
//     ))
//     .parse(input)
// }

#[derive(Clone, Debug, Eq, PartialEq)]
enum BlockItem {
    BlockStatement(Statement),
    Statement(Statement),
    BlockExpr(Expression),
    Expr(Expression),
    Semicolon,
}

fn block_item(input: &str) -> IResult<&str, BlockItem> {
    alt((
        map(for_statement, BlockItem::BlockStatement),
        map(loop_statement, BlockItem::BlockStatement),
        map(let_mut_statement, BlockItem::Statement),
        map(let_statement, BlockItem::Statement),
        map(assign_statement, BlockItem::Statement),
        map(expression_if, BlockItem::BlockExpr),
        map(expression, BlockItem::Expr),
        map(semicolon, |_| BlockItem::Semicolon),
    ))
    .parse(input)
}

fn statement_block(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input, block) = expr_block(input)?;
    if let Expression::Block(mut stmts, result) = block {
        if *result != Expression::Empty {
            stmts.push(Statement::Expr(*result));
        }
        Ok((input, stmts))
    } else {
        unreachable!()
    }
}

fn expr_block(input: &str) -> IResult<&str, Expression> {
    let (input, _) = open_brace.parse(input)?;
    let (input, items) = many0(block_item).parse(input)?;
    let mut needs_semicolon = false;
    let mut was_expr = false;
    let mut stmts = vec![];
    for item in items {
        if item == BlockItem::Semicolon {
            needs_semicolon = false;
            was_expr = false;
        } else {
            assert!(!needs_semicolon);
            match item {
                BlockItem::BlockStatement(stmt) => {
                    stmts.push(stmt);
                    needs_semicolon = false;
                    was_expr = false;
                }
                BlockItem::Statement(stmt) => {
                    stmts.push(stmt);
                    needs_semicolon = true;
                    was_expr = false;
                }
                BlockItem::BlockExpr(expr) => {
                    stmts.push(Statement::Expr(expr));
                    needs_semicolon = false;
                    was_expr = true;
                }
                BlockItem::Expr(expr) => {
                    stmts.push(Statement::Expr(expr));
                    needs_semicolon = true;
                    was_expr = true;
                }
                BlockItem::Semicolon => {
                    unreachable!()
                }
            }
        }
    }
    let expr = if was_expr {
        match stmts.pop().expect("Stack shouldn't be empty at this point") {
            Statement::Expr(expr) => expr,
            _ => {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )));
            }
        }
    } else {
        Expression::Empty
    };
    let (input, _) = close_brace.parse(input)?;
    Ok((input, Expression::Block(stmts, Box::new(expr))))
}

fn function(input: &str) -> IResult<&str, Function> {
    let (input, _) = specific_word(Word::Fn).parse(input)?;
    // TODO: function metaclasses specified by colon
    // let (input, header) = class_row(input)?;
    let (input, name) = expression_word(input)?;
    let (input, params) =
        delimited(open_paren, separated_list0(comma, class_row), close_paren).parse(input)?;
    let (input, ret) = preceded(arrow, class_row).parse(input)?;
    let (input, body) = expr_block(input)?;
    Ok((
        input,
        Function {
            header: vec![name],
            params,
            ret,
            body,
        },
    ))
}

fn function_semicolon(input: &str) -> IResult<&str, Function> {
    let (input, _) = specific_word(Word::Fn).parse(input)?;
    let (input, name) = expression_word(input)?;
    let (input, params) =
        delimited(open_paren, separated_list0(comma, class_row), close_paren).parse(input)?;
    let (input, ret) = preceded(arrow, class_row).parse(input)?;
    let (input, _) = semicolon(input)?;
    Ok((
        input,
        Function {
            header: vec![name],
            params,
            ret,
            body: Expression::Empty,
        },
    ))
}

fn impl_block(input: &str) -> IResult<&str, Impl> {
    let (input, _) = specific_word(Word::Impl).parse(input)?;
    let (input, header) = class_row(input)?;
    let (input, _) = open_brace(input)?;
    let (input, methods) = many0(function).parse(input)?;
    let (input, _) = close_brace(input)?;
    Ok((input, Impl { header, methods }))
}

fn trait_block(input: &str) -> IResult<&str, Trait> {
    let (input, _) = specific_word(Word::Trait).parse(input)?;
    let (input, header) = class_row(input)?;
    let (input, _) = open_brace(input)?;
    let (input, methods) = many0(function_semicolon).parse(input)?;
    let (input, _) = close_brace(input)?;
    Ok((input, Trait { header, methods }))
}

fn declaration(input: &str) -> IResult<&str, Declaration> {
    alt((
        map(class_table, Declaration::Class),
        map(function, Declaration::Fn),
        map(impl_block, Declaration::Impl),
        map(trait_block, Declaration::Trait),
    ))
    .parse(input)
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
    fn expression_token() {
        let input = "my_token";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("my_token".to_owned()));
    }

    #[test]
    fn expression_token_space() {
        let input = "my_token ";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("my_token".to_owned()));
    }

    #[test]
    fn expression_int() {
        let input = "42";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Integer(BigInt::from(42)));
    }

    #[test]
    fn expression_hex() {
        let input = "0x2A";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Integer(BigInt::from(42)));
    }

    #[test]
    fn expression_hex_lower() {
        let input = "0x2a";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Integer(BigInt::from(42)));
    }

    #[test]
    fn expression_hex_empty_error() {
        let input = "0x";
        let result = all_consuming(expression).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn expression_u32() {
        let input = "42u32";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::U32(42));
    }

    #[test]
    fn expression_hex_u32() {
        let input = "0x2Au32";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::U32(42));
    }

    #[test]
    fn expression_int_suffix_error() {
        let input = "42abc";
        let result = all_consuming(expression).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn expression_int_prefix() {
        let input = "abc42";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("abc42".to_owned()));
    }

    #[test]
    fn expression_underscore_int() {
        let input = "_42";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("_42".to_owned()));
    }

    #[test]
    fn expression_quoted_class() {
        let input = "'class'";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("class".to_owned()));
    }

    #[test]
    fn expression_quoted_empty() {
        let input = "''";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("".to_owned()));
    }

    #[test]
    fn expression_quoted_number() {
        let input = "'42'";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("42".to_owned()));
    }

    #[test]
    fn expression_quoted_space() {
        let input = "' '";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token(" ".to_owned()));
    }

    #[test]
    fn expression_quoted_punctuation() {
        let input = "'!@#$%^&*().-'";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Token("!@#$%^&*().-".to_owned()));
    }

    #[test]
    fn expression_string_empty() {
        let input = "\"\"";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Str("".to_owned()));
    }

    #[test]
    fn expression_string() {
        let input = "\"hello world\"";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Str("hello world".to_owned()));
    }

    #[test]
    fn expression_field() {
        let input = "my_token.my_field";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::FieldAccess(
                    Box::new(Expression::Token("my_token".to_owned())),
                    "my_field".to_owned()
                )
        );
    }

    #[test]
    fn expression_fields() {
        let input = "my_token.my_field.my_other_field";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::FieldAccess(
                    Box::new(Expression::FieldAccess(
                        Box::new(Expression::Token("my_token".to_owned())),
                        "my_field".to_owned()
                    )),
                    "my_other_field".to_owned()
                )
        );
    }

    #[test]
    fn expression_build_empty() {
        let input = "Foo {}";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Build(Box::new(Expression::Token("Foo".to_owned())), vec![])
        );
    }

    #[test]
    fn expression_build_1() {
        let input = "Foo { bar: 42 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Build(
                    Box::new(Expression::Token("Foo".to_owned())),
                    vec![vec![
                        Expression::Token("bar".to_owned()),
                        Expression::Integer(BigInt::from(42))
                    ],]
                )
        );
    }

    #[test]
    fn expression_build_2() {
        let input = "Foo { bar: 42, baz: 43 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Build(
                    Box::new(Expression::Token("Foo".to_owned())),
                    vec![
                        vec![
                            Expression::Token("bar".to_owned()),
                            Expression::Integer(BigInt::from(42))
                        ],
                        vec![
                            Expression::Token("baz".to_owned()),
                            Expression::Integer(BigInt::from(43))
                        ],
                    ]
                )
        );
    }

    #[test]
    fn expression_subscript() {
        let input = "a[b]";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Subscript(
                    Box::new(Expression::Token("a".to_owned())),
                    vec![vec![Expression::Token("b".to_owned())]]
                )
        );
    }

    #[test]
    fn expression_subscript2() {
        let input = "a[b,c]";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Subscript(
                    Box::new(Expression::Token("a".to_owned())),
                    vec![
                        vec![Expression::Token("b".to_owned())],
                        vec![Expression::Token("c".to_owned())]
                    ]
                )
        );
    }

    #[test]
    fn expression_subscript_colon() {
        let input = "a[b:c]";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Subscript(
                    Box::new(Expression::Token("a".to_owned())),
                    vec![vec![
                        Expression::Token("b".to_owned()),
                        Expression::Token("c".to_owned())
                    ]]
                )
        );
    }

    #[test]
    fn expression_call_empty() {
        let input = "a()";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(Box::new(Expression::Token("a".to_owned())), vec![])
        );
    }

    #[test]
    fn expression_call() {
        let input = "a(b)";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token("a".to_owned())),
                    vec![Expression::Token("b".to_owned())]
                )
        );
    }

    #[test]
    fn expression_call_2() {
        let input = "a(b, c)";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token("a".to_owned())),
                    vec![
                        Expression::Token("b".to_owned()),
                        Expression::Token("c".to_owned())
                    ]
                )
        );
    }

    #[test]
    fn expression_block_empty() {
        let input = "{}";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Block(vec![], Box::new(Expression::Empty)));
    }

    #[test]
    fn expression_block_expression() {
        let input = "{ 42 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Block(vec![], Box::new(Expression::Integer(BigInt::from(42))))
        );
    }

    #[test]
    fn expression_block_statement() {
        let input = "{ let x = 42; }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Block(
                    vec![Statement::Let(
                        vec![Expression::Token("x".to_owned())],
                        Expression::Integer(BigInt::from(42)),
                        false
                    )],
                    Box::new(Expression::Empty)
                )
        );
    }

    #[test]
    fn expression_block_statement_expression() {
        let input = "{ let x = 42; x }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Block(
                    vec![Statement::Let(
                        vec![Expression::Token("x".to_owned())],
                        Expression::Integer(BigInt::from(42)),
                        false
                    )],
                    Box::new(Expression::Token("x".to_owned())),
                )
        );
    }

    #[test]
    fn class_cell_build_error() {
        let input = "Foo { bar: 42, baz: 43 }";
        let result = all_consuming(class_cell).parse(input);
        assert!(result.is_err());
    }

    #[test]
    fn class_cell_build_paren() {
        let input = "(Foo { bar: 42, baz: 43 })";
        let result = all_consuming(class_cell).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Build(
                    Box::new(Expression::Token("Foo".to_owned())),
                    vec![
                        vec![
                            Expression::Token("bar".to_owned()),
                            Expression::Integer(BigInt::from(42))
                        ],
                        vec![
                            Expression::Token("baz".to_owned()),
                            Expression::Integer(BigInt::from(43))
                        ],
                    ]
                )
        );
    }

    #[test]
    fn class_row_2() {
        let input = "my_token:42";
        let result = all_consuming(class_row).parse(input);
        assert!(
            result.unwrap().1
                == vec![
                    Expression::Token("my_token".to_owned()),
                    Expression::Integer(BigInt::from(42)),
                ]
        );
    }

    #[test]
    fn class_row_1() {
        let input = "my_token";
        let result = all_consuming(class_row).parse(input);
        assert!(result.unwrap().1 == vec![Expression::Token("my_token".to_owned()),]);
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
                    Expression::Empty,
                    Expression::Token("my_token".to_owned()),
                    Expression::Integer(BigInt::from(42)),
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
                    Expression::Token("my_token".to_owned()),
                    Expression::Integer(BigInt::from(42)),
                    Expression::Empty,
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
                    Expression::Token("my_token".to_owned()),
                    Expression::Empty,
                    Expression::Integer(BigInt::from(42)),
                ]
        );
    }

    #[test]
    fn class_row_just_colon() {
        let input = ":";
        let result = all_consuming(class_row).parse(input);
        assert!(result.unwrap().1 == vec![Expression::Empty, Expression::Empty,]);
    }

    #[test]
    fn expression_table_empty() {
        let input = "{}";
        let result = all_consuming(expression_table).parse(input);
        assert!(result.unwrap().1.is_empty());
    }

    #[test]
    fn class_table_empty() {
        let input = "class foo {}";
        let result = all_consuming(class_table).parse(input);
        assert!(
            result.unwrap().1
                == ClassTable {
                    header: vec![Expression::Token("foo".to_owned()),],
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
                    header: vec![Expression::Token("foo".to_owned()),],
                    body: vec![vec![
                        Expression::Token("my_field".to_owned()),
                        Expression::Token("i32".to_owned()),
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
                    header: vec![Expression::Token("foo".to_owned()),],
                    body: vec![vec![
                        Expression::Token("my_field".to_owned()),
                        Expression::Token("i32".to_owned()),
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
                    header: vec![Expression::Token("foo".to_owned()),],
                    body: vec![
                        vec![
                            Expression::Token("my_field".to_owned()),
                            Expression::Token("i32".to_owned()),
                        ],
                        vec![
                            Expression::Token("my_other_field".to_owned()),
                            Expression::Token("f64".to_owned()),
                        ]
                    ],
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
                    header: vec![
                        Expression::Token("foo".to_owned()),
                        Expression::Token("bar".to_owned())
                    ],
                    body: vec![],
                }
        );
    }

    #[test]
    fn statement_let() {
        let input = "let x = 2";
        let result = all_consuming(let_statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::Let(
                    vec![Expression::Token("x".to_owned())],
                    Expression::Integer(BigInt::from(2)),
                    false,
                )
        );
    }

    #[test]
    fn statement_let_mut() {
        let input = "let mut x = 2";
        let result = all_consuming(let_mut_statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::Let(
                    vec![Expression::Token("x".to_owned())],
                    Expression::Integer(BigInt::from(2)),
                    true,
                )
        );
    }

    #[test]
    fn statement_for() {
        let input = "for x : y { 2; }";
        let result = all_consuming(for_statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::For(
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::Token("y".to_owned())
                    ],
                    vec![Statement::Expr(Expression::Integer(BigInt::from(2)))],
                )
        );
    }

    #[test]
    fn statement_for_method() {
        let input = "for x : y.method() { 2; }";
        let result = all_consuming(for_statement).parse(input);
        println!("{:?}", result);
        assert!(
            result.unwrap().1
                == Statement::For(
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::MethodCall(
                            Box::new(Expression::Token("y".to_owned())),
                            "method".to_owned(),
                            vec![]
                        )
                    ],
                    vec![Statement::Expr(Expression::Integer(BigInt::from(2)))],
                )
        );
    }

    #[test]
    fn expr_if() {
        let input = "if x { 2 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::If(
                    Box::new(Expression::Token("x".to_owned())),
                    Box::new(Expression::Block(
                        vec![],
                        Box::new(Expression::Integer(BigInt::from(2)))
                    )),
                    Box::new(Expression::Empty),
                )
        );
    }

    #[test]
    fn expr_if_else() {
        let input = "if x { 2 } else { 3 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::If(
                    Box::new(Expression::Token("x".to_owned())),
                    Box::new(Expression::Block(
                        vec![],
                        Box::new(Expression::Integer(BigInt::from(2)))
                    )),
                    Box::new(Expression::Block(
                        vec![],
                        Box::new(Expression::Integer(BigInt::from(3)))
                    )),
                )
        );
    }

    #[test]
    fn expr_else_if() {
        let input = "if x { 2 } else if y { 3 } else { 4 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::If(
                    Box::new(Expression::Token("x".to_owned())),
                    Box::new(Expression::Block(
                        vec![],
                        Box::new(Expression::Integer(BigInt::from(2)))
                    )),
                    Box::new(Expression::If(
                        Box::new(Expression::Token("y".to_owned())),
                        Box::new(Expression::Block(
                            vec![],
                            Box::new(Expression::Integer(BigInt::from(3)))
                        )),
                        Box::new(Expression::Block(
                            vec![],
                            Box::new(Expression::Integer(BigInt::from(4)))
                        )),
                    )),
                )
        );
    }

    #[test]
    fn expr_block_if_else_no_semicolon() {
        let input = "{ if x { 2; } else { 3; } 4 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Block(
                    vec![Statement::Expr(Expression::If(
                        Box::new(Expression::Token("x".to_owned())),
                        Box::new(Expression::Block(
                            vec![Statement::Expr(Expression::Integer(BigInt::from(2)))],
                            Box::new(Expression::Empty)
                        )),
                        Box::new(Expression::Block(
                            vec![Statement::Expr(Expression::Integer(BigInt::from(3)))],
                            Box::new(Expression::Empty)
                        )),
                    ))],
                    Box::new(Expression::Integer(BigInt::from(4)))
                )
        );
    }

    #[test]
    fn expr_block_if_else_statement_no_semicolon() {
        let input = "{ if x { 2; } else { 3; } x = 5; 4 }";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Block(
                    vec![
                        Statement::Expr(Expression::If(
                            Box::new(Expression::Token("x".to_owned())),
                            Box::new(Expression::Block(
                                vec![Statement::Expr(Expression::Integer(BigInt::from(2)))],
                                Box::new(Expression::Empty)
                            )),
                            Box::new(Expression::Block(
                                vec![Statement::Expr(Expression::Integer(BigInt::from(3)))],
                                Box::new(Expression::Empty)
                            )),
                        )),
                        Statement::Assign(
                            "=".to_owned(),
                            Expression::Token("x".to_owned()),
                            Expression::Integer(BigInt::from(5)),
                        ),
                    ],
                    Box::new(Expression::Integer(BigInt::from(4)))
                )
        );
    }

    #[test]
    fn fn_expr() {
        let input = "fn my_function() -> u32 { 2 }";
        let result = all_consuming(function).parse(input);
        assert!(
            result.unwrap().1
                == Function {
                    header: vec![Expression::Token("my_function".to_owned()),],
                    params: vec![],
                    ret: vec![Expression::Token("u32".to_owned()),],
                    body: Expression::Block(vec![], Box::new(Expression::Integer(BigInt::from(2))))
                }
        );
    }

    #[test]
    fn fn_expr_semicolon() {
        let input = "fn my_function() -> u32 { 2; }";
        let result = all_consuming(function).parse(input);
        assert!(
            result.unwrap().1
                == Function {
                    header: vec![Expression::Token("my_function".to_owned()),],
                    params: vec![],
                    ret: vec![Expression::Token("u32".to_owned()),],
                    body: Expression::Block(
                        vec![Statement::Expr(Expression::Integer(BigInt::from(2))),],
                        Box::new(Expression::Empty)
                    ),
                }
        );
    }

    #[test]
    fn fn_empty() {
        let input = "fn my_function() -> u32 { }";
        let result = all_consuming(function).parse(input);
        assert!(
            result.unwrap().1
                == Function {
                    header: vec![Expression::Token("my_function".to_owned()),],
                    params: vec![],
                    ret: vec![Expression::Token("u32".to_owned()),],
                    body: Expression::Block(vec![], Box::new(Expression::Empty))
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
                    header: vec![Expression::Token("my_function".to_owned()),],
                    params: vec![
                        vec![
                            Expression::Token("arg1".to_owned()),
                            Expression::Token("i32".to_owned())
                        ],
                        vec![
                            Expression::Token("arg2".to_owned()),
                            Expression::Token("f64".to_owned())
                        ],
                    ],
                    ret: vec![Expression::Token("u32".to_owned()),],
                    body: Expression::Block(vec![], Box::new(Expression::Integer(BigInt::from(2))))
                }
        );
    }

    #[test]
    fn expr_eqeq() {
        let input = "x == y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token("==".to_owned())),
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::Token("y".to_owned())
                    ]
                )
        );
    }

    #[test]
    fn expr_noteq() {
        let input = "x != y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token("!=".to_owned())),
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::Token("y".to_owned())
                    ]
                )
        );
    }

    #[test]
    fn expr_lt() {
        let input = "x < y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token("<".to_owned())),
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::Token("y".to_owned())
                    ]
                )
        );
    }

    #[test]
    fn expr_gt() {
        let input = "x > y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token(">".to_owned())),
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::Token("y".to_owned())
                    ]
                )
        );
    }

    #[test]
    fn expr_or() {
        let input = "x || y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Or(
                    Box::new(Expression::Token("x".to_owned())),
                    Box::new(Expression::Token("y".to_owned()))
                )
        );
    }

    #[test]
    fn expr_and() {
        let input = "x && y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::And(
                    Box::new(Expression::Token("x".to_owned())),
                    Box::new(Expression::Token("y".to_owned()))
                )
        );
    }

    #[test]
    fn expr_precedence() {
        let input = "a < b && c == d || e == f && g != h";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Or(
                    Box::new(Expression::And(
                        Box::new(Expression::Call(
                            Box::new(Expression::Token("<".to_owned())),
                            vec![
                                Expression::Token("a".to_owned()),
                                Expression::Token("b".to_owned())
                            ]
                        )),
                        Box::new(Expression::Call(
                            Box::new(Expression::Token("==".to_owned())),
                            vec![
                                Expression::Token("c".to_owned()),
                                Expression::Token("d".to_owned())
                            ]
                        ))
                    )),
                    Box::new(Expression::And(
                        Box::new(Expression::Call(
                            Box::new(Expression::Token("==".to_owned())),
                            vec![
                                Expression::Token("e".to_owned()),
                                Expression::Token("f".to_owned())
                            ]
                        )),
                        Box::new(Expression::Call(
                            Box::new(Expression::Token("!=".to_owned())),
                            vec![
                                Expression::Token("g".to_owned()),
                                Expression::Token("h".to_owned())
                            ]
                        ))
                    ))
                )
        );
    }

    #[test]
    fn expr_add() {
        let input = "x + y";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::Call(
                    Box::new(Expression::Token("+".to_owned())),
                    vec![
                        Expression::Token("x".to_owned()),
                        Expression::Token("y".to_owned())
                    ]
                )
        );
    }

    #[test]
    fn statement_assign() {
        let input = "x = 5";
        let result = all_consuming(assign_statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::Assign(
                    "=".to_owned(),
                    Expression::Token("x".to_owned()),
                    Expression::Integer(BigInt::from(5))
                )
        );
    }

    #[test]
    fn statement_plus_assign() {
        let input = "x += 5";
        let result = all_consuming(assign_statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::Assign(
                    "+=".to_owned(),
                    Expression::Token("x".to_owned()),
                    Expression::Integer(BigInt::from(5))
                )
        );
    }

    #[test]
    fn expr_null() {
        let input = "null";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Null);
    }

    #[test]
    fn function_with_if() {
        let input = "fn test() -> u32 { if x { 2 } else { 3 } }";
        let result = all_consuming(function).parse(input);
        assert!(
            result.unwrap().1
                == Function {
                    header: vec![Expression::Token("test".to_owned())],
                    params: vec![],
                    ret: vec![Expression::Token("u32".to_owned())],
                    body: Expression::Block(
                        vec![],
                        Box::new(Expression::If(
                            Box::new(Expression::Token("x".to_owned())),
                            Box::new(Expression::Block(
                                vec![],
                                Box::new(Expression::Integer(BigInt::from(2)))
                            )),
                            Box::new(Expression::Block(
                                vec![],
                                Box::new(Expression::Integer(BigInt::from(3)))
                            )),
                        )),
                    )
                }
        );
    }

    #[test]
    fn expr_true() {
        let input = "true";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Bool(true));
    }

    #[test]
    fn expr_false() {
        let input = "false";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::Bool(false));
    }

    #[test]
    fn loop_empty() {
        let input = "loop {}";
        let result = all_consuming(loop_statement).parse(input);
        assert!(result.unwrap().1 == Statement::Loop(vec![]));
    }

    #[test]
    fn loop_stmt() {
        let input = "loop { 1; 2; 3; }";
        let result = all_consuming(loop_statement).parse(input);
        assert!(
            result.unwrap().1
                == Statement::Loop(vec![
                    Statement::Expr(Expression::Integer(BigInt::from(1))),
                    Statement::Expr(Expression::Integer(BigInt::from(2))),
                    Statement::Expr(Expression::Integer(BigInt::from(3))),
                ])
        );
    }

    #[test]
    fn impl_empty() {
        let input = "impl a :: c {}";
        let result = all_consuming(impl_block).parse(input);
        assert!(
            result.unwrap().1
                == Impl {
                    header: vec![
                        Expression::Token("a".to_owned()),
                        Expression::Empty,
                        Expression::Token("c".to_owned())
                    ],
                    methods: vec![]
                }
        );
    }

    #[test]
    fn impl_mid_empty() {
        let input = "impl a : b : c {}";
        let result = all_consuming(impl_block).parse(input);
        assert!(
            result.unwrap().1
                == Impl {
                    header: vec![
                        Expression::Token("a".to_owned()),
                        Expression::Token("b".to_owned()),
                        Expression::Token("c".to_owned())
                    ],
                    methods: vec![]
                }
        );
    }

    #[test]
    fn trait_empty() {
        let input = "trait a {}";
        let result = all_consuming(trait_block).parse(input);
        assert!(
            result.unwrap().1
                == Trait {
                    header: vec![Expression::Token("a".to_owned())],
                    methods: vec![]
                }
        );
    }

    #[test]
    fn trait_fn() {
        let input = "trait a { fn b() -> u32; }";
        let result = all_consuming(trait_block).parse(input);
        assert!(
            result.unwrap().1
                == Trait {
                    header: vec![Expression::Token("a".to_owned())],
                    methods: vec![Function {
                        header: vec![Expression::Token("b".to_owned())],
                        params: vec![],
                        ret: vec![Expression::Token("u32".to_owned())],
                        body: Expression::Empty,
                    }]
                }
        );
    }

    #[test]
    fn trait_fn_self() {
        let input = "trait a { fn b(self) -> u32; }";
        let result = all_consuming(trait_block).parse(input);
        assert!(
            result.unwrap().1
                == Trait {
                    header: vec![Expression::Token("a".to_owned())],
                    methods: vec![Function {
                        header: vec![Expression::Token("b".to_owned())],
                        params: vec![vec![Expression::SelfKeyword]],
                        ret: vec![Expression::Token("u32".to_owned())],
                        body: Expression::Empty,
                    }]
                }
        );
    }

    #[test]
    fn method_call_noargs() {
        let input = "a.b()";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::MethodCall(
                    Box::new(Expression::Token("a".to_owned())),
                    "b".to_owned(),
                    vec![]
                )
        );
    }

    #[test]
    fn method_call_args() {
        let input = "a.b(1, 2, 3)";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::MethodCall(
                    Box::new(Expression::Token("a".to_owned())),
                    "b".to_owned(),
                    vec![
                        Expression::Integer(BigInt::from(1)),
                        Expression::Integer(BigInt::from(2)),
                        Expression::Integer(BigInt::from(3)),
                    ]
                )
        );
    }

    #[test]
    fn trait_param() {
        let input = "trait a[T] {}";
        let result = all_consuming(trait_block).parse(input);
        assert!(
            result.unwrap().1
                == Trait {
                    header: vec![Expression::Subscript(
                        Box::new(Expression::Token("a".to_owned())),
                        vec![vec![Expression::Token("T".to_owned())]]
                    ),],
                    methods: vec![]
                }
        );
    }

    #[test]
    fn list_empty() {
        let input = "[]";
        let result = all_consuming(expression).parse(input);
        assert!(result.unwrap().1 == Expression::List(vec![]));
    }

    #[test]
    fn list_values() {
        let input = "[1, 2, 3]";
        let result = all_consuming(expression).parse(input);
        assert!(
            result.unwrap().1
                == Expression::List(vec![
                    Expression::Integer(BigInt::from(1)),
                    Expression::Integer(BigInt::from(2)),
                    Expression::Integer(BigInt::from(3)),
                ])
        );
    }
}
