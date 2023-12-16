enum Operator {
    Equals,
}
enum Literal {
    Number(i64),
    String(String),
    True,
    False,
    Nil,
}

enum UnaryOperator {
    Neg,
    Not,
}
enum Expression {
    Literal(Literal),
    Unary {
        operator: UnaryOperator,
        expression: Expression,
    },
    Binary {
        left: Expression,
        operator: Operator,
        right: Expression,
    },
    Grouping(Expression),
}
