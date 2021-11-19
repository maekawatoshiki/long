use super::lit::Literal;
use super::Located;
use crate::token::kind::IntKind;

/// An expression kind.
#[derive(Debug, Clone)]
pub enum Expr {
    /// A literal expression.
    Literal(Literal),

    /// A unary expression.
    Unary(UnaryOp, Box<Located<Expr>>),

    /// A binary expression.
    Binary(BinOp, Box<Located<Expr>>, Box<Located<Expr>>),

    /// A ternary expression.
    Ternary(Box<Located<Expr>>, Box<Located<Expr>>, Box<Located<Expr>>), // cond, then, else.

    /// An Assign expression.
    Assign(AssignOp, Box<Located<Expr>>, Box<Located<Expr>>),
}

/// A unary operator kind.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

/// A binary operator kind.
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Comma,
    LogicalOr,
    LogicalAnd,
    And,
    Eq,
    Ne,
    Or,
    Gt,
    Ge,
    Lt,
    Le,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}
/// An assignment operator kind.
/// `=  *=  /=  %=   +=  -=  >>=  <<=  &=  ^=  |=`
/// <https://timsong-cpp.github.io/cppwp/n3337/expr.ass#1>
#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    And,
    Or,
    Xor,
    None,
}

impl Located<Expr> {
    // TODO: Add tests for this.
    /// Evaluates the constant expression.
    pub fn eval_constexpr(&self) -> Option<i64> {
        match self.inner_ref() {
            Expr::Unary(UnaryOp::Not, ref val) => Some((val.eval_constexpr() == Some(0)) as i64),
            Expr::Literal(Literal::Int(IntKind::Int(i))) => Some(*i as i64),
            Expr::Literal(_) => todo!(),
            Expr::Binary(BinOp::Comma, _, ref rhs) => rhs.eval_constexpr(),
            Expr::Binary(BinOp::LogicalOr, ref lhs, ref rhs) => {
                if lhs.eval_constexpr() != Some(0) {
                    Some(1)
                } else {
                    Some((rhs.eval_constexpr() != Some(0)) as i64)
                }
            }
            Expr::Binary(BinOp::LogicalAnd, ref lhs, ref rhs) => {
                if lhs.eval_constexpr() == Some(0) {
                    Some(0)
                } else {
                    Some((rhs.eval_constexpr() != Some(0)) as i64)
                }
            }
            Expr::Binary(ref op, ref lhs, ref rhs) => {
                let lhs = lhs.eval_constexpr()?;
                let rhs = rhs.eval_constexpr()?;
                match op {
                    BinOp::And => Some(lhs & rhs),
                    BinOp::Or => Some(lhs | rhs),
                    BinOp::Shl => Some(lhs << rhs),
                    BinOp::Shr => Some(lhs >> rhs),
                    BinOp::Eq => Some((lhs == rhs) as i64),
                    BinOp::Ne => Some((lhs != rhs) as i64),
                    BinOp::Gt => Some((lhs > rhs) as i64),
                    BinOp::Ge => Some((lhs >= rhs) as i64),
                    BinOp::Lt => Some((lhs < rhs) as i64),
                    BinOp::Le => Some((lhs <= rhs) as i64),
                    BinOp::Add => Some(lhs + rhs),
                    BinOp::Sub => Some(lhs - rhs),
                    BinOp::Mul => Some(lhs * rhs),
                    BinOp::Div => Some(lhs / rhs),
                    BinOp::Rem => Some(lhs % rhs),
                    BinOp::Comma => unreachable!(),
                    BinOp::LogicalAnd => unreachable!(),
                    BinOp::LogicalOr => unreachable!(),
                }
            }
            Expr::Ternary(ref cond, ref thn, ref els) => {
                if cond.eval_constexpr() != Some(0) {
                    thn.eval_constexpr()
                } else {
                    els.eval_constexpr()
                }
            }
            Expr::Assign(AssignOp::None, _, ref rhs) => rhs.eval_constexpr(),
            Expr::Assign(_, _, _) => todo!(),
        }
    }
}
