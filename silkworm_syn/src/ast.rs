use crate::ptr::P;
use crate::symbol::{Interner, Symbol};
use crate::token;
use crate::Span;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Sigil {
    /// Global persistent identifier, `$`.
    Global,
    /// Node-scoped persistent identifier, `@`.
    Node,
    /// File-scoped persistent identifier, `@@`.
    File,
    /// Local ephemeral identifier (no sigil).
    Local,
}

/// A variable identifier with a sigil.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Var {
    pub sigil: Sigil,
    pub symbol: Symbol,
    /// `Some` if the symbol is a keyword.
    pub keyword: Option<token::Keyword>,
    pub span: Span,
}

#[derive(Clone, Eq, Debug)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<PathSegment>,
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq<PathSegment> for Path {
    fn eq(&self, other: &PathSegment) -> bool {
        self.segments.len() == 1 && &self.segments[0] == other
    }
}

impl PartialEq<Symbol> for Path {
    fn eq(&self, other: &Symbol) -> bool {
        self.segments.len() == 1 && &self.segments[0] == other
    }
}

#[derive(Clone, Eq, Debug)]
pub struct PathSegment {
    pub symbol: Symbol,
    pub span: Span,
}

impl PathSegment {
    /// Convenience constructor for testing.
    pub fn new(interner: &mut Interner, sym: &str, span: Span) -> Self {
        PathSegment {
            symbol: interner.intern(sym),
            span,
        }
    }
}

impl PartialEq for PathSegment {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl PartialEq<Symbol> for PathSegment {
    fn eq(&self, other: &Symbol) -> bool {
        &self.symbol == other
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LineStmt {
    pub body: StrBody,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StrBody {
    pub segments: Vec<StrSegment>,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum StrSegment {
    Text(Text),
    Escape(Escape),
    Expr(Expr),
    FormatFunc(FormatFunc),
}

impl StrSegment {
    /// Get the span of this segment.
    pub fn span(&self) -> Span {
        match self {
            StrSegment::Text(t) => t.span,
            StrSegment::Escape(t) => t.span,
            StrSegment::Expr(t) => t.span,
            StrSegment::FormatFunc(t) => t.span,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Text {
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Escape {
    pub kind: EscapeKind,
    pub span: Span,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EscapeKind {
    Char(token::EscapeChar),
    Byte(char),

    /// Unicode escape
    ///
    /// The span here is the span of the actual hex code, since it can be variable length
    /// up to 6 digits.
    Unicode(char, Span),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FormatFunc {
    pub path: Path,
    pub expr: Expr,
    pub args: Vec<P<FormatFuncArg>>,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FormatFuncArg {
    pub key: FormatFuncArgKey,
    pub value: Lit,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum FormatFuncArgKey {
    Path(Path),
    Num(Span),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ExprKind {
    Var(Var),
    Call(P<Expr>, Vec<P<Expr>>),
    Unary(UnOp, P<Expr>),
    Binary(BinOp, P<Expr>, P<Expr>),
    Lit(Lit),
    Err,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub span: Span,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BinOpKind {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,

    And,
    Or,
    Xor,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum LitKind {
    Number,
    True,
    False,
    Null,
    Str(P<StrBody>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Command {
    pub kind: CommandKind,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum CommandKind {
    Set(P<Var>, P<Expr>),
    Call(P<Expr>),
    Expr(P<Expr>),
    If(P<Expr>),
    ElseIf,
    Else,
    EndIf,
    Return(Option<P<Expr>>),
    Custom(Vec<token::Token>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Stmt {
    pub pragmas: Vec<P<Pragma>>,
    pub body: StmtBody,
    pub decorator_command: Option<P<Command>>,
    pub hashtags: Vec<Hashtag>,
    pub associated_block: Option<P<Block>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Hashtag {
    pub span: Span,
    pub path: Path,
    pub value: Text,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Pragma {
    pub span: Span,
    pub style: PragmaStyle,
    pub meta: Vec<P<Meta>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum PragmaStyle {
    Outer,
    Inner,
}

impl From<token::PragmaStyle> for PragmaStyle {
    fn from(s: token::PragmaStyle) -> Self {
        use token::PragmaStyle as PS;

        match s {
            PS::Outer => PragmaStyle::Outer,
            PS::Inner => PragmaStyle::Inner,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Meta {
    pub path: Path,
    pub args: Option<Vec<P<Meta>>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StmtBody {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum StmtKind {
    Text(P<StrBody>),
    Command(P<Command>),
    Flow(P<Flow>),
    ShortcutOption(P<ShortcutOption>),
    Block(Block),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Flow {
    pub span: Span,
    pub is_subroutine: bool,
    pub option_text: Option<P<StrBody>>,
    pub target: Path,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ShortcutOption {
    pub span: Span,
    pub text: P<StrBody>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Block {
    pub span: Span,
    pub inner_pragmas: Vec<P<Pragma>>,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Node {
    pub span: Span,
    pub outer_pragmas: Vec<P<Pragma>>,
    pub headers: Vec<NodeHeader>,
    pub body: Block,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum NodeHeader {
    Title(Path),
    Tags(Vec<PathSegment>),
    Custom(PathSegment, Span),
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct File {
    pub inner_pragmas: Vec<P<Pragma>>,
    pub span: Span,
    pub nodes: Vec<P<Node>>,
}
