use crate::ptr::P;
use crate::span::Span;
use crate::symbol::Symbol;
use crate::token;

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub struct Var {
    pub sigil: Sigil,
    pub symbol: Symbol,
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

#[derive(Clone, Debug)]
pub struct LineStmt {
    pub body: StrBody,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StrBody {
    pub segments: Vec<StrSegment>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StrSegment {
    Text(Text),
    Escape(Escape),
    Expr(Expr),
    FormatFunc(FormatFunc),
}

#[derive(Clone, Debug)]
pub struct Text {
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Escape {
    pub escaped_string: Option<String>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FormatFunc {
    pub path: Path,
    pub expr: Expr,
    pub args: Vec<P<FormatFuncArg>>,
}

#[derive(Clone, Debug)]
pub struct FormatFuncArg {
    pub key: FormatFuncArgKey,
    pub value: StrBody,
}

#[derive(Clone, Debug)]
pub enum FormatFuncArgKey {
    Path(Path),
    Num(Span),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Var(Var),
    Call(PathSegment, Vec<P<Expr>>),
    Unary(UnOp, P<Expr>),
    Binary(BinOp, P<Expr>, P<Expr>),
    Lit(Lit),
    Err,
}

#[derive(Clone, Debug)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
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

#[derive(Clone, Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum LitKind {
    Number,
    True,
    False,
    Null,
    Str(P<StrBody>),
}

#[derive(Clone, Debug)]
pub struct Command {
    pub kind: CommandKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Stmt {
    pub span: Span,
    pub pragmas: Vec<P<Pragma>>,
    pub body: StmtBody,
    pub decorator_command: Option<P<Command>>,
    pub hashtags: Vec<P<Hashtag>>,
    pub associated_block: Option<P<Block>>,
}

#[derive(Clone, Debug)]
pub struct Hashtag {
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Pragma {
    pub span: Span,
    pub path: Path,
    pub style: PragmaStyle,
    pub args: PragmaArgs,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum PragmaStyle {
    Outer,
    Inner,
}

#[derive(Clone, Debug)]
pub enum PragmaArgs {
    Empty,
    Tokens(Vec<token::Token>),
}

#[derive(Clone, Debug)]
pub struct StmtBody {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Text(P<StrBody>),
    Command(P<Command>),
    Flow(P<Flow>),
    ShortcutOption(P<ShortcutOption>),
}

#[derive(Clone, Debug)]
pub struct Flow {
    pub span: Span,
    pub is_subroutine: bool,
    pub option_text: Option<P<StrBody>>,
    pub target: Path,
}

#[derive(Clone, Debug)]
pub struct ShortcutOption {
    pub span: Span,
    pub text: P<StrBody>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub span: Span,
    pub pragmas: Vec<P<Pragma>>,
    pub headers: Vec<NodeHeader>,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub enum NodeHeader {
    Title(Path),
    Tags(Vec<PathSegment>),
    Custom(PathSegment, Span),
}

#[derive(Clone, Debug)]
pub struct File {
    pub pragmas: Vec<P<Pragma>>,
    pub span: Span,
    pub nodes: Vec<P<Node>>,
}
