use crate::ptr::P;
use crate::symbol::{Interner, Symbol};
use crate::token;
use crate::Span;

pub mod visit;

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

impl Path {
    /// Returns `true` if there is only one segment.
    pub fn is_segment(&self) -> bool {
        self.segments.len() == 1
    }

    /// Returns the only segment if there is only one.
    pub fn as_segment(&self) -> Option<&PathSegment> {
        if self.is_segment() {
            Some(&self.segments[0])
        } else {
            None
        }
    }
}

#[derive(Clone, Eq, Debug)]
pub struct PathSegment {
    pub symbol: Symbol,
    /// `Some` if the symbol is a keyword.
    pub keyword: Option<token::Keyword>,
    pub span: Span,
}

impl PathSegment {
    /// Convenience constructor for testing.
    pub fn new(interner: &mut Interner, sym: &str, span: Span) -> Self {
        PathSegment {
            symbol: interner.intern(sym),
            keyword: crate::token::Keyword::identify(sym),
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
    /// `Expr` interpolation (`{}`). The first element is the full span including delimiters.
    Expr(Span, P<Expr>),
    /// `FormatFunc` interpolation (`[]`). The first element is the full span including delimiters.
    FormatFunc(Span, FormatFunc),
}

impl StrSegment {
    /// Get the span of this segment.
    pub fn span(&self) -> Span {
        match self {
            StrSegment::Text(t) => t.span,
            StrSegment::Escape(t) => t.span,
            StrSegment::Expr(span, _) => *span,
            StrSegment::FormatFunc(span, _) => *span,
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
    pub expr: P<Expr>,
    pub args: Vec<FormatFuncArg>,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FormatFuncArg {
    pub key: FormatFuncArgKey,
    pub value: P<Expr>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum FormatFuncArgKey {
    Path(Path),
    Num(Lit),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl From<Var> for Expr {
    fn from(var: Var) -> Self {
        Expr {
            span: var.span,
            kind: ExprKind::Var(var),
        }
    }
}

impl From<Lit> for Expr {
    fn from(lit: Lit) -> Self {
        Expr {
            span: lit.span,
            kind: ExprKind::Lit(lit),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ExprKind {
    Var(Var),
    Call(P<Expr>, Vec<Expr>),
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
    /// A literal integer. The field is the parsed value or `None` if unparsable.
    Int(Option<i64>),
    /// A literal floating point number. The field is the parsed value or `None` if unparsable.
    Float(Option<LitFloat>),
    True,
    False,
    Null,
    Str(StrBody),
}

#[derive(Copy, Clone, Debug)]
pub struct LitFloat {
    pub value: f64,
}

impl Eq for LitFloat {}
impl PartialEq for LitFloat {
    fn eq(&self, other: &Self) -> bool {
        approx::relative_eq!(self.value, other.value)
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Command {
    pub kind: CommandKind,
    pub span: Span,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum CommandKind {
    Set(Var, P<Expr>),
    Call(P<Expr>),

    /// The raw `if` command. This is different from the `If` statement, which is generated
    /// during block regrouping from raw commands. See the latter for the rationale of doing
    /// this.
    If(P<Expr>),
    /// The raw `elseif` command. See also `If`.
    ElseIf(P<Expr>),
    /// The raw `else` command. See also `If`.
    Else,
    /// The raw `endif` command. See also `If`.
    EndIf,

    Return(Option<P<Expr>>),
    Custom(StrBody),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Stmt {
    pub span: Span,
    pub pragmas: Vec<Pragma>,
    pub body: StmtBody,
    pub decorator_command: Option<Command>,
    pub hashtags: Vec<Hashtag>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Hashtag {
    pub span: Span,
    pub text: Text,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Pragma {
    pub span: Span,
    pub style: PragmaStyle,
    pub meta: Vec<Meta>,
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
    pub args: Option<Vec<Meta>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StmtBody {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum StmtKind {
    Text(StrBody),
    Command(Command),
    Flow(Flow),
    ShortcutOption(ShortcutOption),
    Block(Block),

    /// The `if` statement. This is not parsed natively, but generated in the block regrouping
    /// transform.
    If(IfStmt),

    /// The "shortcut option group" statement. This is not parsed natively, but generated in
    /// the block regrouping transform.
    Shortcuts(ShortcutsStmt),
}

impl StmtKind {
    /// Returns if the statement kind can be decorated.
    pub(crate) fn may_have_decorators(&self) -> bool {
        match self {
            Self::Text(_) | Self::Command(_) | Self::Flow(_) | Self::ShortcutOption(_) => true,
            Self::Block(_) | Self::If(_) | Self::Shortcuts(_) => false,
        }
    }
}

/// The `if` statement. This is not parsed natively, but generated in the block regrouping
/// step.
///
/// This is different from the raw `if`, `elseif` and `else` commands. Making regrouping a
/// separate step makes it easier to parse weirdly formatted code, and recover from invalid
/// ones when there are multiple nodes.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IfStmt {
    pub span: Span,
    pub if_clause: IfClause,
    pub else_if_clauses: Vec<IfClause>,
    pub else_block: Option<Block>,

    /// Contains invalid clauses after else, before `endif`. Nested if-statements within are not
    /// regrouped, and remain raw commands.
    pub invalid: Option<Block>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IfClause {
    pub span: Span,
    pub condition: P<Expr>,
    pub block: Block,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ShortcutsStmt {
    pub span: Span,
    pub options: Vec<ShortcutOptionClause>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ShortcutOptionClause {
    pub span: Span,
    pub option: ShortcutOption,
    pub decorator_command: Option<Command>,
    pub hashtags: Vec<Hashtag>,
    pub block: Block,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Flow {
    pub span: Span,
    pub option_text: Option<StrBody>,
    pub target: FlowTarget,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum FlowTarget {
    Path(Path),
    SubRoutine(FlowTargetSubRoutine),
    /// A `set $foo = subroutine()` target. The first element is the full span of the `set`
    /// "command".
    SubRoutineSet(Span, Var, FlowTargetSubRoutine),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FlowTargetSubRoutine {
    pub span: Span,
    pub path: Path,
    pub arguments: Vec<Expr>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ShortcutOption {
    pub span: Span,
    pub text: StrBody,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Block {
    pub span: Span,
    pub pragmas: Vec<Pragma>,
    pub stmts: Vec<Stmt>,
}

impl Block {
    /// Creates an empty block with a span.
    pub fn empty(span: Span) -> Self {
        Block {
            span,
            pragmas: Vec::new(),
            stmts: Vec::new(),
        }
    }

    /// Pushes a statement to this block, extending the span.
    pub fn push(&mut self, stmt: Stmt) {
        self.span = self.span.union(stmt.span);
        self.stmts.push(stmt);
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Node {
    pub span: Span,
    pub pragmas: Vec<Pragma>,
    pub headers: Vec<NodeHeader>,
    pub body: Block,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum NodeHeader {
    Title(Path),
    Tags(Vec<Path>),
    /// A custom header entry. The second argument is a span of the header content.
    Custom(Path, Span),
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct File {
    pub pragmas: Vec<Pragma>,
    pub span: Span,
    pub nodes: Vec<Node>,
}
