use super::*;

use crate::token::Keyword;

pub trait Visit<'ast> {
    #[allow(unused_variables)]
    fn visit_sigil(&mut self, sigil: &'ast Sigil) {}

    #[allow(unused_variables)]
    fn visit_symbol(&mut self, symbol: &'ast Symbol) {}

    #[allow(unused_variables)]
    fn visit_keyword(&mut self, keyword: &'ast Keyword) {}

    fn visit_var(&mut self, var: &'ast Var) {
        self.visit_sigil(&var.sigil);
        self.visit_symbol(&var.symbol);
        if let Some(keyword) = var.keyword.as_ref() {
            self.visit_keyword(keyword);
        }
    }

    fn visit_path(&mut self, path: &'ast Path) {
        for segment in &path.segments {
            self.visit_path_segment(segment);
        }
    }

    fn visit_path_segment(&mut self, segment: &'ast PathSegment) {
        self.visit_symbol(&segment.symbol);
        if let Some(keyword) = segment.keyword.as_ref() {
            self.visit_keyword(keyword);
        }
    }

    fn visit_line_stmt(&mut self, line_stmt: &'ast LineStmt) {
        self.visit_str_body(&line_stmt.body);
    }

    fn visit_str_body(&mut self, str_body: &'ast StrBody) {
        for segment in &str_body.segments {
            self.visit_str_segment(segment);
        }
    }

    fn visit_str_segment(&mut self, str_segment: &'ast StrSegment) {
        match str_segment {
            StrSegment::Text(text) => self.visit_text(text),
            StrSegment::Escape(escape) => self.visit_escape(escape),
            StrSegment::Expr(_, expr) => self.visit_expr(expr),
            StrSegment::FormatFunc(_, format_func) => self.visit_format_func(format_func),
        }
    }

    #[allow(unused_variables)]
    fn visit_text(&mut self, text: &'ast Text) {}

    #[allow(unused_variables)]
    fn visit_escape(&mut self, escape: &'ast Escape) {}

    fn visit_format_func(&mut self, format_func: &'ast FormatFunc) {
        self.visit_path(&format_func.path);
        self.visit_expr(&format_func.expr);
        for arg in &format_func.args {
            self.visit_format_func_arg(arg);
        }
    }

    fn visit_format_func_arg(&mut self, format_func_arg: &'ast FormatFuncArg) {
        self.visit_format_func_arg_key(&format_func_arg.key);
        self.visit_expr(&format_func_arg.value)
    }

    fn visit_format_func_arg_key(&mut self, format_func_arg_key: &'ast FormatFuncArgKey) {
        match format_func_arg_key {
            FormatFuncArgKey::Path(path) => self.visit_path(path),
            FormatFuncArgKey::Num(lit) => self.visit_lit(lit),
        }
    }

    #[allow(unused_variables)]
    fn visit_lit(&mut self, lit: &'ast Lit) {}

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match &expr.kind {
            ExprKind::Var(var) => self.visit_var(var),
            ExprKind::Call(receiver, args) => {
                self.visit_expr(receiver);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Unary(_, operand) => self.visit_expr(operand),
            ExprKind::Binary(_, left, right) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            ExprKind::Lit(lit) => self.visit_lit(lit),
            ExprKind::Err => {}
        }
    }

    fn visit_command(&mut self, command: &'ast Command) {
        match &command.kind {
            CommandKind::Set(var, value) => {
                self.visit_var(var);
                self.visit_expr(value);
            }
            CommandKind::Call(expr) => self.visit_expr(expr),
            CommandKind::If(expr) => self.visit_expr(expr),
            CommandKind::ElseIf(expr) => self.visit_expr(expr),
            CommandKind::Else => {}
            CommandKind::EndIf => {}
            CommandKind::Return(Some(expr)) => self.visit_expr(expr),
            CommandKind::Return(None) => {}
            CommandKind::Custom(str_body) => self.visit_str_body(str_body),
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        for pragma in &stmt.pragmas {
            self.visit_pragma(pragma);
        }

        self.visit_stmt_body(&stmt.body);

        if let Some(command) = stmt.decorator_command.as_ref() {
            self.visit_command(command);
        }

        for hashtag in &stmt.hashtags {
            self.visit_hashtag(hashtag);
        }
    }

    fn visit_pragma(&mut self, pragma: &'ast Pragma) {
        for meta in &pragma.meta {
            self.visit_meta(meta)
        }
    }

    fn visit_meta(&mut self, meta: &'ast Meta) {
        self.visit_path(&meta.path);

        if let Some(args) = meta.args.as_ref() {
            for arg in args {
                self.visit_meta(arg);
            }
        }
    }

    fn visit_hashtag(&mut self, hashtag: &'ast Hashtag) {
        self.visit_text(&hashtag.text);
    }

    fn visit_stmt_body(&mut self, stmt_body: &'ast StmtBody) {
        match &stmt_body.kind {
            StmtKind::Text(str_body) => self.visit_str_body(str_body),
            StmtKind::Command(command) => self.visit_command(command),
            StmtKind::Flow(flow) => self.visit_flow(flow),
            StmtKind::ShortcutOption(shortcut_option) => {
                self.visit_shortcut_option(shortcut_option)
            }
            StmtKind::Block(block) => self.visit_block(block),
            StmtKind::If(if_stmt) => self.visit_if_stmt(if_stmt),
            StmtKind::Shortcuts(shortcuts_stmt) => self.visit_shortcuts_stmt(shortcuts_stmt),
        }
    }

    fn visit_if_stmt(&mut self, if_stmt: &'ast IfStmt) {
        self.visit_if_clause(&if_stmt.if_clause);
        for if_clause in &if_stmt.else_if_clauses {
            self.visit_if_clause(if_clause);
        }
        if let Some(block) = if_stmt.else_block.as_ref() {
            self.visit_block(block)
        }
    }

    fn visit_if_clause(&mut self, if_clause: &'ast IfClause) {
        self.visit_expr(&if_clause.condition);
        self.visit_block(&if_clause.block);
    }

    fn visit_shortcuts_stmt(&mut self, shortcuts_stmt: &'ast ShortcutsStmt) {
        for shortcut_option_clause in &shortcuts_stmt.options {
            self.visit_shortcut_option_clause(shortcut_option_clause);
        }
    }

    fn visit_shortcut_option_clause(&mut self, shortcut_option_clause: &'ast ShortcutOptionClause) {
        self.visit_shortcut_option(&shortcut_option_clause.option);
        self.visit_block(&shortcut_option_clause.block);

        if let Some(command) = shortcut_option_clause.decorator_command.as_ref() {
            self.visit_command(command);
        }

        for hashtag in &shortcut_option_clause.hashtags {
            self.visit_hashtag(hashtag);
        }
    }

    fn visit_block(&mut self, block: &'ast Block) {
        for pragma in &block.pragmas {
            self.visit_pragma(pragma);
        }
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_flow(&mut self, flow: &'ast Flow) {
        if let Some(str_body) = flow.option_text.as_ref() {
            self.visit_str_body(str_body);
        }

        self.visit_flow_target(&flow.target);
    }

    fn visit_flow_target(&mut self, flow_target: &'ast FlowTarget) {
        match flow_target {
            FlowTarget::Path(path) => self.visit_path(path),
            FlowTarget::SubRoutine(flow_target_sub_routine) => {
                self.visit_flow_target_sub_routine(flow_target_sub_routine)
            }
            FlowTarget::SubRoutineSet(_, var, flow_target_sub_routine) => {
                self.visit_var(var);
                self.visit_flow_target_sub_routine(flow_target_sub_routine);
            }
        }
    }

    fn visit_flow_target_sub_routine(
        &mut self,
        flow_target_sub_routine: &'ast FlowTargetSubRoutine,
    ) {
        self.visit_path(&flow_target_sub_routine.path);
        for argument in &flow_target_sub_routine.arguments {
            self.visit_expr(argument);
        }
    }

    fn visit_shortcut_option(&mut self, shortcut_option: &'ast ShortcutOption) {
        self.visit_str_body(&shortcut_option.text);
    }

    fn visit_node(&mut self, node: &'ast Node) {
        for pragma in &node.pragmas {
            self.visit_pragma(pragma);
        }
        for node_header in &node.headers {
            self.visit_node_header(node_header);
        }
        self.visit_block(&node.body);
    }

    fn visit_node_header(&mut self, node_header: &'ast NodeHeader) {
        match node_header {
            NodeHeader::Title(path) => self.visit_path(path),
            NodeHeader::Tags(tags) => {
                for tag in tags {
                    self.visit_path(tag);
                }
            }
            NodeHeader::Custom(key, _) => self.visit_path(key),
        }
    }

    fn visit_file(&mut self, file: &'ast File) {
        for pragma in &file.pragmas {
            self.visit_pragma(pragma);
        }
        for node in &file.nodes {
            self.visit_node(node);
        }
    }
}

pub trait VisitMut {
    #[allow(unused_variables)]
    fn visit_sigil_mut(&mut self, sigil: &mut Sigil) {}

    #[allow(unused_variables)]
    fn visit_symbol_mut(&mut self, symbol: &mut Symbol) {}

    #[allow(unused_variables)]
    fn visit_keyword_mut(&mut self, keyword: &mut Keyword) {}

    fn visit_var_mut(&mut self, var: &mut Var) {
        self.visit_sigil_mut(&mut var.sigil);
        self.visit_symbol_mut(&mut var.symbol);
        if let Some(keyword) = var.keyword.as_mut() {
            self.visit_keyword_mut(keyword);
        }
    }

    fn visit_path_mut(&mut self, path: &mut Path) {
        for segment in &mut path.segments {
            self.visit_path_segment_mut(segment);
        }
    }

    fn visit_path_segment_mut(&mut self, segment: &mut PathSegment) {
        self.visit_symbol_mut(&mut segment.symbol);
        if let Some(keyword) = segment.keyword.as_mut() {
            self.visit_keyword_mut(keyword);
        }
    }

    fn visit_line_stmt_mut(&mut self, line_stmt: &mut LineStmt) {
        self.visit_str_body_mut(&mut line_stmt.body);
    }

    fn visit_str_body_mut(&mut self, str_body: &mut StrBody) {
        for segment in &mut str_body.segments {
            self.visit_str_segment_mut(segment);
        }
    }

    fn visit_str_segment_mut(&mut self, str_segment: &mut StrSegment) {
        match str_segment {
            StrSegment::Text(text) => self.visit_text_mut(text),
            StrSegment::Escape(escape) => self.visit_escape_mut(escape),
            StrSegment::Expr(_, expr) => self.visit_expr_mut(expr),
            StrSegment::FormatFunc(_, format_func) => self.visit_format_func_mut(format_func),
        }
    }

    #[allow(unused_variables)]
    fn visit_text_mut(&mut self, text: &mut Text) {}

    #[allow(unused_variables)]
    fn visit_escape_mut(&mut self, escape: &mut Escape) {}

    fn visit_format_func_mut(&mut self, format_func: &mut FormatFunc) {
        self.visit_path_mut(&mut format_func.path);
        self.visit_expr_mut(&mut format_func.expr);
        for arg in &mut format_func.args {
            self.visit_format_func_arg_mut(arg);
        }
    }

    fn visit_format_func_arg_mut(&mut self, format_func_arg: &mut FormatFuncArg) {
        self.visit_format_func_arg_key_mut(&mut format_func_arg.key);
        self.visit_expr_mut(&mut format_func_arg.value)
    }

    fn visit_format_func_arg_key_mut(&mut self, format_func_arg_key: &mut FormatFuncArgKey) {
        match format_func_arg_key {
            FormatFuncArgKey::Path(path) => self.visit_path_mut(path),
            FormatFuncArgKey::Num(lit) => self.visit_lit_mut(lit),
        }
    }

    #[allow(unused_variables)]
    fn visit_lit_mut(&mut self, lit: &mut Lit) {}

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Var(var) => self.visit_var_mut(var),
            ExprKind::Call(receiver, args) => {
                self.visit_expr_mut(receiver);
                for arg in args {
                    self.visit_expr_mut(arg);
                }
            }
            ExprKind::Unary(_, operand) => self.visit_expr_mut(operand),
            ExprKind::Binary(_, left, right) => {
                self.visit_expr_mut(left);
                self.visit_expr_mut(right);
            }
            ExprKind::Lit(lit) => self.visit_lit_mut(lit),
            ExprKind::Err => {}
        }
    }

    fn visit_command_mut(&mut self, command: &mut Command) {
        match &mut command.kind {
            CommandKind::Set(var, value) => {
                self.visit_var_mut(var);
                self.visit_expr_mut(value);
            }
            CommandKind::Call(expr) => self.visit_expr_mut(expr),
            CommandKind::If(expr) => self.visit_expr_mut(expr),
            CommandKind::ElseIf(expr) => self.visit_expr_mut(expr),
            CommandKind::Else => {}
            CommandKind::EndIf => {}
            CommandKind::Return(Some(expr)) => self.visit_expr_mut(expr),
            CommandKind::Return(None) => {}
            CommandKind::Custom(str_body) => self.visit_str_body_mut(str_body),
        }
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        for pragma in &mut stmt.pragmas {
            self.visit_pragma_mut(pragma);
        }

        self.visit_stmt_body_mut(&mut stmt.body);

        if let Some(command) = stmt.decorator_command.as_mut() {
            self.visit_command_mut(command);
        }

        for hashtag in &mut stmt.hashtags {
            self.visit_hashtag_mut(hashtag);
        }
    }

    fn visit_pragma_mut(&mut self, pragma: &mut Pragma) {
        for meta in &mut pragma.meta {
            self.visit_meta_mut(meta)
        }
    }

    fn visit_meta_mut(&mut self, meta: &mut Meta) {
        self.visit_path_mut(&mut meta.path);

        if let Some(args) = meta.args.as_mut() {
            for arg in args {
                self.visit_meta_mut(arg);
            }
        }
    }

    fn visit_hashtag_mut(&mut self, hashtag: &mut Hashtag) {
        self.visit_text_mut(&mut hashtag.text);
    }

    fn visit_stmt_body_mut(&mut self, stmt_body: &mut StmtBody) {
        match &mut stmt_body.kind {
            StmtKind::Text(str_body) => self.visit_str_body_mut(str_body),
            StmtKind::Command(command) => self.visit_command_mut(command),
            StmtKind::Flow(flow) => self.visit_flow_mut(flow),
            StmtKind::ShortcutOption(shortcut_option) => {
                self.visit_shortcut_option_mut(shortcut_option)
            }
            StmtKind::Block(block) => self.visit_block_mut(block),
            StmtKind::If(if_stmt) => self.visit_if_stmt_mut(if_stmt),
            StmtKind::Shortcuts(shortcuts_stmt) => self.visit_shortcuts_stmt_mut(shortcuts_stmt),
        }
    }

    fn visit_if_stmt_mut(&mut self, if_stmt: &mut IfStmt) {
        self.visit_if_clause_mut(&mut if_stmt.if_clause);
        for if_clause in &mut if_stmt.else_if_clauses {
            self.visit_if_clause_mut(if_clause);
        }
        if let Some(block) = if_stmt.else_block.as_mut() {
            self.visit_block_mut(block)
        }
    }

    fn visit_if_clause_mut(&mut self, if_clause: &mut IfClause) {
        self.visit_expr_mut(&mut if_clause.condition);
        self.visit_block_mut(&mut if_clause.block);
    }

    fn visit_shortcuts_stmt_mut(&mut self, shortcuts_stmt: &mut ShortcutsStmt) {
        for shortcut_option_clause in &mut shortcuts_stmt.options {
            self.visit_shortcut_option_clause_mut(shortcut_option_clause);
        }
    }

    fn visit_shortcut_option_clause_mut(
        &mut self,
        shortcut_option_clause: &mut ShortcutOptionClause,
    ) {
        self.visit_shortcut_option_mut(&mut shortcut_option_clause.option);
        self.visit_block_mut(&mut shortcut_option_clause.block);

        if let Some(command) = shortcut_option_clause.decorator_command.as_mut() {
            self.visit_command_mut(command);
        }

        for hashtag in &mut shortcut_option_clause.hashtags {
            self.visit_hashtag_mut(hashtag);
        }
    }

    fn visit_block_mut(&mut self, block: &mut Block) {
        for pragma in &mut block.pragmas {
            self.visit_pragma_mut(pragma);
        }
        for stmt in &mut block.stmts {
            self.visit_stmt_mut(stmt);
        }
    }

    fn visit_flow_mut(&mut self, flow: &mut Flow) {
        if let Some(str_body) = flow.option_text.as_mut() {
            self.visit_str_body_mut(str_body);
        }

        self.visit_flow_target_mut(&mut flow.target);
    }

    fn visit_flow_target_mut(&mut self, flow_target: &mut FlowTarget) {
        match flow_target {
            FlowTarget::Path(path) => self.visit_path_mut(path),
            FlowTarget::SubRoutine(flow_target_sub_routine) => {
                self.visit_flow_target_sub_routine_mut(flow_target_sub_routine)
            }
            FlowTarget::SubRoutineSet(_, var, flow_target_sub_routine) => {
                self.visit_var_mut(var);
                self.visit_flow_target_sub_routine_mut(flow_target_sub_routine);
            }
        }
    }

    fn visit_flow_target_sub_routine_mut(
        &mut self,
        flow_target_sub_routine: &mut FlowTargetSubRoutine,
    ) {
        self.visit_path_mut(&mut flow_target_sub_routine.path);
        for argument in &mut flow_target_sub_routine.arguments {
            self.visit_expr_mut(argument);
        }
    }

    fn visit_shortcut_option_mut(&mut self, shortcut_option: &mut ShortcutOption) {
        self.visit_str_body_mut(&mut shortcut_option.text);
    }

    fn visit_node_mut(&mut self, node: &mut Node) {
        for pragma in &mut node.pragmas {
            self.visit_pragma_mut(pragma);
        }
        for node_header in &mut node.headers {
            self.visit_node_header_mut(node_header);
        }
        self.visit_block_mut(&mut node.body);
    }

    fn visit_node_header_mut(&mut self, node_header: &mut NodeHeader) {
        match node_header {
            NodeHeader::Title(path) => self.visit_path_mut(path),
            NodeHeader::Tags(tags) => {
                for tag in tags {
                    self.visit_path_mut(tag);
                }
            }
            NodeHeader::Custom(key, _) => self.visit_path_mut(key),
        }
    }

    fn visit_file_mut(&mut self, file: &mut File) {
        for pragma in &mut file.pragmas {
            self.visit_pragma_mut(pragma);
        }
        for node in &mut file.nodes {
            self.visit_node_mut(node);
        }
    }
}
