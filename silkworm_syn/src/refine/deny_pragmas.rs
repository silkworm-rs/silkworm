use silkworm_err::ErrorCtx;

use crate::ast::visit::VisitMut;
use crate::ast::*;

/// Validation transform that removes all pragmas. This should only be ran if pragmas are
/// disabled as a whole.
pub struct DenyPragmas<'a> {
    errors: &'a ErrorCtx,
}

impl<'a> DenyPragmas<'a> {
    pub fn new(errors: &'a ErrorCtx) -> Self {
        DenyPragmas { errors }
    }
}

impl<'a> VisitMut for DenyPragmas<'a> {
    fn visit_block_mut(&mut self, block: &mut Block) {
        for pragma in block.pragmas.drain(..) {
            self.errors
                .error("pragmas are disabled in configuration")
                .span(pragma.span);
        }

        for stmt in &mut block.stmts {
            self.visit_stmt_mut(stmt);
        }
    }

    fn visit_node_mut(&mut self, node: &mut Node) {
        for pragma in node.pragmas.drain(..) {
            self.errors
                .error("pragmas are disabled in configuration")
                .span(pragma.span);
        }

        for node_header in &mut node.headers {
            self.visit_node_header_mut(node_header);
        }
        self.visit_block_mut(&mut node.body);
    }

    fn visit_file_mut(&mut self, file: &mut File) {
        for pragma in file.pragmas.drain(..) {
            self.errors
                .error("pragmas are disabled in configuration")
                .span(pragma.span);
        }

        for node in &mut file.nodes {
            self.visit_node_mut(node);
        }
    }
}
