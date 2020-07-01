use hashbrown::hash_map::Entry;
use hashbrown::HashMap;

use crate::ast::visit::VisitMut;
use crate::ast::*;
use crate::parse::ParseCtx;
use crate::Span;

/// Validation transform that checks duplicate headers.
pub struct DuplicateHeaders<'a> {
    ctx: &'a ParseCtx<'a>,
}

impl<'a> DuplicateHeaders<'a> {
    pub fn new(ctx: &'a ParseCtx<'a>) -> Self {
        DuplicateHeaders { ctx }
    }
}

impl<'a> VisitMut for DuplicateHeaders<'a> {
    fn visit_node_mut(&mut self, node: &mut Node) {
        let mut seen = HashMap::new();
        let mut title_seen: Option<Span> = None;
        let mut tags_seen = None;

        node.headers.retain(|header| match header {
            NodeHeader::Title(title) => {
                if let Some(old_span) = title_seen {
                    self.ctx
                        .errors
                        .error("title already defined for this node")
                        .span(title.span)
                        .annotate_span(old_span, "title defined here");
                    false
                } else {
                    title_seen = Some(title.span);
                    true
                }
            }
            NodeHeader::Tags(_, span) => {
                let span = *span;
                if let Some(old_span) = tags_seen {
                    self.ctx
                        .errors
                        .error("tags already defined for this node")
                        .span(span)
                        .annotate_span(old_span, "tags defined here");
                    false
                } else {
                    tags_seen = Some(span);
                    true
                }
            }
            NodeHeader::Custom(path, span) => match seen.entry(path.clone()) {
                Entry::Occupied(entry) => {
                    self.ctx
                        .errors
                        .error(format!(
                            "header `{}` already defined for this node",
                            path.to_string(self.ctx.interner)
                        ))
                        .span(*span)
                        .annotate_span(*entry.get(), "header defined here");

                    false
                }
                Entry::Vacant(entry) => {
                    entry.insert(*span);
                    true
                }
            },
        })
    }
}
