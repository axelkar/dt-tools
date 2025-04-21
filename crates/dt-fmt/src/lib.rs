use std::sync::Arc;

use dt_parser::cst::{GreenItem, GreenNode, GreenToken, NodeKind};

// TODO: better perf by just printing the new dts directly without mutability?
// TODO: multithreading?

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FormatterContext {
    block_indent: usize,
    // config should go here too
}

pub fn format(root: &mut GreenNode) {
    let ctx = FormatterContext { block_indent: 0 };
    for child in &mut root.children {
        match child {
            GreenItem::Node(node) => format_node(ctx, node),
            GreenItem::Token(token) => format_token(ctx, token),
        }
    }
}

fn format_token(_ctx: FormatterContext, _token: &mut Arc<GreenToken>) {}

fn format_node(mut ctx: FormatterContext, node: &mut Arc<GreenNode>) {
    if node.kind == NodeKind::DtNode {
        ctx.block_indent += 1;
    }
    // TODO: only make_mut when needed?
    for child in &mut Arc::make_mut(node).children {
        match child {
            GreenItem::Node(node) => format_node(ctx, node),
            GreenItem::Token(token) => format_token(ctx, token),
        }
    }
}

#[cfg(test)]
mod tests {
    #[expect(dead_code, reason = "not yet implemented")]
    #[track_caller]
    fn check(src: &'static str, expected: &'static str) {
        let parse = dt_parser::parser::parse(src);
        if !parse.lex_errors.is_empty() || !parse.errors.is_empty() {
            eprintln!("Invalid DTS!");
            std::process::exit(1);
        };

        // TODO: add tests

        assert_eq!(parse.green_node.print_tree(), expected);
    }

    #[test]
    fn fmt_simple() {}
}
