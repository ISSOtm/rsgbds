use std::{
    cell::RefCell,
    num::NonZeroUsize,
    ops::{Deref, DerefMut, Index, IndexMut, Range},
    rc::Rc,
};

use compact_str::CompactString;
use string_interner::symbol::SymbolUsize;

use crate::{
    macro_args::MacroArgs,
    source_store::{SourceHandle, SourceSlice, SourceStore},
    syntax::lexer::LexerState,
};

// TODO: can we use something from crate `qcell` instead of `RefCell`?
#[derive(Debug)]
pub struct ContextStack(RefCell<StackInner>);

#[derive(Debug)]
struct StackInner {
    /// An important invariant is maintained here: sources with a non-zero `ref_count` have their index preserved.
    nodes: Vec<SourceNode>,
    /// The topmost context is the active one.
    contexts: Vec<SourceContext>,
}

#[derive(Debug)]
pub struct SourceNode {
    /// If this is zero, then the slot is actually empty.
    ref_count: usize,
    /// `None` for **a** root context.
    ///
    /// Implicitly holds a reference to the corresponding [`SourceNode`].
    /// Not using [`SourceRef`] to avoid making the [`ContextStack`] self-referential.
    parent_id: Option<NonZeroUsize>,
    kind: SourceKind,
}

#[derive(Debug)]
pub enum SourceKind {
    File(SourceHandle),
    Macro {
        name: CompactString,
        file: SourceHandle,
        slice: Range<usize>,
    },
    Rept {
        iteration: u32,
        file: SourceHandle,
        slice: Range<usize>,
    },
}

/// What's the difference between a [`SourceNode`] and a [`SourceContext`]?
/// The former persists even after its scope is exited, but the latter doesn't.
#[derive(Debug)]
pub struct SourceContext {
    /// Implicitly holds a reference to the corresponding [`SourceNode`].
    /// Not using [`SourceRef`] to avoid making the [`ContextStack`] self-referential.
    source_id: NonZeroUsize,
    lexer_state: LexerState,
    /// - If this is `None`, then `\@` is not allowed for this scope.
    /// - If this is `Some("")`, then the `\@` string hasn't been generated for this scope yet.
    /// - Otherwise, it just contains the string.
    unique_id_str: Option<CompactString>,
    macro_args: Option<Rc<MacroArgs>>,
    // TODO
    //loop_state: LoopState,
}

#[derive(Debug)]
pub enum LoopState {
    Rept(u32),
    For {
        value: u32,
        step: u32,
        sym_name: SymbolUsize,
    },
}

#[derive(Debug)]
pub struct Span<'ctx_stack> {
    pub src: Option<SourceRef<'ctx_stack>>,
    pub byte_span: Range<usize>,
}

impl ContextStack {
    pub fn new() -> Self {
        Self(RefCell::new(StackInner {
            nodes: Vec::new(),
            contexts: Vec::new(),
        }))
    }

    pub fn sources(&self) -> Sources<'_> {
        Sources(self.0.borrow())
    }

    pub fn sources_mut(&self) -> SourcesMut<'_> {
        SourcesMut(self.0.borrow_mut())
    }
}

impl SourceNode {
    pub fn source<'srcs>(&self, sources: &'srcs SourceStore) -> &'srcs str {
        match self.kind {
            SourceKind::File(handle) => sources[handle].as_ref(),
            _ => todo!(),
        }
    }
}

impl SourceContext {
    pub fn lexer_state(&self) -> &LexerState {
        &self.lexer_state
    }

    pub fn lexer_state_mut(&mut self) -> &mut LexerState {
        &mut self.lexer_state
    }

    pub fn source_id(&self) -> NonZeroUsize {
        self.source_id
    }

    pub fn macro_args(&self) -> Option<&MacroArgs> {
        self.macro_args.as_deref()
    }

    pub fn node<'nodes>(&self, nodes: &'nodes [SourceNode]) -> &'nodes SourceNode {
        &nodes[self.source_id.get() - 1]
    }
}

impl Span<'_> {
    pub const BUILTIN: Self = Self {
        src: None,
        byte_span: 0..0,
    };
    pub const COMMAND_LINE: Self = Self {
        src: None,
        byte_span: 1..1,
    };

    pub fn resolve(&self) -> (SourceHandle, Range<usize>) {
        if let Some(src) = &self.src {
            src.get().resolve(&self.byte_span)
        } else if self.byte_span == Span::BUILTIN.byte_span {
            (SourceHandle::BUILTIN, 0..0)
        } else {
            debug_assert_eq!(self.byte_span, Span::COMMAND_LINE.byte_span);
            (SourceHandle::COMMAND_LINE, 0..0)
        }
    }
}

impl SourceNode {
    pub fn resolve(&self, byte_span: &Range<usize>) -> (SourceHandle, Range<usize>) {
        fn offset_by(byte_span: &Range<usize>, buffer_slice: &Range<usize>) -> Range<usize> {
            debug_assert!(byte_span.end <= buffer_slice.len());
            byte_span.start + buffer_slice.start..byte_span.end + buffer_slice.start
        }

        match &self.kind {
            SourceKind::File(handle) => (*handle, byte_span.clone()),
            SourceKind::Macro { file, slice, .. } => (*file, offset_by(byte_span, slice)),
            SourceKind::Rept { file, slice, .. } => (*file, offset_by(byte_span, slice)),
        }
    }
}

// Nobody gets to cheat with a `SourceRef`. Everybody has to abide by the ref-counting.
mod source_ref {
    use super::*;

    #[derive(Debug)] // Cloning this would require incrementing the referenced `SourceNode`'s ref count!
    pub struct SourceRef<'stack>(&'stack ContextStack, NonZeroUsize);

    impl<'stack> SourceRef<'stack> {
        pub fn new(stack: &'stack ContextStack, idx: NonZeroUsize) -> Self {
            stack.sources_mut()[idx].ref_count += 1;
            Self(stack, idx)
        }

        pub fn new_via(
            stack: &'stack ContextStack,
            idx: NonZeroUsize,
            sources: &mut SourcesMut<'_>,
        ) -> Self {
            sources[idx].ref_count += 1;
            Self(stack, idx)
        }

        pub fn get(&self) -> SourceGuard {
            SourceGuard(self.0.sources(), self.1)
        }
    }

    impl Drop for SourceRef<'_> {
        fn drop(&mut self) {
            self.0.sources_mut()[self.1].ref_count -= 1;
        }
    }

    #[derive(Debug)]
    pub struct SourceGuard<'stack>(Sources<'stack>, NonZeroUsize);

    impl Deref for SourceGuard<'_> {
        type Target = SourceNode;

        fn deref(&self) -> &Self::Target {
            &self.0[self.1]
        }
    }
}
pub use source_ref::*;

#[derive(Debug)]
pub struct Sources<'stack>(std::cell::Ref<'stack, StackInner>);

impl Index<NonZeroUsize> for Sources<'_> {
    type Output = SourceNode;

    fn index(&self, index: NonZeroUsize) -> &Self::Output {
        &self.0.nodes[index.get() - 1]
    }
}

impl Sources<'_> {
    pub fn active_context(&self) -> Option<&SourceContext> {
        self.0.contexts.last()
    }
}

#[derive(Debug)]
pub struct SourcesMut<'stack>(std::cell::RefMut<'stack, StackInner>);

impl Index<NonZeroUsize> for SourcesMut<'_> {
    type Output = SourceNode;

    fn index(&self, index: NonZeroUsize) -> &Self::Output {
        &self.0.nodes[index.get() - 1]
    }
}

impl IndexMut<NonZeroUsize> for SourcesMut<'_> {
    fn index_mut(&mut self, index: NonZeroUsize) -> &mut Self::Output {
        &mut self.0.nodes[index.get() - 1]
    }
}

impl SourcesMut<'_> {
    pub fn active_context(&self) -> Option<&SourceContext> {
        self.0.contexts.last()
    }

    pub fn active_context_mut(&mut self) -> Option<(&mut SourceContext, &[SourceNode])> {
        let StackInner { nodes, contexts } = self.0.deref_mut();
        contexts
            .last_mut()
            .map(|contexts| (contexts, nodes.as_slice()))
    }

    fn push_context(
        &mut self,
        node_kind: SourceKind,
        has_new_unique_id: bool,
        macro_args: Option<Rc<MacroArgs>>,
    ) {
        let context = self.active_context();
        let unique_id_str = if has_new_unique_id {
            Some(CompactString::default())
        } else {
            context.and_then(|ctx| ctx.unique_id_str.clone())
        };
        let macro_args = macro_args.or_else(|| {
            if let Some(SourceContext {
                macro_args: Some(args),
                ..
            }) = context
            {
                Some(Rc::clone(args))
            } else {
                None
            }
        });

        let parent_id = context.map(|ctx| ctx.source_id); // TODO: increment its ref count
        self.0.nodes.push(SourceNode {
            ref_count: 1, // Implicitly referenced due to being the active node.
            parent_id,
            kind: node_kind,
        });
        let source_id = NonZeroUsize::new(self.0.nodes.len()).unwrap();

        self.0.contexts.push(SourceContext {
            source_id,
            lexer_state: LexerState::new(),
            unique_id_str,
            macro_args,
        });
    }

    pub fn push_file_context(&mut self, source: SourceHandle) {
        self.push_context(SourceKind::File(source), false, None);
    }

    pub fn push_macro_context(
        &mut self,
        name: CompactString,
        slice: &SourceSlice,
        args: Rc<MacroArgs>,
    ) {
        let (file, slice) = slice.as_raw();
        self.push_context(
            SourceKind::Macro {
                name,
                file: *file,
                slice: slice.clone(),
            },
            true,
            Some(args),
        );
    }

    pub fn end_current_context(&mut self) {
        let (context, _nodes) = self
            .active_context_mut()
            .expect("No active context to end?!");

        // if let Some(loop_state) = context.loop_state {
        //     todo!();
        // }

        self.0.contexts.pop();
    }
}
