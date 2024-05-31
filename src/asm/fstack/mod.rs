/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    cell::{Cell, RefCell},
    num::{NonZeroU32, NonZeroUsize},
    ops::Range,
    rc::Rc,
};

use codespan_reporting::files::Files;
use rgbds::object::FileStackNodesProvider;

use crate::{
    input::Storage,
    language::{Lexer, Location},
    macro_args::MacroArgs,
};

/// State in the fstack nodes is what needs to persist even after exiting the scope.
#[derive(Debug)]
pub struct Fstack(RefCell<FstackImpl>);

#[derive(Debug)]
struct FstackImpl {
    nodes: Vec<Node>,
    cur_node_id: Option<NonZeroUsize>,
}

#[derive(Debug)]
pub struct NodeHandle<'fstack> {
    fstack: &'fstack Fstack,
    node_id: usize,
}

/// # Panic
///
/// Comparisons between `NodeHandle`s originating from different `Fstack`s will panic.
impl PartialEq for NodeHandle<'_> {
    fn eq(&self, other: &Self) -> bool {
        debug_assert!(std::ptr::eq(self.fstack, other.fstack));
        self.node_id == other.node_id
    }
}

#[derive(Debug)]
pub struct Node {
    /// A count of 0 indicates either that the slot is free, or that the node is the "current node",
    /// which does not necessarily have a non-zero count.
    ref_count: Cell<usize>,
    kind: NodeKind,
    parent: Option<NonZeroUsize>,
}

#[derive(Debug)]
enum NodeKind {
    File(Storage),
    Macro((), Rc<String>),
    Loop(u32),
}

fn idx(node_id: NonZeroUsize) -> usize {
    node_id.get() - 1
}

/// Convenience shorthand.
pub type DiagInfo = Option<(usize, Range<usize>)>;

impl Fstack {
    pub fn new(root_file: Storage) -> Self {
        let this = Self(RefCell::new(FstackImpl {
            nodes: vec![],
            cur_node_id: None,
        }));
        this.push_new_node(NodeKind::File(root_file));
        this
    }

    pub fn make_diag_info(begin: &Location<'_>, end: Option<&Location<'_>>) -> DiagInfo {
        // A lack of handle means a "default" location, which should be mapped to the root node,
        // which is necessarily a file, and that always has some storage.
        if !begin.handle().map_or(true, |handle| {
            handle.with_node(|node| node.storage().is_some())
        }) {
            return None;
        }

        /// The parser generates "default" locations, which stand for "beginning of the stream".
        /// They have offset 0, we just need to map them to the root node.
        fn raw_id(location: &Location) -> usize {
            location.handle().map_or(0, |handle| handle.node_id)
        }
        let file_id = raw_id(begin);
        Some(match end {
            Some(end) => {
                debug_assert_eq!(file_id, raw_id(end));
                (file_id, begin.offset()..end.offset())
            }
            None => (file_id, begin.offset()..(begin.offset() + 1)),
        })
    }

    pub fn get_files(&self) -> Binder<'_> {
        Binder(self.0.borrow())
    }

    pub fn cur_node_handle(&self) -> Option<NodeHandle<'_>> {
        let inner = self.0.borrow();
        let node_id = idx(inner.cur_node_id?);
        inner.nodes[node_id].inc_ref_count();
        Some(NodeHandle {
            fstack: self,
            node_id,
        })
    }

    fn push_new_node(&self, kind: NodeKind) {
        let mut inner = self.0.borrow_mut();

        // First, increment the current node's ref count, to ensure that it won't be allocated over.
        // (A reference is added due to the upcoming node having the current one as its parent.)
        if let Some(id) = inner.cur_node_id {
            inner.nodes[idx(id)].inc_ref_count();
        }

        // Then, create the new node.
        let node = Node {
            ref_count: Cell::new(0),
            kind,
            parent: inner.cur_node_id,
        };
        // Find an empty slot (or create one), and write the node in.
        inner.cur_node_id = match inner
            .nodes
            .iter_mut()
            .enumerate()
            .find(|(_, node)| node.ref_count.get() == 0)
        {
            Some((i, slot)) => {
                *slot = node;
                Some(NonZeroUsize::new(i + 1).unwrap())
            }
            None => {
                inner.nodes.push(node);
                NonZeroUsize::new(inner.nodes.len())
            }
        };
    }

    /// Counterpart of [`push_new_node`][Self::push_new_node()].
    ///
    /// # Panics
    ///
    /// This function panics if no more nodes remain.
    pub fn handle_end_of_node(&self, lexer: &mut Lexer, macro_args_stack: &mut Vec<MacroArgs>) {
        let mut inner = self.0.borrow_mut();
        // Don't modify that node, as it might still be referenced!
        let cur_node_id = inner.cur_node_id.unwrap();
        let cur_node = &mut inner.nodes[idx(cur_node_id)];

        match cur_node.kind {
            NodeKind::File(_) => {}
            NodeKind::Macro(_, _) => {
                macro_args_stack
                    .pop()
                    .expect("Exiting macro with arg stack empty?!?");
            }
            // TODO: handle looping for loop nodes (and reset the lexer state as well!)
            NodeKind::Loop(_) => {}
        }

        // Switch back to its parent node.
        inner.cur_node_id = cur_node.parent;

        // Decrement the (former) parent's count.
        if let Some(id) = inner.cur_node_id {
            inner.nodes[idx(id)].dec_ref_count();
        }

        lexer.pop_state();
    }

    pub fn push_file(&self, storage: Storage, lexer: &mut Lexer) {
        self.push_new_node(NodeKind::File(storage));
        lexer.push_new_state();
    }

    pub fn push_macro(
        &self,
        body: Rc<String>,
        lexer: &mut Lexer,
        args: MacroArgs,
        macro_args_stack: &mut Vec<MacroArgs>,
    ) {
        macro_args_stack.push(args);

        self.push_new_node(NodeKind::Macro((), body));
        lexer.push_new_state();
    }

    pub fn finalize(&self) -> impl FileStackNodesProvider + '_ {
        FinalizedFstack(self.0.borrow())
    }
}

struct FinalizedFstack<'fstack>(std::cell::Ref<'fstack, FstackImpl>);
impl FileStackNodesProvider for FinalizedFstack<'_> {
    type Node = Node;

    type Iter<'slice> = std::iter::Filter<std::slice::Iter<'slice, Self::Node>, for<'a> fn(&'a &'slice Self::Node) -> bool>
    where
        Self: 'slice;

    fn nodes(&self) -> Self::Iter<'_> {
        fn is_referenced(node: &&Node) -> bool {
            node.ref_count.get() != 0
        }
        self.0.nodes.iter().filter(is_referenced)
    }

    fn parent_info(node: &Self::Node) -> Option<(NonZeroU32, u32)> {
        // TODO: the ID is wrong! What if there *are* unref'd nodes?
        node.parent.map(|id| (id.try_into().unwrap(), 0)) // TODO: parent line no
    }

    fn node_kind(node: &Self::Node) -> rgbds::object::NodeKind {
        match &node.kind {
            NodeKind::File(storage) => rgbds::object::NodeKind::File(storage.name().into()),
            NodeKind::Macro(_, _) => todo!(),
            NodeKind::Loop(_) => todo!(),
        }
    }
}

impl AsRef<str> for Node {
    fn as_ref(&self) -> &str {
        match &self.kind {
            NodeKind::File(storage) => storage.as_ref(),
            NodeKind::Macro(_, body) => body.as_ref(),
            NodeKind::Loop(_) => todo!(),
        }
    }
}

impl Node {
    fn inc_ref_count(&self) {
        let count = self.ref_count.get();
        self.ref_count.set(count + 1);
    }

    fn dec_ref_count(&self) {
        let count = self.ref_count.get();
        self.ref_count.set(count - 1);
    }

    pub fn slice(&self, range: Range<usize>) -> String {
        match &self.kind {
            NodeKind::File(storage) => storage.as_ref()[range].to_owned(),
            NodeKind::Macro(_, body) => body[range].to_owned(),
            NodeKind::Loop(_) => todo!(),
        }
    }

    pub fn storage_base_ofs(&self) -> usize {
        match &self.kind {
            NodeKind::File(_) => 0,
            NodeKind::Macro(_, body) => 0,
            NodeKind::Loop(_) => todo!(),
        }
    }
}

impl Clone for NodeHandle<'_> {
    fn clone(&self) -> Self {
        self.fstack.0.borrow().nodes[self.node_id].inc_ref_count();
        Self {
            fstack: self.fstack,
            node_id: self.node_id,
        }
    }
}

impl Drop for NodeHandle<'_> {
    fn drop(&mut self) {
        self.fstack.0.borrow().nodes[self.node_id].dec_ref_count();
    }
}

impl NodeHandle<'_> {
    pub fn with_node<T, F: FnOnce(&Node) -> T>(&self, f: F) -> T {
        let inner = self.fstack.0.borrow();
        f(&inner.nodes[self.node_id])
    }
}

/// Support function for the [`Binder`]'s [`Files`] impl.
impl Node {
    fn storage(&self) -> Option<&Storage> {
        match &self.kind {
            NodeKind::File(storage) => Some(storage),
            NodeKind::Macro(_, string) => None, // TODO: not great, the location in the original source code is lost.
            NodeKind::Loop(_) => todo!(),
        }
    }
}

/// It's a binder because it's a collection of files!
#[derive(Debug)]
pub struct Binder<'fstack>(std::cell::Ref<'fstack, FstackImpl>);

impl Binder<'_> {
    fn get_file(&self, id: usize) -> Result<&Storage, codespan_reporting::files::Error> {
        match self.0.nodes.get(id) {
            Some(node) => Ok(node
                .storage()
                .expect("Diagnostic label generated within node not backed by storage")),
            None => Err(codespan_reporting::files::Error::FileMissing),
        }
    }
}

impl<'fstack> Files<'fstack> for Binder<'fstack> {
    type FileId = usize;
    type Name = &'fstack str;
    type Source = &'fstack str;

    fn name(
        &'fstack self,
        id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.get_file(id).map(Storage::name)
    }

    fn source(
        &'fstack self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.get_file(id).map(AsRef::as_ref)
    }

    fn line_index(
        &'fstack self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.get_file(id)?.line_start(byte_index).map_err(|max| {
            codespan_reporting::files::Error::IndexTooLarge {
                given: byte_index,
                max,
            }
        })
    }

    fn line_range(
        &'fstack self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        self.get_file(id)?.line_range(line_index).map_err(|max| {
            codespan_reporting::files::Error::LineTooLarge {
                given: line_index,
                max,
            }
        })
    }
}
