use std::{
    cell::RefCell,
    rc::Rc,
    mem,
};

use tree_sitter::{Parser, Tree, Point, Language, InputEdit, Node, TreeCursor};

// TODO: We probably don't need RefCell.
pub type SharedTree = Rc<RefCell<Tree>>;

pub fn shared<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
}

// XXX: Don't juggle with alignment...
const NODE_LEN: usize = mem::size_of::<Node>() / 8;
type RawNode = [u64; NODE_LEN];

#[derive(Clone)]
pub struct WrappedNode {
    pub(crate) tree: SharedTree,
    raw: RawNode,
}

impl WrappedNode {
    pub unsafe fn new(tree: &SharedTree, node: Node) -> Self {
        let ptr = (&node as *const Node) as *const RawNode;

        //        let s = std::slice::from_raw_parts(ptr as *const u8, NODE_LEN);
        //        let mut raw = [0u8; NODE_LEN];
        //        raw.copy_from_slice(s);

        // XXX
        let mut raw = mem::MaybeUninit::uninit();
        std::ptr::copy_nonoverlapping(ptr, raw.as_mut_ptr(), 1);
        let raw = raw.assume_init();

        Self { tree: tree.clone(), raw }
    }

    pub fn inner(&self) -> &Node {
        let ptr = (&self.raw as *const RawNode) as *const Node;
        unsafe { ptr.as_ref() }.unwrap()
    }
}

//impl Deref for WrappedNode {
//    type Target = Node<'static>;
//
//    fn deref(&self) -> &Node<'static> {
//        let ptr = (&self.raw as *const RawNode) as *const Node;
//        unsafe { ptr.as_ref() }.unwrap()
//    }
//}

//pub trait Foo<'t>: Sized + 'static {
//    type Inner: Sized + 't;
//
//    unsafe fn new(tree: &SharedTree, inner: Self::Inner) -> Self {
//        unimplemented!()
//    }
//}
//
//pub struct Wrapped<R> {
//    pub(crate) tree: SharedTree,
//    raw: R,
//}
//
//impl<'t> Foo<'t> for Wrapped<RawNode> {
//    type Inner = Node<'t>;
//}

//impl<R> Foo for Wrapped<R> {
//    pub unsafe fn new<T: Sized>(tree: &SharedTree, inner: T) -> Self {
//        let ptr = (&inner as *const T) as *const R;
//
//        let mut raw = mem::MaybeUninit::uninit();
//        std::ptr::copy_nonoverlapping(ptr, raw.as_mut_ptr(), 1);
//        let raw = raw.assume_init();
//
//        Self { tree: tree.clone(), raw }
//    }
//}

//impl<'t, R: Sized> Deref for Wrapped<R> where Wrapped<R>: Foo<'t> {
//    type Target = <Wrapped<R> as Foo<'t>>::Inner;
//
//    fn deref(&self) -> &Self::Target {
//        unimplemented!()
//    }
//
////    fn deref(&self) -> &Target {
////        let ptr = (&self.raw as *const R) as *const T;
////        unsafe { ptr.as_ref() }.unwrap()
////    }
//}

// XXX: Don't juggle with alignment...
const TREE_CURSOR_LEN: usize = mem::size_of::<TreeCursor>() / 8;
type RawCursor = [u64; TREE_CURSOR_LEN];

#[derive(Clone)]
pub struct WrappedCursor {
    tree: SharedTree,
    raw: RawCursor,
}

impl WrappedCursor {
    pub unsafe fn new(tree: &SharedTree, cursor: TreeCursor) -> Self {
        let ptr = (&cursor as *const TreeCursor) as *const RawCursor;

        // XXX
        let mut raw = mem::MaybeUninit::uninit();
        std::ptr::copy_nonoverlapping(ptr, raw.as_mut_ptr(), 1);
        let raw = raw.assume_init();

        Self { tree: tree.clone(), raw }
    }

    pub fn inner(&self) -> &TreeCursor {
        let ptr = (&self.raw as *const RawCursor) as *const TreeCursor;
        unsafe { ptr.as_ref() }.unwrap()
    }
}
