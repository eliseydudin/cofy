use std::sync::atomic::{AtomicI32, Ordering};

use super::{AllocError, Arena};

#[test]
fn test_alloc() {
    let arena = Arena::new(10);
    let reften = arena.alloc(10);
    assert_eq!(*reften, 10);
    *reften = i32::MAX;
    assert_eq!(*reften, i32::MAX);
    assert_eq!(arena.allocated(), 4);
}

#[test]
fn test_out_of_bounds() {
    let arena = Arena::new(3);
    // the size of i32 is 4 bytes, which is bigger than the arena's block size
    let newref = arena.try_alloc(10);
    assert_eq!(newref, Err(AllocError::OutOfBounds))
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct Foo {
    foo: i32,
    bar: f64,
    baz: &'static str,
}

#[test]
fn test_struct_allocation() {
    let foo = Foo {
        foo: 10,
        bar: 6.1,
        baz: "foo",
    };
    let arena = Arena::new(64);
    let foo_on_the_arena = arena.alloc(foo);

    assert_eq!(*foo_on_the_arena, foo);
    foo_on_the_arena.foo *= 10;
    assert_eq!(foo_on_the_arena.foo, 100);
    assert_eq!(arena.allocated(), size_of_val(&foo))
}

#[test]
fn test_nonoverlapping() {
    let foo = Foo {
        foo: 10,
        bar: 6.1,
        baz: "foo",
    };

    let bar = Foo {
        foo: 10,
        bar: 6.1,
        baz: "bar",
    };

    let arena = Arena::new(100);
    let foo_on_arena = arena.alloc(foo);
    let bar_on_arena = arena.alloc(bar);

    assert_eq!(*foo_on_arena, foo);
    assert_eq!(*bar_on_arena, bar);
}

#[test]
fn test_reuse() {
    let foo = Foo {
        foo: 10,
        bar: 6.1,
        baz: "foo",
    };

    let arena = Arena::new(size_of_val(&foo));
    let arena_int = arena.alloc(10); // block index 0
    let arena_foo = arena.alloc(foo);
    // ^ block index 1 since the first block doesn't have enough memory for `Foo`

    assert_eq!(*arena_int, 10);
    assert_eq!(*arena_foo, foo);

    let arena_float = arena.alloc(6.1); // block index 1 since it fits f64
    assert_eq!(*arena_float, 6.1);

    // should only have block 0 and block 1
    assert_eq!(arena.len(), 2);
}

#[test]
fn test_drop() {
    static NUMBER: AtomicI32 = AtomicI32::new(0);

    struct AddOnDrop(i32);

    impl Drop for AddOnDrop {
        fn drop(&mut self) {
            NUMBER.fetch_add(self.0, Ordering::Relaxed);
        }
    }

    let arena = Arena::new(8);
    let add_on_drop = arena.alloc_boxed(AddOnDrop(10));
    drop(add_on_drop);

    assert_eq!(NUMBER.load(Ordering::Relaxed), 10);
}
