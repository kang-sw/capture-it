//!
//! # Modern C++-ish closure macros.
//!
//! This crate provides the [`capture`] macro, which implements C++11's lambda-style capture
//! functionality. It uses only a list of AST-parsable tokens, so it can be automatically
//! formatted by `rustfmt`.
//!
//! most of macro expressions are inspired* from the crate
//! [`oliver-giersch/closure`](https://github.com/oliver-giersch/closure/blob/master/src/lib.rs)
//!

#![cfg_attr(all(feature = "no-std", not(test)), no_std)]

///
/// Generate a closure that captures specified variables, and captures all other unspecified
/// variables by **move**.
///
/// The first argument to the macro is always a list of capture arguments wrapped in [], and you
/// pass a closure or async block to the second argument, separated by commas. In this case, the
/// second argument must be explicitly specified as move capture.
///
/// # Usage
///
/// ### capture by copy
///
/// If you specify a variable name in square brackets, the [`Clone::clone`] function is called
/// against that variable, creating a variable of the same name and capturing it with the specified
/// closure. (Capturing the variable is always guaranteed, even if you don't explicitly use it
/// within the block.)
///
/// ```rust
/// let a = 1;
/// let closure = capture_it::capture!([a]..|| { a });
/// assert_eq!(closure(), 1);
/// ```
///
/// All of these rules, except for reference capture (the & prefix), which we'll discuss later, can
/// be declared mutable by prefixing the variable name with * (an asterisk).
///
/// (NOTE: We originally wanted to use the `mut` keyword prefix, but that would cause `rustfmt` to
/// consider the macro expression as unjustified rust code and disable formatting, so we were forced
/// to adopt this unorthodox method)
///
/// ```rust
/// let count = 0;
/// let mut closure = capture_it::capture!([*count]..|| { count += 1; count });
///
/// assert_eq!(closure(), 1);
/// assert_eq!(closure(), 2);
/// assert_eq!(closure(), 3);
///
/// assert_eq!(count, 0); // as it was copied ...
/// ```
///
/// ### capture by reference
///
/// You can explicitly reference-capture a variable by prefixing its name with & or &mut. (Note that
/// all variables not specified in the capture list will be MOVE-captured, as only blocks with a
/// MOVE policy are allowed as second arguments. Any variables you want to capture by reference must
/// be explicitly specified in the capture list)
///
/// ```rust
/// let a = std::cell::Cell::new(1);
/// let closure = capture_it::capture!([&a]..|| { a.get() });
/// a.set(2);
/// assert_eq!(closure(), 2);
/// ```
///
/// ### capture by alias
///
/// Similar to the lambda capture rules in modern C++, it is possible to capture an expression by
/// giving it an alias.
///
/// ```rust
/// let mut closure = capture_it::capture!([*a = 0]..|| { a += 1; a });
///
/// assert_eq!(closure(), 1);
/// assert_eq!(closure(), 2);
/// assert_eq!(closure(), 3);
/// ```
///
/// ### capture struct fields
///
/// Under limited conditions, you can capture struct fields. The following expressions will capture
/// each struct field as a copy and a reference, respectively.
///
/// ```rust
/// struct Foo {
///     copied: i32,
///     borrowed: std::cell::Cell<i32>,
/// }
///
/// let mut foo = Foo { copied: 1, borrowed: 2.into() };
/// let closure = capture_it::capture!([foo.copied, &foo.borrowed]..|| {
///     copied + borrowed.get()
/// });
///
/// foo.copied = 9999;
/// foo.borrowed.set(3);
/// assert_eq!(closure(), 4);
/// ```
///
/// ### async blocks
///
/// All rules apply equally to the `async move` block.
///
/// ```rust
/// let mut copied = 1;
/// let borrowed = std::cell::Cell::new(2);
/// let task = capture_it::capture!([copied, &borrowed]..async {
///    copied + borrowed.get()
/// });
///
/// copied = 9999;
/// borrowed.set(3);
///
/// let val = futures::executor::block_on(task);
/// assert_eq!(val, 4);
/// ```
///
/// ## Shortcuts
///
/// There are various shortcuts for capturing variables for convenience.
///
/// - `Own`: Call the [`ToOwned::to_owned`] method on the target variable.
/// - `Weak`: Downgrades a [`std::rc::Rc`] or [`std::sync::Arc`] type to a Weak reference.
/// - `Some`: Wrap cloned variable with [`Option`]. This is useful when you have to retrieve
///   captured variable when it's not a [`FnOnce`] closure.
/// - All paths that do not fall under the above rules (`$($p:ident)::*`) are replaced with function
///   calls.
/// - You can simply write single method invocation on the captured variable, to capture the result
///   of return value. `capture!([foo.bar()], move || { ... })` -> in this case, `let foo =
///   foo.bar()` will be captured into the closure.
///
/// ```rust
/// #[cfg(not(feature = "no-std"))]
/// {
///     use capture_it::capture;
///
///     let hello = "hello, world!";
///     let rc = std::rc::Rc::new(());
///     let arc = std::sync::Arc::new(());
///     let arc_2 = arc.clone();
///     let hello_other = "hello, other!";
///
///     let closure = capture!([Own(hello), Weak(rc), Weak(arc), arc_2, *hello_other.to_string()]..|| {
///         assert_eq!(hello, "hello, world!");
///         assert!(rc.upgrade().is_none());
///         assert!(arc.upgrade().is_some());
///         assert_eq!(hello_other, "hello, other!");
///     });
///
///     drop((rc, arc));
///     closure();
/// }
/// ```
#[macro_export]
macro_rules! capture {
    ([$($args:tt)*], $($closure:tt)*) => {{
        $crate::__capture!($($args)*,);
        $crate::__wrap_touched!([$($args)*] $($closure)*)
    }};

    ([$($args:tt)*]..$($closure:tt)*) => {{
        $crate::__capture!($($args)*,);
        $crate::__wrap_touched_v2!([$($args)*] $($closure)*)
    }};

}

/// Generate a closure that touches all specified variables, which makes all of them to be
/// captures even if they are not used in the provided closure.
#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __wrap_touched {
    /* --------------------------------------- Parametered -------------------------------------- */
    ([$($args:tt)*] move |$($params:tt $(:$param_type:ty)?),*| $expr:expr) => {
        move |$($params $(:$param_type)?),*| {
            $crate::__touch_all!($($args)*,);
            $expr
        }
    };

    ([$($args:tt)*] move |$($params:tt $(:$param_type:ty)?),*| -> $rt:ty { $($content:tt)* }) => {
        move |$($params $(:$param_type)?),*| -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    /* --------------------------------------- Zero Params -------------------------------------- */
    ([$($args:tt)*] move || $content:expr) => {
        move || {
            $crate::__touch_all!($($args)*,);
            $content
        }
    };

    ([$($args:tt)*] move || -> $rt:ty { $($content:tt)* }) => {
        move || -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    /* --------------------------------------- Async Move --------------------------------------- */
    ([$($args:tt)*] async move {$($content:tt)*}) => {
        async move {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };
}

/// Doesn't require 'move' keyword here.
#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __wrap_touched_v2 {
    /* --------------------------------------- Parametered -------------------------------------- */
    ([$($args:tt)*] |$($params:tt $(:$param_type:ty)?),*| $expr:expr) => {
        move |$($params $(:$param_type)?),*| {
            $crate::__touch_all!($($args)*,);
            $expr
        }
    };

    ([$($args:tt)*] |$($params:tt $(:$param_type:ty)?),*| -> $rt:ty { $($content:tt)* }) => {
        move |$($params $(:$param_type)?),*| -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    /* --------------------------------------- Zero Params -------------------------------------- */
    ([$($args:tt)*] || $content:expr) => {
        move || {
            $crate::__touch_all!($($args)*,);
            $content
        }
    };

    ([$($args:tt)*] || -> $rt:ty { $($content:tt)* }) => {
        move || -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    /* --------------------------------------- Async Move --------------------------------------- */
    ([$($args:tt)*] async {$($content:tt)*}) => {
        async move {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };
}

/// Enforces the compiler to capture all variables in the closure.
#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __touch_all {
    /* ----------------------------------------- By Copy ---------------------------------------- */
    ($v:ident, $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    (* $v:ident, $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    /* -------------------------------------- By Reference -------------------------------------- */
    (&$v:ident, $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    (&mut $v:ident, $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    /* -------------------------------------- By Expression ------------------------------------- */
    ($a:ident=$_:expr, $($tail:tt)*) => {
        let _ = &$a;
        $crate::__touch_all!($($tail)*);
    };

    (* $a:ident=$_:expr, $($tail:tt)*) => {
        let _ = &$a;
        $crate::__touch_all!($($tail)*);
    };

    /* ------------------------------------- Struct By Copy ------------------------------------- */
    ($($ids:ident).+, $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    (* $($ids:ident).+, $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    /* ----------------------------------- Struct By Reference ---------------------------------- */
    (&$($ids:ident).+, $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    (&mut $($ids:ident).+, $($tail:tt)*) => {
        let _ = $crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    /* ----------------------------------------- By Ops ----------------------------------------- */
    ($($ops:ident)::* ($v:ident), $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    (*$($ops:ident)::* ($v:ident), $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    ($($ops:ident)::* ($($ids:ident).+), $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    (*$($ops:ident)::* ($($ids:ident).+), $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    /* -------------------------------------- By Ops - Ref -------------------------------------- */
    ($($ops:ident)::* (&$v:ident), $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    (*$($ops:ident)::* (&$v:ident), $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    ($($ops:ident)::* (&$($ids:ident).+), $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    (*$($ops:ident)::* (&$($ids:ident).+), $($tail:tt)*) => {
        let _ = &$crate::__last_tok!($($ids).+);
        $crate::__touch_all!($($tail)*);
    };

    /* ---------------------------------- By Self.* Invocation ---------------------------------- */
    (*$v:ident.$expr:ident($($args:expr),*), $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    ($v:ident.$expr:ident($($args:expr),*), $($tail:tt)*) => {
        let _ = &$v;
        $crate::__touch_all!($($tail)*);
    };

    /* ----------------------------------------- Escape ----------------------------------------- */
    ($(,)*) => {};
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __capture {
    /* ----------------------------------------- By Ops ----------------------------------------- */
    ($($ops:ident)::* ($v:ident), $($tail:tt)*) => {
        let $v = $crate::__apply_ops!($($ops)::*, $v);
        $crate::__capture!($($tail)*);
    };

    (*$($ops:ident)::* ($v:ident), $($tail:tt)*) => {
        let mut $v = $crate::__apply_ops!($($ops)::*, $v);
        $crate::__capture!($($tail)*);
    };

    ($($ops:ident)::* ($($ids:ident).+), $($tail:tt)*) => {
        let __capture_it = $crate::__apply_ops!($($ops)::*, $($ids).+);
        let $crate::__last_tok!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    (*$($ops:ident)::* ($($ids:ident).+), $($tail:tt)*) => {
        let __capture_it = $crate::__apply_ops!($($ops)::*, $($ids).+);
        let $crate::__last_tok_mut!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    /* ----------------------------------- By Ops - & Prefixed ---------------------------------- */
    ($($ops:ident)::* (&$v:ident), $($tail:tt)*) => {
        let $v = $crate::__apply_ops!($($ops)::*, &$v);
        $crate::__capture!($($tail)*);
    };

    (*$($ops:ident)::* (&$v:ident), $($tail:tt)*) => {
        let mut $v = $crate::__apply_ops!($($ops)::*, &$v);
        $crate::__capture!($($tail)*);
    };

    ($($ops:ident)::* (&$($ids:ident).+), $($tail:tt)*) => {
        let __capture_it = $crate::__apply_ops!($($ops)::*, &$($ids).+);
        let $crate::__last_tok!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    (*$($ops:ident)::* (&$($ids:ident).+), $($tail:tt)*) => {
        let __capture_it = $crate::__apply_ops!($($ops)::*, &$($ids).+);
        let $crate::__last_tok_mut!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    /* ---------------------------------- By Self.* Invocation ---------------------------------- */
    (*$v:ident.$expr:ident($($args:expr),*), $($tail:tt)*) => {
        let mut $v = $v.$expr($($args),*);
        $crate::__capture!($($tail)*);
    };

    ($v:ident.$expr:ident($($args:expr),*), $($tail:tt)*) => {
        let $v = $v.$expr($($args),*);
        $crate::__capture!($($tail)*);
    };

    /* ----------------------------------------- By Copy ---------------------------------------- */
    ($v:ident, $($tail:tt)*) => {
        $crate::__capture!(Clone::clone(&$v), $($tail)*);
    };

    (* $v:ident, $($tail:tt)*) => {
        $crate::__capture!(*Clone::clone(&$v), $($tail)*);
    };

    /* -------------------------------------- By Reference -------------------------------------- */
    (&$v:ident, $($tail:tt)*) => {
        $crate::__capture!(__Built_In::refer($v), $($tail)*);
    };

    (&mut $v:ident, $($tail:tt)*) => {
        $crate::__capture!(__Built_In::refer_mut($v), $($tail)*);
    };

    /* -------------------------------------- By Expression ------------------------------------- */
    ($a:ident=$v:expr, $($tail:tt)*) => {
        let $a = $v;
        $crate::__capture!($($tail)*);
    };

    (* $a:ident=$v:expr, $($tail:tt)*) => {
        let mut $a = $v;
        $crate::__capture!($($tail)*);
    };

    /* ------------------------------------- Struct By Copy ------------------------------------- */
    ($($ids:ident).+, $($tail:tt)*) => {
        $crate::__capture!(Clone::clone(&$($ids).+), $($tail)*);
    };

    (* $($ids:ident).+, $($tail:tt)*) => {
        $crate::__capture!(*Clone::clone(&$($ids).+), $($tail)*);
    };

    /* ----------------------------------- Struct By Reference ---------------------------------- */
    (&$($ids:ident).+, $($tail:tt)*) => {
        $crate::__capture!(__Built_In::refer($($ids).+), $($tail)*);
    };

    (&mut $($ids:ident).+, $($tail:tt)*) => {
        $crate::__capture!(__Built_In::refer_mut($($ids).+), $($tail)*);
    };

    /* ----------------------------------------- Escape ----------------------------------------- */
    ($(,)*) => {};
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __apply_ops {
    (Own, $($v:tt)*) => {
        ($($v)*).to_owned()
    };

    (Weak, $($v:tt)*) => {
        $crate::__sync_help::Downgrade::downgrade(&$($v)*)
    };

    (Some, $($v:tt)*) => {
        Some(Clone::clone(&$($v)*))
    };

    (__Built_In::refer, $($v:tt)*) => {
        &$($v)*
    };

    (__Built_In::refer_mut, $($v:tt)*) => {
        &mut $($v)*
    };

    ($($ops:ident)::*, $($v:tt)*) => {
        ($($ops)::*)($($v)*)
    };

    ($($ops:ident)::*, &$($v:tt)*) => {
        ($($ops)::*)(&$($v)*)
    };
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __last_tok {
    ($a:ident) => { $a };
    ($a:ident. $($tail:tt)*) => { $crate::__last_tok!($($tail)*) };
    () => { compile_error!("??") };
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __last_tok_mut {
    ($a:ident) => { mut $a };
    ($a:ident. $($tail:tt)*) => { $crate::__last_tok_mut!($($tail)*) };
    () => { compile_error!("??") };
}

#[cfg(not(feature = "no-std"))]
pub mod __sync_help {
    pub trait Downgrade {
        type Weak;

        fn downgrade(&self) -> Self::Weak;
    }

    impl<T> Downgrade for std::rc::Rc<T> {
        type Weak = std::rc::Weak<T>;

        fn downgrade(&self) -> Self::Weak {
            std::rc::Rc::downgrade(self)
        }
    }

    impl<T> Downgrade for std::sync::Arc<T> {
        type Weak = std::sync::Weak<T>;

        fn downgrade(&self) -> Self::Weak {
            std::sync::Arc::downgrade(self)
        }
    }
}

#[cfg(test)]
mod test {
    use std::{cell::Cell, rc::Rc};

    struct Foo {
        inner: Rc<()>,
        inner_mut: Rc<()>,
    }

    #[test]
    fn can_compile() {
        let ss = Rc::new(());
        let [foo, bar, baz, mut qux] = std::array::from_fn(|_| ss.clone());
        let ss_2 = ss.clone();
        let strt = Foo { inner: ss.clone(), inner_mut: ss.clone() };

        drop(ss);

        let _ = capture!(
            [
                foo,
                *bar,
                &baz,
                &mut qux,
                cloned = bar.clone(),
                *other = cloned.clone(),
                oar = 3,
                ss_2.clone(),
                strt.inner,
                *strt.inner_mut,
            ],
            move || {
                drop((other, inner_mut, bar));

                bar = Default::default();
                other = Default::default();
                inner_mut = Default::default();

                drop((foo, bar, baz, qux, cloned, inner, other, inner_mut));
            }
        );

        #[derive(Default)]
        struct Foo2 {
            va: usize,
        }

        assert!(capture!([], move || Foo2 { va: 5 })().va == 5);
        assert!(capture!([va = 5], move || Foo2 { va })().va == 5);
        assert!(capture!([], move || Foo2::default())().va == 0);
    }

    // shamelessly cloned test cases from https://github.com/oliver-giersch/closure/blob/master/src/lib.rs
    #[test]
    fn no_capture_one_line() {
        let closure = capture!([], move || 5 * 5);
        assert_eq!(closure(), 25);
    }

    #[test]
    fn no_capture_with_arg() {
        let closure = capture!([], move |x| x * x);
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_arg_and_type_hint() {
        let closure = capture!([], move |x: usize| x * x);
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_arg_and_return_type() {
        let closure = capture!([], move |x: usize| -> usize { x * x });
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_return_type() {
        let closure = capture!([], move || -> &str { "result" });
        assert_eq!(closure(), "result");
    }

    #[test]
    fn capture_by_move() {
        let string = "move".to_string();
        let closure = capture!([], move || string.len());
        assert_eq!(closure(), 4);
    }

    #[test]
    fn capture_by_ref() {
        let var = -1;
        let closure = capture!([&var], move || *var == -1);
        assert!(closure());
    }

    #[test]
    fn capture_by_own() {
        let my_str = "move";
        let other_str = "move_2";
        let str = "move_3";
        let char_get = "hello";

        let closure = capture!(
            [
                ot = my_str,
                *Own(my_str),
                other_str.to_owned(),
                char_get.ends_with("llo"),
                String::from(str)
            ],
            move || {
                assert!(char_get);

                my_str.push_str(" back");
                my_str
            }
        );

        assert_eq!(closure(), "move back");
    }

    #[test]
    fn capture_by_misc_extension() {
        let var = -1;
        let other_str = "hello, world!".to_owned();

        let closure = capture!([&var, Some(other_str)], move || other_str.unwrap() + " go away!");

        assert!(closure() == "hello, world! go away!");
    }

    #[cfg(not(feature = "no-std"))]
    #[test]
    fn capture_by_downgrade() {
        let arc = std::sync::Arc::new("hell, world!");
        let rc = std::rc::Rc::new("hell, world!");
        let ww = std::rc::Rc::downgrade(&rc);

        let closure = capture!([Weak(arc), Weak(rc), ww.upgrade()], move || {
            drop(ww);
            (arc.upgrade().is_some(), rc.upgrade().is_some())
        });

        drop(arc);
        assert_eq!(closure(), (false, true));
    }

    #[test]
    fn capture_by_own_struct() {
        struct PewPew {
            inner: &'static str,
        }

        let my_pewpew = PewPew { inner: "move" };
        let closure = capture!([*Own(my_pewpew.inner), _r = 3], move || {
            inner.push_str(" back");
            inner
        });

        assert_eq!(closure(), "move back");
    }

    #[test]
    fn capture_by_ref_mut() {
        let mut var = -1;
        capture!([&mut var], move || *var *= -1)();
        assert_eq!(var, 1);
    }

    #[test]
    fn capture_multiple_mixed() {
        let borrow = 1;
        let mut borrow_mut = 1;
        let string = "move".to_string();

        let closure = capture!([&borrow, &mut borrow_mut, *string], move || {
            assert_eq!(*borrow, 1);
            *borrow_mut -= 1;
            string.push_str("d back");
            string
        });

        assert_eq!(&closure(), "moved back");
        assert_eq!(&string, "move");
    }

    #[test]
    fn capture_by_clone() {
        let rc = Rc::new(0);
        let closure = capture!([rc, _unused_but_captured = rc.clone()], move |expected| -> bool {
            assert_eq!(Rc::strong_count(&rc), 3);
            *rc == expected
        });
        assert!(closure(0));
    }

    #[test]
    fn async_closure() {
        let rc = Rc::new(Cell::new(0));
        let mut borrowed = 1;
        let copied = 2;
        let task = capture!([rc, copied, &mut borrowed], async move {
            assert!(rc.get() == 2);
            rc.set(1);
            *borrowed = 2;
        });

        rc.set(2);
        let rc = rc; // to check if rc was correctly copied, not referenced ...

        futures::executor::block_on(task);
        assert!(rc.get() == 1);
        assert!(borrowed == 2);
        assert!(copied == 2);
    }

    #[test]
    fn struct_field() {
        struct Bar {
            borrowed: i32,
            copied: i32,
        }

        let mut val = Bar { borrowed: 1, copied: 2 };

        let mut closure = capture!([&mut val.borrowed, val.copied], move || {
            assert_eq!(*borrowed, 1);
            assert_eq!(copied, 2);

            *borrowed = 3;
        });

        closure();
        assert_eq!(val.borrowed, 3);
        assert_eq!(val.copied, 2);
    }

    #[test]
    #[allow(unused_must_use)]
    #[allow(unused_unsafe)]
    fn test_compilation() {
        let val = 1;
        let val2 = 2;

        use capture as c_;

        c_!([val, val2], move || val + val2);
        c_!([val, val2]..|| val + val2);

        c_!([val, val2], move |_: i32| val + val2);
        c_!([val, val2]..|_: i32| val + val2);

        c_!([val, val2], move |_: i32| -> i32 { val + val2 });
        c_!([val, val2]..|_: i32| -> i32 { val + val2 });

        c_!([val, val2], move || -> i32 { val + val2 });
        c_!([val, val2]..|| -> i32 { val + val2 });

        c_!([val, val2], move || async move { val + val2 });
        c_!([val, val2]..|| async move { val + val2 });

        c_!([val, val2], move || async {});
        c_!([val, val2]..|| async {});

        c_!([val, val2], async move {});
        c_!([val, val2]..async {});

        c_!([]..async { val });
        assert!(3 == capture!([val, val2], move || val + val2)());
        assert!(3 == c_!([val, val2]..|| val + val2)());
        assert!(3 == c_!([val, val2]..|| unsafe { val + val2 })());
        assert!(3 == c_!([val, val2]..|| return val + val2)());
        assert!(
            c_!([val, val2]..|| {
                return val + val2;
            })() == 3
        );
        c_!([val, val2], async move {});
        c_!([val, val2], move |_: i32, _x| unsafe { core::mem::zeroed::<i32>() })(1, 2);

        struct MyType(i32, i32);
        c_!([val, val2], move |_: i32, _: i32| MyType(1, 22));

        struct MyType2 {
            _a: i32,
            _b: i32,
        }
        capture!([val, val2], move |_: i32, _: i32| MyType2 { _a: 1, _b: 22 });
    }
}
