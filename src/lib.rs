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

///
/// Generate a closure that captures specified variables, and captures all other
/// unspecified variables by **move**.
///
/// The first argument to the macro is always a list of capture arguments wrapped in [],
/// and you pass a closure or async block to the second argument, separated by commas.
/// In this case, the second argument must be explicitly specified as move capture.
///
/// # Usage
///
/// ### capture by copy
///
/// If you specify a variable name in square brackets, the [`Clone::clone`] function is called
/// against that variable, creating a variable of the same name and capturing it with the
/// specified closure. (Capturing the variable is always guaranteed, even if you don't
/// explicitly use it within the block.)
///
/// ```rust
/// let a = 1;
/// let closure = capture_it::capture!([a], move || { a });
/// assert_eq!(closure(), 1);
/// ```
///
/// All of these rules, except for reference capture (the & prefix), which we'll discuss later,
/// can be declared mutable by prefixing the variable name with * (an asterisk).
///
/// (NOTE: We originally wanted to use the `mut` keyword prefix, but that would cause
/// `rustfmt` to consider the macro expression as unjustified rust code and disable formatting,
/// so we were forced to adopt this unorthodox method)
///
/// ```rust
/// let count = 0;
/// let mut closure = capture_it::capture!([*count], move || { count += 1; count });
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
/// You can explicitly reference-capture a variable by prefixing its name with & or &mut. (Note
/// that all variables not specified in the capture list will be MOVE-captured, as only blocks with
/// a MOVE policy are allowed as second arguments. Any variables you want to capture by reference
/// must be explicitly specified in the capture list)
///
/// ```rust
/// let a = std::cell::Cell::new(1);
/// let closure = capture_it::capture!([&a], move || { a.get() });
/// a.set(2);
/// assert_eq!(closure(), 2);
/// ```
///
/// ### capture by alias
///
/// Similar to the lambda capture rules in modern C++, it is possible to capture an expression
/// by giving it an alias.
///
/// ```rust
/// let mut closure = capture_it::capture!([*a = 0], move || { a += 1; a });
///
/// assert_eq!(closure(), 1);
/// assert_eq!(closure(), 2);
/// assert_eq!(closure(), 3);
/// ```
///
/// ### capture struct fields
///
/// Under limited conditions, you can capture struct fields. The following expressions will
/// capture each struct field as a copy and a reference, respectively.
///
/// ```rust
/// struct Foo {
///     copied: i32,
///     borrowed: std::cell::Cell<i32>,
/// }
///
/// let mut foo = Foo { copied: 1, borrowed: 2.into() };
/// let closure = capture_it::capture!([foo.copied, &foo.borrowed], move || {
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
/// let task = capture_it::capture!([copied, &borrowed], async move {
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
#[macro_export]
macro_rules! capture {
    ([$($args:tt)*], $($closure:tt)*) => {{
        $crate::__capture!($($args)*,);
        $crate::__wrap_touched!([$($args)*] $($closure)*)
    }};
}

/// Generate a closure that touches all specified variables, which makes all of them to be
/// captures even if they are not used in the provided closure.
#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __wrap_touched {
    /* --------------------------------------- Parametered -------------------------------------- */
    ([$($args:tt)*] move |$($params:tt $(:$param_type:ty)?),*| $($deco:ident)* { $($content:tt)* }) => {
        move |$($params $(:$param_type)?),*| $($deco)* {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] move |$($params:tt $(:$param_type:ty)?),*| -> $rt:ty { $($content:tt)* }) => {
        move |$($params $(:$param_type)?),*| -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] move |$($params:tt $(:$param_type:ty)?),*| $content:expr) => {
        move |$($params $(:$param_type)?),*| {
            $crate::__touch_all!($($args)*,);
            $content
        }
    };

    /* ------------------------------------- Zero-parameter ------------------------------------- */
    ([$($args:tt)*]move  || $($deco:ident)* {$($content:tt)*}) => {
        move || $($deco)* {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] move || -> $rt:ty { $($content:tt)* }) => {
        move || -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] move || $content:expr) => {
        move || {
            $crate::__touch_all!($($args)*,);
            $content
        }
    };

    /* ------------------------------------------ Async ----------------------------------------- */
    ([$($args:tt)*] async move {$($content:tt)*}) => {
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
        drop(&$v);
        $crate::__touch_all!($($tail)*);
    };

    (* $v:ident, $($tail:tt)*) => {
        drop(&$v);
        $crate::__touch_all!($($tail)*);
    };

    /* -------------------------------------- By Reference -------------------------------------- */
    (&$v:ident, $($tail:tt)*) => {
        drop(&$v);
        $crate::__touch_all!($($tail)*);
    };

    (&mut $v:ident, $($tail:tt)*) => {
        drop(&$v);
        $crate::__touch_all!($($tail)*);
    };

    /* -------------------------------------- By Expression ------------------------------------- */
    ($a:ident=$_:expr, $($tail:tt)*) => {
        drop(&$a);
        $crate::__touch_all!($($tail)*);
    };

    (* $a:ident=$_:expr, $($tail:tt)*) => {
        drop(&$a);
        $crate::__touch_all!($($tail)*);
    };

    /* ------------------------------------- Struct By Copy ------------------------------------- */
    ($($ids:ident).+, $($tail:tt)*) => {
        drop(&$crate::__last_tok!($($ids).+));
        $crate::__touch_all!($($tail)*);
    };

    (* $($ids:ident).+, $($tail:tt)*) => {
        drop(&$crate::__last_tok!($($ids).+));
        $crate::__touch_all!($($tail)*);
    };

    /* ----------------------------------- Struct By Reference ---------------------------------- */
    (&$($ids:ident).+, $($tail:tt)*) => {
        drop(&$crate::__last_tok!($($ids).+));
        $crate::__touch_all!($($tail)*);
    };

    (&mut $($ids:ident).+, $($tail:tt)*) => {
        drop(&$crate::__last_tok!($($ids).+));
        $crate::__touch_all!($($tail)*);
    };

    /* ----------------------------------------- By Ops ----------------------------------------- */
    ($ops:ident ($v:ident), $($tail:tt)*) => {
        drop(&$v);
        $crate::__touch_all!($($tail)*);
    };

    (*$ops:ident ($v:ident), $($tail:tt)*) => {
        drop(&$v);
        $crate::__touch_all!($($tail)*);
    };

    ($ops:ident ($($ids:ident).+), $($tail:tt)*) => {
        drop(&$crate::__last_tok!($($ids).+));
        $crate::__touch_all!($($tail)*);
    };

    (*$ops:ident ($($ids:ident).+), $($tail:tt)*) => {
        drop(&$crate::__last_tok!($($ids).+));
        $crate::__touch_all!($($tail)*);
    };

    /* ----------------------------------------- Escape ----------------------------------------- */
    ($(,)*) => {};
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __capture {
    /* ----------------------------------------- By Copy ---------------------------------------- */
    ($v:ident, $($tail:tt)*) => {
        let $v = Clone::clone(&$v);
        $crate::__capture!($($tail)*);
    };

    (* $v:ident, $($tail:tt)*) => {
        let mut $v = Clone::clone(&$v);
        $crate::__capture!($($tail)*);
    };

    /* -------------------------------------- By Reference -------------------------------------- */
    (&$v:ident, $($tail:tt)*) => {
        let $v = &$v;
        $crate::__capture!($($tail)*);
    };

    (&mut $v:ident, $($tail:tt)*) => {
        let $v = &mut $v;
        $crate::__capture!($($tail)*);
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
        let __capture_it = Clone::clone(&$($ids).+);
        let $crate::__last_tok!($($ids).+) = __capture_it; // this helps you to get rust-analyzer support.
        $crate::__capture!($($tail)*);
    };

    (* $($ids:ident).+, $($tail:tt)*) => {
        let __capture_it = Clone::clone(&$($ids).+);
        let $crate::__last_tok_mut!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    /* ----------------------------------- Struct By Reference ---------------------------------- */
    (&$($ids:ident).+, $($tail:tt)*) => {
        let $crate::__last_tok!($($ids).+) = &$($ids).+;
        $crate::__capture!($($tail)*);
    };

    (&mut $($ids:ident).+, $($tail:tt)*) => {
        let $crate::__last_tok!($($ids).+) = &mut $($ids).+;
        $crate::__capture!($($tail)*);
    };

    /* ----------------------------------------- By Ops ----------------------------------------- */
    ($ops:ident ($v:ident), $($tail:tt)*) => {
        let $v = $crate::__apply_ops($ops, $v);
        $crate::__capture!($($tail)*);
    };

    (*$ops:ident ($v:ident), $($tail:tt)*) => {
        let mut $v = $crate::__apply_ops($ops, $v);
        $crate::__capture!($($tail)*);
    };

    ($ops:ident ($($ids:ident).+), $($tail:tt)*) => {
        let __capture_it = $crate::__apply_ops($ops, $v);
        let $crate::__last_tok!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    (*$ops:ident ($($ids:ident).+), $($tail:tt)*) => {
        let __capture_it = $crate::__apply_ops($ops, $v);
        let $crate::__last_tok_mut!($($ids).+) = __capture_it;
        $crate::__capture!($($tail)*);
    };

    /* ----------------------------------------- Escape ----------------------------------------- */
    ($(,)*) => {};
}

#[macro_export(local_inner_macros)]
#[doc(hidden)]
macro_rules! __apply_ops {
    (own, $v:ident) => {
        std::borrow::ToOwned::to_owned(&$v)
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
        let strt = Foo {
            inner: ss.clone(),
            inner_mut: ss.clone(),
        };

        let _ = capture!(
            [
                foo,
                *bar,
                &baz,
                &mut qux,
                cloned = bar.clone(),
                *other = cloned.clone(),
                oar = 3,
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
        let closure = capture!(
            [rc, _unused_but_captured = rc.clone()],
            move |expected| -> bool {
                assert_eq!(Rc::strong_count(&rc), 3);
                *rc == expected
            }
        );
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

        let mut val = Bar {
            borrowed: 1,
            copied: 2,
        };

        let mut closure = capture!([&mut val.borrowed, val.copied], move || {
            assert_eq!(*borrowed, 1);
            assert_eq!(copied, 2);

            *borrowed = 3;
        });

        closure();
        assert_eq!(val.borrowed, 3);
        assert_eq!(val.copied, 2);
    }
}
