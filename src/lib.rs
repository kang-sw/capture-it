//!
//! # C++11-ish
//!
//! [@see](https://github.com/oliver-giersch/closure/blob/master/src/lib.rs)
//!

///
/// Generate a closure that captures specified variables, and captures all other
/// unspecified variables by **move**.
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
    ([$($args:tt)*] |$($params:tt $(:$param_type:ty)?),*| $($deco:ident)* { $($content:tt)* }) => {
        move |$($params $(:$param_type)?),*| $($deco)* {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] |$($params:tt $(:$param_type:ty)?),*| -> $rt:ty { $($content:tt)* }) => {
        move |$($params $(:$param_type)?),*| -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] |$($params:tt $(:$param_type:ty)?),*| $content:expr) => {
        move |$($params $(:$param_type)?),*| {
            $crate::__touch_all!($($args)*,);
            $content
        }
    };

    /* ------------------------------------- Zero-parameter ------------------------------------- */
    ([$($args:tt)*] || $($deco:ident)* {$($content:tt)*}) => {
        move || $($deco)* {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] || -> $rt:ty { $($content:tt)* }) => {
        move || -> $rt {
            $crate::__touch_all!($($args)*,);
            $($content)*
        }
    };

    ([$($args:tt)*] || $content:expr) => {
        move || {
            $crate::__touch_all!($($args)*,);
            $content
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
        let _g = Clone::clone(&$($ids).+);
        let $crate::__last_tok!($($ids).+) = _g; // this helps you to get rust-analyzer support.
        $crate::__capture!($($tail)*);
    };

    (* $($ids:ident).+, $($tail:tt)*) => {
        let _g = Clone::clone(&$($ids).+);
        let $crate::__last_tok_mut!($($ids).+) = _g;
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

    ($(,)*) => {};
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
    use std::rc::Rc;

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
            || {
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
        let closure = capture!([], || 5 * 5);
        assert_eq!(closure(), 25);
    }

    #[test]
    fn no_capture_with_arg() {
        let closure = capture!([], |x| x * x);
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_arg_and_type_hint() {
        let closure = capture!([], |x: usize| x * x);
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_arg_and_return_type() {
        let closure = capture!([], |x: usize| -> usize { x * x });
        assert_eq!(closure(5), 25);
    }

    #[test]
    fn no_capture_with_return_type() {
        let closure = capture!([], || -> &str { "result" });
        assert_eq!(closure(), "result");
    }

    #[test]
    fn capture_by_move() {
        let string = "move".to_string();
        let closure = capture!([], || string.len());
        assert_eq!(closure(), 4);
    }

    #[test]
    fn capture_by_ref() {
        let var = -1;
        let closure = capture!([&var], || *var == -1);
        assert!(closure());
    }

    #[test]
    fn capture_by_ref_mut() {
        let mut var = -1;
        capture!([&mut var], || *var *= -1)();
        assert_eq!(var, 1);
    }

    #[test]
    fn capture_multiple_mixed() {
        let borrow = 1;
        let mut borrow_mut = 1;
        let string = "move".to_string();

        let closure = capture!([&borrow, &mut borrow_mut, *string], || {
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
            |expected| -> bool {
                assert_eq!(Rc::strong_count(&rc), 3);
                *rc == expected
            }
        );
        assert!(closure(0));
    }
}
