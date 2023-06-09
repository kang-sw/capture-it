# capture-it &emsp; ![crates.io](https://img.shields.io/crates/v/capture-it)

See [example](examples/usage.rs)

For detailed documentation, see [`capture_it::capture`](src/lib.rs)

## Usage

Creates closures with a syntax similar to modern C++'s lambda capture rules. The first argument to
the `capture!` macro is an array listing the arguments to be captured by the closure, and the second
argument specifies either an 'async move' block or a 'move' closure. (to more explicitly indicate
that the `move` closure is used, a compile-time error is raised for any async or closure
function missing the `move` tag).

The following example demonstrates how to create a generator closure by capturing an arbitrary expression (`=0`) with the `index` identifier.

```rust
    use capture_it::capture;

    // You can capture an expression, as we do in c++'s lambda capture.
    //
    // Any identifier prefixed with *(asterisk) declared as mutable.
    let mut gen = capture!([*index = 0], move || {
        index += 1;
        index
    });

    assert!((gen(), gen(), gen(), gen(), gen()) == (1, 2, 3, 4, 5));
```

Since the function arguments of the `capture` macro must use the `move` closure, reference captures must be explicitly listed; they are represented by the `&` or `&mut` prefix, as in normal rust syntax.

```rust
    use capture_it::capture;
    let mut num = 0;
    let mut gen = capture!([&mut num], move || {
        *num += 1;
        *num
    });

    assert!((gen(), gen(), gen(), gen(), gen()) == (1, 2, 3, 4, 5));
```

The `capture!` macro calls `Clone::clone` for every argument passed in, by default. This is a more
ergonomic way to create closures.

```rust
    use capture_it::capture;
    use std::sync::{Arc, Mutex};

    let arc = Arc::new(Mutex::new(0));

    // From this ...
    std::thread::spawn({
        let arc = arc.clone();
        move || {
            *arc.lock().unwrap() += 1;
        }
    });

    // To this
    std::thread::spawn(capture!([arc], move || {
        *arc.lock().unwrap() += 1;
    }));

    // The naive spin wait ...
    while Arc::strong_count(&arc) > 1 {
        std::thread::yield_now();
    }

    assert_eq!(*arc.lock().unwrap(), 2);
```

This macro is particularly useful when you need to pass multiple `Arc` instances through `Clone` to different closures. Take a look at the following example to see how it simplifies traditional block capture.

```rust
    use capture_it::capture;
    use std::sync::Arc;

    let arc = Arc::new(());
    let arc2 = arc.clone(); // let's just think these are all different variables
    let arc3 = arc.clone();
    let arc4 = arc.clone();

    let while_strong_count = |arc: &Arc<()>, pred_continue: fn(usize) -> bool| {
        while pred_continue(Arc::strong_count(arc)) {
            std::thread::yield_now();
        }
    };

    // Before, when you have to capture variables by copy ...
    std::thread::spawn({
        let arc = arc.clone();
        let arc2 = arc2.clone();
        let arc3 = arc3.clone();
        let arc4 = arc4.clone();

        move || {
            while_strong_count(&arc, |x| x >= 8);

            // we have to explicitly capture them.
            drop((arc2, arc3, arc4));
        }
    });

    // Then, we can write same logic with above, but in much more concise way
    std::thread::spawn(capture!([arc, arc2, arc3, arc4], move || {
        while_strong_count(&arc, |x| x >= 12);

        // `capture!` macro automatically captures all specified variables into closure,
        // thus, we don't need to explicitly capture them.
        // drop((arc2, arc3, arc4));
    }));

    assert!(Arc::strong_count(&arc) == 12);

    // as all variables are captured by clone, we can still owning `arc*` series
    drop((arc2, arc3, arc4));

    while_strong_count(&arc, |x| x > 1);
```

All variables other than those specified in the capture list follow the normal closure rules for rust, so if you need to take ownership of a variable, simply remove its name from the capture list.

```rust
    use capture_it::capture;
    use std::sync::Arc;

    let cloned = Arc::new(());
    let moved = cloned.clone();

    std::thread::spawn(capture!([cloned], move || {
        // Explicit 'move' capture
        drop(moved);
    }));

    // 'moved' was moved. So we cannot use it here.
    // drop(moved);

    while Arc::strong_count(&cloned) > 1 {
        std::thread::yield_now();
    }
```

Asynchronous blocks follow the same rules.

```rust
    use capture_it::capture;
    use futures::{SinkExt, StreamExt};

    let (tx, mut rx) = futures::channel::mpsc::unbounded::<usize>();

    let task1 = capture!([*tx], async move {
        // `move` is mandatory
        for val in 1..=3 {
            tx.send(val).await.unwrap();
        }
    });

    let task2 = capture!([*tx], async move {
        for val in 4..=6 {
            tx.send(val).await.unwrap();
        }
    });

    drop(tx); // we still have ownership of tx

    task2.await;
    task1.await;

    for val in (4..=6).chain(1..=3) {
        assert_eq!(rx.next().await.unwrap(), val);
    }
```

### Bonus

The `capture` macro contains several syntactic sugars. For example, if you want to capture the type
`&str` as the corresponding `ToOwned` type, `String`, you can apply the `Own(..)` decorator.

```rust
    use capture_it::capture;

    let hello = "hello, world!";
    let mut gen = capture!([*Own(hello), *times = 0], move || {
        times += 1;
        hello.push_str(&times.to_string());
        hello.clone()
    });

    assert_eq!(gen(), "hello, world!1");
    assert_eq!(gen(), "hello, world!12");
    assert_eq!(gen(), "hello, world!123");
```

The `Weak` decorator is used to capture a downgraded instance of `Arc` or `Rc`.

```rust
    use capture_it::capture;
    use std::rc::Rc;
    use std::sync::Arc;

    let rc = Rc::new(());
    let arc = Arc::new(());

    let closure = capture!([Weak(rc), Weak(arc)], move || {
        assert!(rc.upgrade().is_none());
        assert!(arc.upgrade().is_some());
    });

    drop(rc); // Let weak pointer upgrade of 'rc' fail
    closure();
```

The `Some` decorator is useful to mimic `FnOnce` in the `FnMut` function.

```rust
    use capture_it::capture;

    let initial_value = ();
    let mut increment = 0;

    let mut closure = capture!([*Some(initial_value), &mut increment], move || {
        if let Some(_) = initial_value.take() {
            // Evaluated only once, as we can take out `initial_value` only for single time...
            *increment = 100;
        } else {
            *increment += 1;
        }
    });

    closure();
    closure();
    closure();

    assert_eq!(increment, 102);
```

Any other function call with single argument can be used as a decorator. For example, the normal
clone representation of the `capture` macro is replaced by `Clone::clone(var)`.

```rust
    use capture_it::capture;

    let clone1 = std::sync::Arc::new(());
    let clone2 = clone1.clone();

    // following capture statement, `clone1` and `Clone::clone(&clone2)` behave equivalent.
    let closure = capture!([clone1, Clone::clone(&clone2)], move || {
        drop((clone1, clone2)); // Explicit drop will make this closure `FnOnce`
    });

    closure();
```

Alternatively, you can capture the return value of a function called on `self` as the name of its
variable. Function calls can contain parameters, but there are some restrictions; for example,
re-chaining to a function's return value will not work (`var.foo().bar()....`). Only one function
call is allowed.

Decorators are useful for capturing simple type changes; if you want to capture complex expressions, it's best to use the assignment syntax of `a=b`.

```rust
    use std::{rc::Rc, sync::Arc};

    use capture_it::capture;

    let arc = Arc::new(());
    let rc = Rc::new(());

    let weak_arc = Arc::downgrade(&arc);
    let weak_rc = Rc::downgrade(&rc);

    drop(arc);

    // The return value of `.upgrade()` will be captured as of its name.
    let closure = capture!([weak_arc.upgrade(), weak_rc.upgrade()], move || {
        assert!(weak_arc.is_none());
        assert!(weak_rc.is_some());
    });

    closure();
```

# Trivia

## Other closure crates use a more intuitive capture syntax...

For example, the `(move a, ref b, clone b, ...)` grammar in the [`closure`](https://crates.io/crates/closure) crate can express closure parameters more intuitively.

On the other hand, the `*` prefix to express the mutability of `capture-it` is unintuitive and hard to understand - why did we do it this way?

Introducing new grammars is a very tempting option, but by default, most of these attempts are poorly understood by the `rustfmt` utility.

Since closure macros typically pass the function body as a macro argument, a fairly long body can lose the benefit of the formatter if the `rustfmt` parser fails to parse the macro argument.

On the other hand, the `capture_it::capture` macro is a perfectly valid rust syntax (at least syntactically) that simply passes an array and a single function block as macro arguments. (Also, a capture list wrapped in [square brackets] can be used in a similar sense to C++.)

So any capture and function block you define in the `capture_it::capture` macro can be formatted by `rustfmt`, which is something I personally find quite important.
