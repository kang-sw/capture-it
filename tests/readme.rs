#[cfg(test)]
#[test]
fn example_generator() {
    use capture_it::capture as closure;

    let mut gen = closure!([*index = 0], move || {
        index += 1;
        index
    });

    assert!((gen(), gen(), gen(), gen(), gen()) == (1, 2, 3, 4, 5));
}

#[cfg(test)]
#[test]
fn example_ref() {
    use capture_it::capture;
    let mut num = 0;
    let mut gen = capture!([&mut num], move || {
        *num += 1;
        *num
    });

    assert!((gen(), gen(), gen(), gen(), gen()) == (1, 2, 3, 4, 5));
}

#[cfg(test)]
#[test]
fn example_ptr() {
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
}

#[cfg(test)]
#[test]
fn example_many() {
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
}

#[cfg(test)]
#[test]
fn example_move_clone() {
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
}

#[cfg(test)]
async fn example_async_impl() {
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
}

#[cfg(test)]
#[test]
fn example_async() {
    futures::executor::block_on(example_async_impl());
}

#[cfg(test)]
#[test]
fn example_tuple_param() {
    use capture_it::capture;

    assert_eq!(
        capture!([*index = 3], move |(x, y)| { x + y + index })((1, 2)),
        6
    );
}

#[cfg(test)]
#[test]
fn example_decorate() {
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
}

#[cfg(test)]
#[test]
fn example_weak() {
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
}

#[cfg(test)]
#[test]
fn example_some() {
    use capture_it::capture;

    let initial_value = 0;
    let mut increment = 0;
    let mut closure = capture!([*Some(initial_value), &mut increment], move || {
        if initial_value.take().is_some() {
            *increment = 100;
        } else {
            *increment += 1;
        }
    });

    closure();
    closure();
    closure();

    assert_eq!(increment, 102);
}

#[cfg(test)]
#[test]
fn example_deco_misc() {
    use capture_it::capture;

    let clone1 = std::sync::Arc::new(());
    let clone2 = clone1.clone();

    // following capture statement, `clone1` and `Clone::clone(&clone2)` behave equivalent.
    let closure = capture!([clone1, Clone::clone(&clone2)], move || {
        drop((clone1, clone2)); // Explicit drop will make this closure `FnOnce`
    });

    closure();
}

#[cfg(test)]
#[test]
fn example_method() {
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
}
