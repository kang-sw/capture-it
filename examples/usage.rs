use std::sync::atomic::Ordering::Relaxed;
use std::sync::{atomic::AtomicUsize, Arc};
use std::time::Duration;

use capture_it::capture;

fn main() {
    let va = Arc::new(AtomicUsize::new(0));

    // This expr makes a closure that captures `va` by copy
    std::thread::spawn(capture!([va], || {
        va.fetch_add(1, Relaxed);
    }));

    // We can create new variable with expression, same as c++.
    std::thread::spawn(capture!([va_other = va.clone()], || {
        va_other.fetch_add(1, Relaxed);
    }));

    // Asterisk-prefixed variable will be declared as mutable.
    std::thread::spawn(capture!([*va], || {
        va = Arc::new(AtomicUsize::new(0));
        va.fetch_add(1, Relaxed);
    }));

    // This expr makes a closure that captures `va` by reference
    while capture!([&va], || -> bool {
        if va.load(Relaxed) == 2 {
            // the last thread does not increment same reference ...
            false
        } else {
            std::thread::sleep(Duration::from_millis(10));
            true
        }
    })() {}

    println!("va = {}", va.load(Relaxed));
    assert_eq!(va.load(Relaxed), 2);

    // This expr makes a closure that captures `va` by move
    let _ = capture!([], || { va });

    // Therefore, we can't access `va` anymore
    // let _ = capture!([], || { va }); // this line will cause compile error!
}
