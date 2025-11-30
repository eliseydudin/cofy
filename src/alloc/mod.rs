mod arena;
mod boxed;

pub use arena::{AllocError, Arena};
pub use boxed::Box;

#[cfg(test)]
mod test;
