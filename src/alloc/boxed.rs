use core::{borrow, mem, ops, ptr};

pub struct Box<'a, T: ?Sized>(&'a mut T);

impl<'a, T: ?Sized> Box<'a, T> {
    pub(super) fn new(value: &'a mut T) -> Self {
        Self(value)
    }

    pub const fn as_mut_ptr(&mut self) -> *mut T {
        self.0 as *mut T
    }

    pub const fn as_ptr(&self) -> *const T {
        self.0 as *const T
    }

    pub const fn as_arena_ref(&self) -> &'a T {
        unsafe { mem::transmute(&*self.0) }
    }

    pub const fn as_arena_mut(&mut self) -> &'a mut T {
        unsafe { mem::transmute(&mut *self.0) }
    }
}

impl<'a, T: Sized> Box<'a, T> {
    pub fn into_inner(self) -> T {
        unsafe { ptr::read(self.as_ptr()) }
    }
}

impl<T: ?Sized> Drop for Box<'_, T> {
    fn drop(&mut self) {
        unsafe { ptr::drop_in_place(self.as_mut_ptr()) }
    }
}

impl<T: ?Sized> ops::Deref for Box<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<T: ?Sized> ops::DerefMut for Box<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<T: ?Sized> borrow::Borrow<T> for Box<'_, T> {
    fn borrow(&self) -> &T {
        self.0
    }
}

impl<T: ?Sized> borrow::BorrowMut<T> for Box<'_, T> {
    fn borrow_mut(&mut self) -> &mut T {
        self.0
    }
}

impl<T: ?Sized> AsRef<T> for Box<'_, T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<T: ?Sized> AsMut<T> for Box<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        self.0
    }
}
