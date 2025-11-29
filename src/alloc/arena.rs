// TODO
// docs
// boxes/vectors/all other stuff like that
// drop implementation for arena and regions

use core::{cell, ptr};
use std::alloc;

#[derive(Debug, PartialEq)]
pub enum AllocError {
    /// The amount of memory to be allocated exceeds the max block size
    OutOfBounds,
    /// The current block of memory doesn't have enough memory to allocate the
    /// requested amount, if you see this error something went really wrong
    OutOfMemory,
    /// The global allocator cannot allocate the requested amount
    OutOfGlobalMemory,
    Layout(alloc::LayoutError),
}

impl From<alloc::LayoutError> for AllocError {
    fn from(value: alloc::LayoutError) -> Self {
        Self::Layout(value)
    }
}

struct Region {
    next: cell::OnceCell<ptr::NonNull<Self>>,
    start: cell::OnceCell<ptr::NonNull<u8>>,
    allocated: cell::Cell<usize>,
    cap: usize,
}

pub struct Arena {
    block_size: usize,
    head: cell::OnceCell<ptr::NonNull<Region>>,
}

impl Arena {
    pub const fn new(block_size: usize) -> Self {
        Self {
            block_size,
            head: cell::OnceCell::new(),
        }
    }

    fn init_head(&self) -> Result<ptr::NonNull<Region>, AllocError> {
        match self.head.get() {
            Some(head) => Ok(*head),
            None => {
                let value =
                    unsafe { Region::new(self.block_size) }.ok_or(AllocError::OutOfGlobalMemory)?;
                self.head
                    .set(value)
                    .expect("it is verified that `self.head` had no value before this action");
                Ok(value)
            }
        }
    }

    fn get_tail(&self) -> Result<&Region, AllocError> {
        self.init_head()
            .map(|head| unsafe { Region::tail(head).as_ref() })
    }

    pub unsafe fn try_alloc_raw(
        &self,
        layout: alloc::Layout,
    ) -> Result<ptr::NonNull<u8>, AllocError> {
        let tail = self.get_tail()?;
        let new_alloc = unsafe { tail.alloc(layout) };

        match new_alloc {
            Ok(value) => Ok(value),
            Err(e) => {
                if e == AllocError::OutOfMemory {
                    // the new block is guaranteed to have enough memory
                    // for this allocation so it should not fail
                    let new_block = unsafe { tail.new_child()?.as_ref() };
                    unsafe { new_block.alloc(layout) }
                } else {
                    Err(e)
                }
            }
        }
    }

    pub fn try_alloc<'arena, T: Copy>(&'arena self, value: T) -> Result<&'arena mut T, AllocError> {
        unsafe {
            let mut ptr = self.try_alloc_raw(alloc::Layout::new::<T>())?.cast::<T>();
            ptr.write(value);
            Ok(ptr.as_mut())
        }
    }

    pub fn alloc<'arena, T: Copy>(&'arena self, value: T) -> &'arena mut T {
        self.try_alloc(value)
            .expect("an error occured while allocating!")
    }
}

impl Region {
    const fn from_block_size(block_size: usize) -> Self {
        Self {
            allocated: cell::Cell::new(0),
            cap: block_size,
            next: cell::OnceCell::new(),
            start: cell::OnceCell::new(),
        }
    }

    unsafe fn new(block_size: usize) -> Option<ptr::NonNull<Self>> {
        assert!(block_size > 0);
        let layout = alloc::Layout::new::<Self>();
        let bytes = unsafe { ptr::NonNull::new(alloc::alloc(layout)) };

        match bytes {
            None => None,
            Some(ptr) => {
                let new = ptr.cast::<Self>();
                unsafe { new.write(Self::from_block_size(block_size)) };
                Some(new)
            }
        }
    }

    fn initialize_allocation(&self) -> Result<ptr::NonNull<u8>, AllocError> {
        if let Some(start) = self.start.get() {
            return Ok(*start);
        }

        let layout = alloc::Layout::array::<u8>(self.cap)?;
        unsafe { ptr::NonNull::new(alloc::alloc(layout)).ok_or(AllocError::OutOfGlobalMemory) }
    }

    unsafe fn alloc(&self, layout: alloc::Layout) -> Result<ptr::NonNull<u8>, AllocError> {
        if layout.size() > self.cap - self.allocated.get() {
            if layout.size() < self.cap {
                // basically tell the arena that it should create a new block
                return Err(AllocError::OutOfMemory);
            } else {
                return Err(AllocError::OutOfBounds);
            }
        }

        let ptr = unsafe { self.initialize_allocation()?.add(self.allocated.get()) };
        self.allocated.set(self.allocated.get() + layout.size());
        Ok(ptr)
    }

    fn tail(mut pointer: ptr::NonNull<Self>) -> ptr::NonNull<Self> {
        while let Some(reg) = unsafe { pointer.as_ref().next.get() } {
            pointer = *reg;
        }
        pointer
    }

    fn new_child(&self) -> Result<ptr::NonNull<Self>, AllocError> {
        assert!(
            self.next.get().is_none(),
            "`new_child` should only be called on the tail of the block list"
        );

        let new_ptr = unsafe { Self::new(self.cap).ok_or(AllocError::OutOfGlobalMemory)? };
        self.next.set(new_ptr).expect(
            "the pointer return by `Region::tail` should always\
            have an uninitialized `next`",
        );

        Ok(new_ptr)
    }
}
