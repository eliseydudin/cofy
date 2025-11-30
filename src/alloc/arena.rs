// TODO
// docs
// boxes/vectors/all other stuff like that

use super::Box;
use core::{cell, ptr, slice};
use std::{alloc, collections::LinkedList, sync::Mutex};

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
    /// Thread using this allocator has panicked while working with memory
    Poisoned,
}

unsafe impl Sync for Arena {}

impl From<alloc::LayoutError> for AllocError {
    fn from(value: alloc::LayoutError) -> Self {
        Self::Layout(value)
    }
}

struct Region {
    start: cell::OnceCell<ptr::NonNull<u8>>,
    allocated: usize,
    cap: usize,
}

pub struct Arena {
    block_size: usize,
    regions: Mutex<LinkedList<Region>>,
}

impl Arena {
    pub const fn new(block_size: usize) -> Self {
        Self {
            block_size,
            regions: Mutex::new(LinkedList::new()),
        }
    }

    pub fn try_alloc_raw(&self, layout: alloc::Layout) -> Result<ptr::NonNull<u8>, AllocError> {
        if self.block_size < layout.size() {
            return Err(AllocError::OutOfBounds);
        }

        let mut regions = self.regions.lock().map_err(|_| AllocError::Poisoned)?;

        for region in regions.iter_mut() {
            if region.can_alloc(layout) {
                return unsafe { region.alloc(layout) };
            }
        }

        // if the function has reached this line this means that
        // the requested amount of memory can fit into our block size
        // but we don't have a region that has enough unused memory for this block
        // so we need to make a new region
        regions.push_back(Region::new(self.block_size));
        unsafe {
            regions
                .back_mut()
                .expect("after regions.push_back regions should have at least one element")
                .alloc(layout)
        }
    }

    pub fn try_alloc<T: Copy>(&self, value: T) -> Result<&mut T, AllocError> {
        unsafe {
            let mut ptr = self.try_alloc_raw(alloc::Layout::new::<T>())?.cast::<T>();
            ptr.write(value);
            Ok(ptr.as_mut())
        }
    }

    pub fn alloc<T: Copy>(&self, value: T) -> &mut T {
        self.try_alloc(value)
            .expect("an error occured while allocating!")
    }

    pub fn alloc_boxed<'arena, T>(&'arena self, value: T) -> Box<'arena, T> {
        unsafe {
            let mut ptr = self
                .try_alloc_raw(alloc::Layout::new::<T>())
                .expect("an error occured while allocating!")
                .cast::<T>();

            ptr.write(value);
            Box::new(ptr.as_mut())
        }
    }

    pub unsafe fn try_alloc_slice_raw<'arena, T>(
        &'arena self,
        len: usize,
    ) -> Result<Box<'arena, [T]>, AllocError> {
        let layout = alloc::Layout::array::<T>(len)?;
        unsafe {
            let ptr = self.try_alloc_raw(layout)?.cast::<T>();
            Ok(Box::new(slice::from_raw_parts_mut(ptr.as_ptr(), len)))
        }
    }

    pub fn try_move_slice<'arena, T, const N: usize>(
        &'arena self,
        value: [T; N],
    ) -> Result<Box<'arena, [T]>, AllocError> {
        let new_slice = unsafe { self.try_alloc_slice_raw::<T>(N) };
        match new_slice {
            Ok(mut slice) => {
                for (i, value) in value.into_iter().enumerate() {
                    slice[i] = value
                }
                Ok(slice)
            }
            Err(e) => Err(e),
        }
    }

    pub fn move_slice<'arena, T, const N: usize>(&'arena self, value: [T; N]) -> Box<'arena, [T]> {
        self.try_move_slice(value)
            .expect("an error occured while allocating!")
    }

    pub fn try_clone_slice<'arena, T: Clone>(
        &'arena self,
        value: &[T],
    ) -> Result<Box<'arena, [T]>, AllocError> {
        let mut rawslice = unsafe { self.try_alloc_slice_raw::<T>(value.len())? };
        rawslice.clone_from_slice(value);
        Ok(rawslice)
    }

    pub fn clone_slice<'arena, T: Clone>(&'arena self, value: &[T]) -> Box<'arena, [T]> {
        self.try_clone_slice(value)
            .expect("an error occured while allocating!")
    }

    pub fn allocated(&self) -> usize {
        let regions = match self.regions.lock() {
            Ok(r) => r,
            Err(_) => return 0,
        };

        regions.iter().map(|r| r.allocated).sum()
    }

    /// Returns the amount of regions this arena has
    pub fn len(&self) -> usize {
        let regions = match self.regions.lock() {
            Ok(r) => r,
            Err(_) => return 0,
        };

        regions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Region {
    const fn new(block_size: usize) -> Self {
        Self {
            allocated: 0,
            cap: block_size,
            start: cell::OnceCell::new(),
        }
    }

    fn initialize_allocation(&self) -> Result<ptr::NonNull<u8>, AllocError> {
        if let Some(start) = self.start.get() {
            return Ok(*start);
        }

        let layout = alloc::Layout::array::<u8>(self.cap)?;
        unsafe { ptr::NonNull::new(alloc::alloc(layout)).ok_or(AllocError::OutOfGlobalMemory) }
    }

    unsafe fn alloc(&mut self, layout: alloc::Layout) -> Result<ptr::NonNull<u8>, AllocError> {
        if !self.can_alloc(layout) {
            if layout.size() < self.cap {
                // basically tell the arena that it should create a new block
                return Err(AllocError::OutOfMemory);
            } else {
                return Err(AllocError::OutOfBounds);
            }
        }

        let ptr = unsafe { self.initialize_allocation()?.add(self.allocated) };
        self.allocated += layout.size();
        Ok(ptr)
    }

    const fn can_alloc(&self, layout: alloc::Layout) -> bool {
        self.cap - self.allocated >= layout.size()
    }
}

impl Drop for Region {
    fn drop(&mut self) {
        if let Some(ptr) = self.start.get() {
            let layout = alloc::Layout::array::<u8>(self.cap).expect(
                "since the pointer has already been allocated this operation should not fail",
            );
            unsafe { alloc::dealloc(ptr.as_ptr(), layout) }
        }
    }
}
