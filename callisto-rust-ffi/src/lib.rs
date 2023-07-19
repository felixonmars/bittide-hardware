#![feature(slice_ptr_get)]
use hs_bindgen::{traits::ReprRust, traits::ReprC, *};
use std::mem;
use std::fmt;

#[macro_use]
extern crate static_assertions;

#[repr(u32)]
enum SpeedChange
  { SpeedUp  = 0
  , SlowDown = 1
  , NoChange = 2
  }

const_assert!(
  4 ==
    mem::size_of::<SpeedChange>()
);

const_assert!(
  4 ==
    mem::align_of::<SpeedChange>()
);

impl fmt::Display for SpeedChange {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self
    { SpeedChange::SpeedUp  => write!(f, "SpeedUp")
    , SpeedChange::SlowDown => write!(f, "SlowDown")
    , SpeedChange::NoChange => write!(f, "NoChange")
    }
  }
}

impl ReprRust<u32> for SpeedChange
{
  fn from(v: u32) -> Self
    {
      match v
        { 0 => SpeedChange::SpeedUp
        , 1 => SpeedChange::SlowDown
        , 2 => SpeedChange::NoChange
        , _ => panic!("out of range")
        }
    }
}

impl ReprC<SpeedChange> for u32
{
  fn from(v: SpeedChange) -> Self
    {
      match v
        { SpeedChange::SpeedUp  => 0
        , SpeedChange::SlowDown => 1
        , SpeedChange::NoChange => 2
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
enum ReframingState
  { Detect
  , Done
  , Wait
     { target_correction: f32
     , cur_wait_time:     u32
     }
  }

// assert that things appear as expected with respect to the memory
// layout of ReframingState to ensure that values are consistently
// encoding at the haskell side

const_assert!(
  12 ==
    mem::size_of::<ReframingState>()
);

const_assert!(
  4 ==
    mem::align_of::<ReframingState>()
);

const_assert!(
  0u32 ==
    { let v = ReframingState::Detect
    ; unsafe {*(&v as *const _ as *const u32)}
    }
);

const_assert!(
  1u32 ==
    { let v = ReframingState::Done
    ; unsafe {*(&v as *const _ as *const u32)}
    }
);

#[allow(dead_code)]
const SOME_RF_STATE_WAIT: ReframingState =
  ReframingState::Wait
    { target_correction: 3.321
    , cur_wait_time: 12345
    };

#[allow(dead_code)]
const CASTED_RF_STATE_WAIT: *const (u32, f32, u32) =
  &SOME_RF_STATE_WAIT as *const _ as *const (u32, f32, u32);

const_assert!(
  2u32 ==
    { let (x,_,_) = unsafe {*CASTED_RF_STATE_WAIT}; x }
);

const_assert!(
  3.321f32 ==
    { let (_,x,_) = unsafe {*CASTED_RF_STATE_WAIT}; x }
);

const_assert!(
  12345u32 ==
    { let (_,_,x) = unsafe {*CASTED_RF_STATE_WAIT}; x }
);

// This implementation produces the same output as the corresponding
// Haskell Show instance.
impl fmt::Display for ReframingState {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self
    { ReframingState::Detect => write!(f, "Detect")
    , ReframingState::Done => write!(f, "Done")
    , ReframingState::Wait{target_correction, cur_wait_time} => write!
      ( f
      , "Wait {{targetCorrection = {}, curWaitTime = {}}}"
      , target_correction
      , cur_wait_time
      )
    }
  }
}

impl ReprRust<*const ()> for &mut ReframingState {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

#[repr(C)]
struct ControlSt
  { z_k: i32
  , b_k: SpeedChange
  , steady_state_target: f32
  , rf_state: ReframingState
  }

const_assert!(
  24 ==
    mem::size_of::<ControlSt>()
);

const_assert!(
  4 ==
    mem::align_of::<ControlSt>()
);

#[allow(dead_code)]
const SOME_CONTROL_STATE: ControlSt =
  ControlSt
    { z_k: 54321
    , b_k: SpeedChange::SlowDown
    , steady_state_target: 3.185
    , rf_state: SOME_RF_STATE_WAIT
    };

#[allow(dead_code)]
const CASTED_CONTROL_STATE: *const (i32, u32, f32, u32, f32, u32) =
  &SOME_CONTROL_STATE as *const _ as *const (i32, u32, f32, u32, f32, u32);

const_assert!(
  54321i32 ==
    { let (x,_,_,_,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  1u32 ==
    { let (_,x,_,_,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  3.185f32 ==
    { let (_,_,x,_,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  2u32 ==
    { let (_,_,_,x,_,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  3.321f32 ==
    { let (_,_,_,_,x,_) = unsafe {*CASTED_CONTROL_STATE}; x }
);

const_assert!(
  12345u32 ==
    { let (_,_,_,_,_,x) = unsafe {*CASTED_CONTROL_STATE}; x }
);

// This implementation produces the same output as the corresponding
// Haskell Show instance.
impl fmt::Display for ControlSt {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!
      ( f
      , "ControlSt {{\
         _z_k = {}, \
         _b_k = {}, \
         _steadyStateTarget = {}, \
         rfState = {}\
         }}"
      , self.z_k
      , self.b_k
      , self.steady_state_target
      , self.rf_state
      )
  }
}

impl ReprRust<*const ()> for &mut ControlSt {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

impl ReprRust<*const ()> for &ControlSt {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

#[derive(PartialEq, PartialOrd)]
struct StabilityIndication(usize);

impl StabilityIndication {
  fn stable(&self) -> bool {
    return self.0 & 1 == 1;
  }
  fn settled(&self) -> bool {
    return self.0 & 2 == 2;
  }
}

impl fmt::Display for StabilityIndication {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!
      ( f
      , "StabilityIndication {{\
         stable = {}, \
         settled = {}\
         }}"
      , if self.stable()  { "True" } else { "False" }
      , if self.settled() { "True" } else { "False" }
      )
  }
}

struct VSI(Vec<StabilityIndication>);

impl ReprRust<*const ()> for &VSI {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

impl fmt::Display for VSI {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Err(e) = write!(f, "[ ") {
      return Err(e);
    }
    for i in 0..self.0.len()-1 {
      if let Err(e) = write!(f, "{}, ", self.0[i]) {
        return Err(e);
      }
    }
    if let Err(e) = write!(f, "{}", self.0[self.0.len()-1]) {
      return Err(e);
    }
    return write!(f, " ]");
  }
}

struct DataCounts(Vec<isize>);

impl ReprRust<*const ()> for &DataCounts {
  fn from(ptr: *const ()) -> Self {
    return unsafe { mem::transmute(ptr) };
  }
}

impl fmt::Display for DataCounts {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if let Err(e) = write!(f, "[ ") {
      return Err(e);
    }
    for i in 0..self.0.len()-1 {
      if let Err(e) = write!(f, "{}, ", self.0[i]) {
        return Err(e);
      }
    }
    if let Err(e) = write!(f, "{}", self.0[self.0.len()-1]) {
      return Err(e);
    }
    return write!(f, " ]");
  }
}

#[hs_bindgen(
    callisto_rust ::
      CUInt  -> // reframing_enabled
      CUInt  -> // wait_time
      Ptr () -> // stability_checks
      CUInt  -> // availability_mask
      Ptr () -> // data_counts
      Ptr () -> // control_state
      IO ()
  )
]
fn callisto
  ( reframing_enabled: u32
  , wait_time:         u32
  , stability_checks:  &VSI
  , availability_mask: u32
  , data_counts:       &DataCounts
  , control_state:     &mut ControlSt
  ) -> () {
  print!
    ( "{}\n{}\n{}\n{:0b}\n{}\n{}"
    , reframing_enabled
    , wait_time
    , stability_checks
    , availability_mask
    , data_counts
    , control_state
    );
}
