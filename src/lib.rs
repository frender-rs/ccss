#![no_std]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]

extern crate core as std;

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod input;

pub mod token;

pub mod parse;

pub mod const_known;

pub mod collections;
