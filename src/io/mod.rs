use crate::error::Result;
use crate::Error;
use std::io::{Read};

/// An iterator over some input.
/// The iterator transforms the input into unicode characters, assuming the input is encoded in UTF-8.
pub struct ReadIterator<Source> {
    source: Source,
}

impl<Source> ReadIterator<Source> {
    /// Construct a new instance over the given source.
    pub fn new(source: Source) -> Self {
        Self { source }
    }
}

impl<Source: Read> Iterator for ReadIterator<Source> {
    type Item = Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut byte_buffer = [0];
        let mut char_buffer = 0;
        let mut char_buffer_length = 0;

        while char_buffer_length < 4 {
            match self.source.read(&mut byte_buffer) {
                Err(error) => return Some(Err(Error::ReadError(error))),
                Ok(0) => return None, // if no bytes were read, we reached EOF
                Ok(1) => {}
                Ok(_) => unreachable!("Buffer has length one"),
            }
            char_buffer <<= 8;
            char_buffer |= u32::from(byte_buffer[0]);
            char_buffer_length += 1;

            if let Some(character) = char::from_u32(char_buffer) {
                return Some(Ok(character));
            }
        }

        Some(Err(Error::InvalidUtf8Character {invalid: char_buffer}))
    }
}
