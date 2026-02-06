//! Hedgehog Channels
//!
//! Buffered channels for concurrent communication.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use super::value::Value;

/// A buffered channel for passing values
#[derive(Debug, Clone)]
pub struct Channel {
    buffer: Rc<RefCell<VecDeque<Value>>>,
    closed: Rc<RefCell<bool>>,
}

impl Channel {
    /// Create a new unbounded channel
    pub fn new() -> Self {
        Self {
            buffer: Rc::new(RefCell::new(VecDeque::new())),
            closed: Rc::new(RefCell::new(false)),
        }
    }

    /// Send a value to the channel
    /// Returns Ok(()) on success, Err if channel is closed
    pub fn send(&self, value: Value) -> Result<(), &'static str> {
        if *self.closed.borrow() {
            return Err("send on closed channel");
        }
        self.buffer.borrow_mut().push_back(value);
        Ok(())
    }

    /// Try to receive a value from the channel
    /// Returns Some(value) if available, None if empty
    pub fn try_recv(&self) -> Option<Value> {
        self.buffer.borrow_mut().pop_front()
    }

    /// Receive a value from the channel
    /// Returns Ok(value) if available, Err if empty or closed
    pub fn recv(&self) -> Result<Value, &'static str> {
        if let Some(value) = self.try_recv() {
            Ok(value)
        } else if *self.closed.borrow() {
            Err("receive on closed channel")
        } else {
            Err("channel is empty")
        }
    }

    /// Check if the channel has pending values
    pub fn is_empty(&self) -> bool {
        self.buffer.borrow().is_empty()
    }

    /// Get the number of pending values
    pub fn len(&self) -> usize {
        self.buffer.borrow().len()
    }

    /// Close the channel
    pub fn close(&self) {
        *self.closed.borrow_mut() = true;
    }

    /// Check if the channel is closed
    pub fn is_closed(&self) -> bool {
        *self.closed.borrow()
    }
}

impl Default for Channel {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Channel {
    fn eq(&self, other: &Self) -> bool {
        // Channels are equal if they share the same buffer
        Rc::ptr_eq(&self.buffer, &other.buffer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_channel_send_recv() {
        let ch = Channel::new();
        ch.send(Value::Int(42)).unwrap();
        assert_eq!(ch.recv().unwrap(), Value::Int(42));
    }

    #[test]
    fn test_channel_multiple_values() {
        let ch = Channel::new();
        ch.send(Value::Int(1)).unwrap();
        ch.send(Value::Int(2)).unwrap();
        ch.send(Value::Int(3)).unwrap();

        assert_eq!(ch.recv().unwrap(), Value::Int(1));
        assert_eq!(ch.recv().unwrap(), Value::Int(2));
        assert_eq!(ch.recv().unwrap(), Value::Int(3));
    }

    #[test]
    fn test_channel_empty() {
        let ch = Channel::new();
        assert!(ch.is_empty());
        assert!(ch.recv().is_err());
    }

    #[test]
    fn test_channel_try_recv() {
        let ch = Channel::new();
        assert_eq!(ch.try_recv(), None);

        ch.send(Value::Int(42)).unwrap();
        assert_eq!(ch.try_recv(), Some(Value::Int(42)));
        assert_eq!(ch.try_recv(), None);
    }

    #[test]
    fn test_channel_close() {
        let ch = Channel::new();
        ch.send(Value::Int(1)).unwrap();
        ch.close();

        // Can still receive buffered values
        assert_eq!(ch.recv().unwrap(), Value::Int(1));

        // But send fails
        assert!(ch.send(Value::Int(2)).is_err());
    }

    #[test]
    fn test_channel_clone_shares_buffer() {
        let ch1 = Channel::new();
        let ch2 = ch1.clone();

        ch1.send(Value::Int(42)).unwrap();
        assert_eq!(ch2.recv().unwrap(), Value::Int(42));
    }

    #[test]
    fn test_channel_equality() {
        let ch1 = Channel::new();
        let ch2 = ch1.clone();
        let ch3 = Channel::new();

        assert_eq!(ch1, ch2);
        assert_ne!(ch1, ch3);
    }
}
