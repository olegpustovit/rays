//! The safe side of shim.c: a D-Bus connection and messages with a write/read cursor. Public so tests (and a
//! host that wants a second connection) speak D-Bus through the same code as the bridge.
use std::ffi::{CStr, CString, c_char, c_int, c_void};
use std::ptr::NonNull;

#[repr(C)]
struct RawConn(c_void);
#[repr(C)]
struct RawMsg(c_void);

unsafe extern "C" {
    fn ax_open(address: *const c_char, err: *mut c_char, errlen: usize) -> *mut RawConn;
    fn ax_close(c: *mut RawConn);
    fn ax_fd(c: *mut RawConn) -> c_int;
    fn ax_unique_name(c: *mut RawConn) -> *const c_char;
    fn ax_add_match(c: *mut RawConn, rule: *const c_char, err: *mut c_char, errlen: usize) -> c_int;
    fn ax_new_call(dest: *const c_char, path: *const c_char, iface: *const c_char, member: *const c_char) -> *mut RawMsg;
    fn ax_new_signal(path: *const c_char, iface: *const c_char, member: *const c_char) -> *mut RawMsg;
    fn ax_new_reply(call: *mut RawMsg) -> *mut RawMsg;
    fn ax_new_error(call: *mut RawMsg, name: *const c_char, text: *const c_char) -> *mut RawMsg;
    fn ax_free(m: *mut RawMsg);
    fn ax_put(m: *mut RawMsg, kind: c_int, p: *const c_void, len: usize);
    fn ax_open_container(m: *mut RawMsg, kind: c_int, sig: *const c_char);
    fn ax_close_container(m: *mut RawMsg);
    fn ax_send(c: *mut RawConn, m: *mut RawMsg) -> u32;
    fn ax_flush(c: *mut RawConn);
    fn ax_call_block(c: *mut RawConn, m: *mut RawMsg, timeout_ms: c_int, err: *mut c_char, errlen: usize) -> *mut RawMsg;
    fn ax_pop(c: *mut RawConn, connected: *mut c_int) -> *mut RawMsg;
    fn ax_wait(c: *mut RawConn, timeout_ms: c_int, connected: *mut c_int) -> *mut RawMsg;
    fn ax_kind(m: *mut RawMsg) -> c_int;
    fn ax_path(m: *mut RawMsg) -> *const c_char;
    fn ax_interface(m: *mut RawMsg) -> *const c_char;
    fn ax_member(m: *mut RawMsg) -> *const c_char;
    fn ax_error_name(m: *mut RawMsg) -> *const c_char;
    fn ax_signature(m: *mut RawMsg) -> *const c_char;
    fn ax_reply_serial(m: *mut RawMsg) -> u32;
    fn ax_failed(m: *mut RawMsg) -> c_int;
    fn ax_arg_type(m: *mut RawMsg) -> c_int;
    fn ax_get(m: *mut RawMsg, out: *mut c_void);
    fn ax_next(m: *mut RawMsg) -> c_int;
    fn ax_recurse(m: *mut RawMsg) -> c_int;
    fn ax_up(m: *mut RawMsg);
}

/// D-Bus message types.
pub const METHOD_CALL: i32 = 1;
pub const METHOD_RETURN: i32 = 2;
pub const ERROR: i32 = 3;
pub const SIGNAL: i32 = 4;

fn cstr(text: &str) -> CString {
    CString::new(text).unwrap_or_default()
}

fn error_text(buf: &[u8]) -> String {
    CStr::from_bytes_until_nul(buf).map(|s| s.to_string_lossy().into_owned()).unwrap_or_default()
}

/// A private connection to one bus.
pub struct Conn(NonNull<RawConn>);

impl Conn {
    /// Connect to the bus at `address` (`None`: the session bus) and register on it.
    pub fn open(address: Option<&str>) -> Result<Conn, String> {
        let address = address.map(cstr);
        let mut err = [0u8; 256];
        let raw = unsafe { ax_open(address.as_ref().map_or(std::ptr::null(), |a| a.as_ptr()), err.as_mut_ptr().cast(), err.len()) };
        NonNull::new(raw).map(Conn).ok_or_else(|| error_text(&err))
    }

    pub fn fd(&self) -> i32 {
        unsafe { ax_fd(self.0.as_ptr()) }
    }

    pub fn unique_name(&self) -> String {
        let p = unsafe { ax_unique_name(self.0.as_ptr()) };
        if p.is_null() { String::new() } else { unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned() }
    }

    pub fn add_match(&self, rule: &str) -> Result<(), String> {
        let rule = cstr(rule);
        let mut err = [0u8; 256];
        let ok = unsafe { ax_add_match(self.0.as_ptr(), rule.as_ptr(), err.as_mut_ptr().cast(), err.len()) };
        if ok != 0 { Ok(()) } else { Err(error_text(&err)) }
    }

    /// Queue `msg`; its serial, or `None` when it was malformed (e.g. invalid UTF-8) and was dropped.
    pub fn send(&self, msg: Msg) -> Option<u32> {
        let raw = msg.0.as_ptr();
        std::mem::forget(msg);
        let serial = unsafe { ax_send(self.0.as_ptr(), raw) };
        (serial != 0).then_some(serial)
    }

    pub fn flush(&self) {
        unsafe { ax_flush(self.0.as_ptr()) }
    }

    /// Send `call` and wait for its reply (an error reply is an `Err` with its message).
    pub fn call(&self, call: Msg, timeout_ms: i32) -> Result<Msg, String> {
        let raw = call.0.as_ptr();
        std::mem::forget(call);
        let mut err = [0u8; 256];
        let reply = unsafe { ax_call_block(self.0.as_ptr(), raw, timeout_ms, err.as_mut_ptr().cast(), err.len()) };
        NonNull::new(reply).map(Msg).ok_or_else(|| error_text(&err))
    }

    /// The next incoming message without blocking; `Err` once disconnected.
    pub fn pop(&self) -> Result<Option<Msg>, ()> {
        let mut connected = 0;
        let m = NonNull::new(unsafe { ax_pop(self.0.as_ptr(), &mut connected) }).map(Msg);
        if m.is_none() && connected == 0 { Err(()) } else { Ok(m) }
    }

    /// The next incoming message, waiting up to `timeout_ms`.
    pub fn wait(&self, timeout_ms: i32) -> Result<Option<Msg>, ()> {
        let mut connected = 0;
        let m = NonNull::new(unsafe { ax_wait(self.0.as_ptr(), timeout_ms, &mut connected) }).map(Msg);
        if m.is_none() && connected == 0 { Err(()) } else { Ok(m) }
    }
}

impl Drop for Conn {
    fn drop(&mut self) {
        unsafe { ax_close(self.0.as_ptr()) }
    }
}

/// A message being written (new calls, signals, replies) or read (incoming, replies), with one cursor.
pub struct Msg(NonNull<RawMsg>);

fn made(raw: *mut RawMsg) -> Msg {
    Msg(NonNull::new(raw).expect("libdbus: out of memory"))
}

fn text<'a>(p: *const c_char) -> &'a str {
    if p.is_null() { "" } else { unsafe { CStr::from_ptr(p) }.to_str().unwrap_or("") }
}

impl Msg {
    pub fn call(dest: &str, path: &str, iface: &str, member: &str) -> Msg {
        let (d, p, i, m) = (cstr(dest), cstr(path), cstr(iface), cstr(member));
        made(unsafe { ax_new_call(d.as_ptr(), p.as_ptr(), i.as_ptr(), m.as_ptr()) })
    }

    pub fn signal(path: &str, iface: &str, member: &str) -> Msg {
        let (p, i, m) = (cstr(path), cstr(iface), cstr(member));
        made(unsafe { ax_new_signal(p.as_ptr(), i.as_ptr(), m.as_ptr()) })
    }

    pub fn reply(&self) -> Msg {
        made(unsafe { ax_new_reply(self.0.as_ptr()) })
    }

    pub fn error(&self, name: &str, message: &str) -> Msg {
        let (n, m) = (cstr(name), cstr(message));
        made(unsafe { ax_new_error(self.0.as_ptr(), n.as_ptr(), m.as_ptr()) })
    }

    /// A fixed-size basic value at `v` (its type matches `kind`).
    fn put(&mut self, kind: u8, v: *const c_void) -> &mut Msg {
        unsafe { ax_put(self.0.as_ptr(), kind as c_int, v, 0) };
        self
    }
    fn put_text(&mut self, kind: u8, v: &str) -> &mut Msg {
        unsafe { ax_put(self.0.as_ptr(), kind as c_int, v.as_ptr().cast(), v.len()) };
        self
    }
    pub fn s(&mut self, v: &str) -> &mut Msg { self.put_text(b's', v) }
    pub fn o(&mut self, v: &str) -> &mut Msg { self.put_text(b'o', v) }
    pub fn u(&mut self, v: u32) -> &mut Msg { self.put(b'u', (&v as *const u32).cast()) }
    pub fn i(&mut self, v: i32) -> &mut Msg { self.put(b'i', (&v as *const i32).cast()) }
    pub fn n(&mut self, v: i16) -> &mut Msg { self.put(b'n', (&v as *const i16).cast()) }
    pub fn d(&mut self, v: f64) -> &mut Msg { self.put(b'd', (&v as *const f64).cast()) }
    pub fn b(&mut self, v: bool) -> &mut Msg { self.put(b'b', (&u32::from(v) as *const u32).cast()) }
    /// Open a container: `b'a'` with the element signature, `b'v'` with the contents' signature, `b'r'`
    /// (struct) or `b'e'` (dict entry) with `""`.
    pub fn open(&mut self, kind: u8, sig: &str) -> &mut Msg {
        let s = cstr(sig);
        unsafe { ax_open_container(self.0.as_ptr(), kind as c_int, if sig.is_empty() { std::ptr::null() } else { s.as_ptr() }) };
        self
    }
    pub fn close(&mut self) -> &mut Msg {
        unsafe { ax_close_container(self.0.as_ptr()) };
        self
    }
    /// A write failed (invalid text, unbalanced containers): sending drops the message.
    pub fn failed(&self) -> bool {
        unsafe { ax_failed(self.0.as_ptr()) != 0 }
    }

    pub fn kind(&self) -> i32 { unsafe { ax_kind(self.0.as_ptr()) } }
    pub fn path(&self) -> &str { text(unsafe { ax_path(self.0.as_ptr()) }) }
    pub fn interface(&self) -> &str { text(unsafe { ax_interface(self.0.as_ptr()) }) }
    pub fn member(&self) -> &str { text(unsafe { ax_member(self.0.as_ptr()) }) }
    pub fn error_name(&self) -> &str { text(unsafe { ax_error_name(self.0.as_ptr()) }) }
    pub fn signature(&self) -> &str { text(unsafe { ax_signature(self.0.as_ptr()) }) }
    pub fn reply_serial(&self) -> u32 { unsafe { ax_reply_serial(self.0.as_ptr()) } }

    /// The current argument's type code (0 past the end).
    pub fn arg(&self) -> u8 {
        unsafe { ax_arg_type(self.0.as_ptr()) as u8 }
    }
    /// A fixed-size basic argument of type `kind` as 8 native-endian bytes (libdbus's DBusBasicValue), and advance.
    fn get(&mut self, kind: u8) -> Option<[u8; 8]> {
        if self.arg() != kind {
            return None;
        }
        let mut v = [0u8; 8];
        unsafe {
            ax_get(self.0.as_ptr(), v.as_mut_ptr().cast());
            ax_next(self.0.as_ptr());
        }
        Some(v)
    }
    /// Read a string or object path argument and advance; `None` on a type mismatch.
    pub fn read_str(&mut self) -> Option<&str> {
        let kind = self.arg();
        if kind != b's' && kind != b'o' {
            return None;
        }
        let mut p: *const c_char = std::ptr::null();
        unsafe {
            ax_get(self.0.as_ptr(), (&mut p as *mut *const c_char).cast());
            ax_next(self.0.as_ptr());
        }
        Some(text(p))
    }
    pub fn read_u32(&mut self) -> Option<u32> { self.get(b'u').map(|v| u32::from_ne_bytes([v[0], v[1], v[2], v[3]])) }
    pub fn read_i32(&mut self) -> Option<i32> { self.get(b'i').map(|v| i32::from_ne_bytes([v[0], v[1], v[2], v[3]])) }
    pub fn read_f64(&mut self) -> Option<f64> { self.get(b'd').map(f64::from_ne_bytes) }
    pub fn read_bool(&mut self) -> Option<bool> { self.get(b'b').map(|v| v[..4] != [0; 4]) }
    /// Step into the current container (array, struct, variant, dict entry); `false` if it is none.
    pub fn enter(&mut self) -> bool {
        unsafe { ax_recurse(self.0.as_ptr()) != 0 }
    }
    /// Leave the container and step past it.
    pub fn leave(&mut self) {
        unsafe {
            ax_up(self.0.as_ptr());
            ax_next(self.0.as_ptr());
        }
    }
    /// Skip the current argument.
    pub fn skip(&mut self) {
        unsafe { ax_next(self.0.as_ptr()) };
    }
}

impl Drop for Msg {
    fn drop(&mut self) {
        unsafe { ax_free(self.0.as_ptr()) }
    }
}
