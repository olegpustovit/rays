use std::ffi::c_void;
use std::io;
use std::mem::size_of;
use std::os::windows::io::{AsRawHandle, FromRawHandle, OwnedHandle};
use std::os::windows::process::CommandExt;
use std::process::{Child, Command};
use std::ptr;
use std::time::Duration;

#[repr(C)]
#[derive(Default)]
struct JobLimits {
    process_time: i64,
    job_time: i64,
    flags: u32,
    minimum_working_set: usize,
    maximum_working_set: usize,
    active_process_limit: u32,
    affinity: usize,
    priority_class: u32,
    scheduling_class: u32,
}

#[repr(C)]
#[derive(Default)]
struct ExtendedJobLimits {
    basic: JobLimits,
    io_counters: [u64; 6],
    memory_limits: [usize; 4],
}

#[repr(C)]
#[derive(Default)]
struct ThreadEntry {
    size: u32,
    usage: u32,
    thread_id: u32,
    process_id: u32,
    base_priority: i32,
    delta_priority: i32,
    flags: u32,
}

#[link(name = "kernel32")]
unsafe extern "system" {
    fn CreateJobObjectW(attributes: *const c_void, name: *const u16) -> *mut c_void;
    fn SetInformationJobObject(job: *mut c_void, class: i32, info: *const c_void, length: u32) -> i32;
    fn AssignProcessToJobObject(job: *mut c_void, process: *mut c_void) -> i32;
    fn TerminateJobObject(job: *mut c_void, exit_code: u32) -> i32;
    fn CreateToolhelp32Snapshot(flags: u32, process_id: u32) -> *mut c_void;
    fn Thread32First(snapshot: *mut c_void, entry: *mut ThreadEntry) -> i32;
    fn Thread32Next(snapshot: *mut c_void, entry: *mut ThreadEntry) -> i32;
    fn OpenThread(access: u32, inherit: i32, thread_id: u32) -> *mut c_void;
    fn ResumeThread(thread: *mut c_void) -> u32;
    fn OpenProcess(access: u32, inherit: i32, process_id: u32) -> *mut c_void;
    fn WaitForSingleObject(handle: *mut c_void, milliseconds: u32) -> u32;
}

/// Owns a process tree even after its root exits. Closing the last handle kills the tree.
pub struct Job(OwnedHandle);

impl Job {
    /// Assign the suspended child before any of its code can spawn an unowned descendant.
    pub fn spawn(command: &mut Command) -> io::Result<(Child, Self)> {
        let handle = unsafe { CreateJobObjectW(ptr::null(), ptr::null()) };
        if handle.is_null() {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: CreateJobObjectW returned a new, exclusively owned kernel handle.
        let job = Self(unsafe { OwnedHandle::from_raw_handle(handle) });
        let mut limits = ExtendedJobLimits::default();
        limits.basic.flags = 0x2000; // JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
        let configured = unsafe {
            SetInformationJobObject(
                job.0.as_raw_handle(),
                9, // JobObjectExtendedLimitInformation
                (&limits as *const ExtendedJobLimits).cast(),
                size_of::<ExtendedJobLimits>() as u32,
            )
        };
        if configured == 0 {
            return Err(io::Error::last_os_error());
        }
        let mut child = command.creation_flags(0x4).spawn()?; // CREATE_SUSPENDED
        if let Err(error) = job.assign_and_resume(&child) {
            // No child code has run: killing this root also covers assignment failures.
            let _ = child.kill();
            let _ = child.wait();
            return Err(error);
        }
        Ok((child, job))
    }

    fn assign_and_resume(&self, child: &Child) -> io::Result<()> {
        if unsafe { AssignProcessToJobObject(self.0.as_raw_handle(), child.as_raw_handle()) } == 0 {
            return Err(io::Error::last_os_error());
        }
        // Stable std hides the primary thread handle. The suspended process has only that thread.
        let snapshot = unsafe { CreateToolhelp32Snapshot(0x4, 0) }; // TH32CS_SNAPTHREAD
        if snapshot == -1isize as *mut c_void {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: the snapshot is a new valid kernel handle, closed on every return path.
        let snapshot = unsafe { OwnedHandle::from_raw_handle(snapshot) };
        let mut entry = ThreadEntry { size: size_of::<ThreadEntry>() as u32, ..Default::default() };
        let mut found = unsafe { Thread32First(snapshot.as_raw_handle(), &mut entry) };
        while found != 0 {
            if entry.process_id == child.id() {
                let thread = unsafe { OpenThread(0x2, 0, entry.thread_id) }; // THREAD_SUSPEND_RESUME
                if thread.is_null() {
                    return Err(io::Error::last_os_error());
                }
                // SAFETY: OpenThread returned a new, exclusively owned kernel handle.
                let thread = unsafe { OwnedHandle::from_raw_handle(thread) };
                if unsafe { ResumeThread(thread.as_raw_handle()) } == u32::MAX {
                    return Err(io::Error::last_os_error());
                }
                return Ok(());
            }
            found = unsafe { Thread32Next(snapshot.as_raw_handle(), &mut entry) };
        }
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(18) { // ERROR_NO_MORE_FILES
            Err(io::Error::new(io::ErrorKind::NotFound, "suspended child thread not found"))
        } else {
            Err(error)
        }
    }

    pub fn terminate(&self) -> io::Result<()> {
        if unsafe { TerminateJobObject(self.0.as_raw_handle(), 124) } == 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

/// Wait for an existing process to exit; a PID that no longer exists has already exited.
pub fn wait_for_process_exit(process_id: u32, timeout: Duration) -> io::Result<bool> {
    let process = unsafe { OpenProcess(0x0010_0000, 0, process_id) }; // SYNCHRONIZE
    if process.is_null() {
        let error = io::Error::last_os_error();
        return if error.raw_os_error() == Some(87) { Ok(true) } else { Err(error) };
    }
    // SAFETY: OpenProcess returned a new, exclusively owned kernel handle.
    let process = unsafe { OwnedHandle::from_raw_handle(process) };
    let milliseconds = timeout.as_millis().min(u128::from(u32::MAX - 1)) as u32;
    match unsafe { WaitForSingleObject(process.as_raw_handle(), milliseconds) } {
        0 => Ok(true),
        258 => Ok(false), // WAIT_TIMEOUT
        _ => Err(io::Error::last_os_error()),
    }
}
