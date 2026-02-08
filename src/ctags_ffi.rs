use std::ffi::CString;
use std::os::raw::{c_char, c_int};

#[link(name = "dctags")]
extern "C" {
    #[allow(dead_code)]
    fn dctags_process_file_direct(filename: *const c_char) -> c_int;
    fn dctags_process_rust_file(filename: *const c_char) -> c_int;
}

pub struct DCTags;

impl DCTags {
    pub fn new() -> Result<Self, String> {
        Ok(DCTags)
    }

    pub fn process_file_direct(&self, filename: &str) -> Result<(), String> {
        let c_filename = CString::new(filename)
            .map_err(|e| format!("Failed to convert filename: {}", e))?;

        unsafe {
            if dctags_process_file_direct(c_filename.as_ptr()) != 0 {
                return Err("Failed to process file directly".to_string());
            }
        }
        Ok(())
    }

    pub fn process_rust_file(&self, filename: &str) -> Result<(), String> {
        let c_filename = CString::new(filename)
            .map_err(|e| format!("Failed to convert filename: {}", e))?;

        unsafe {
            if dctags_process_rust_file(c_filename.as_ptr()) != 0 {
                return Err("Failed to process Rust file".to_string());
            }
        }
        Ok(())
    }
}

impl Drop for DCTags {
    fn drop(&mut self) {
    }
}

// Stub functions required by ctags_amalg.c
// These are minimal implementations to satisfy linking

#[no_mangle]
pub extern "C" fn rs_getTagsToStdout() -> bool {
    false  // We collect tags via makeTagEntry callback instead
}

#[no_mangle]
pub extern "C" fn rs_freeTagFileResourcesDirect() {
    // No-op: cargo-slicer doesn't use tag files
}

#[no_mangle]
pub extern "C" fn rs_getTagFile() -> *mut std::ffi::c_void {
    std::ptr::null_mut()  // We don't use tag files
}

#[no_mangle]
pub extern "C" fn rs_error_impl(_selection: c_int, _message: *const c_char) {
    // Stub for error handling - ctags error messages are suppressed
    // In cargo-slicer, we handle errors via Result<> return values
}

// Additional stub functions required by test linking
// These are internal ctags functions not needed for our minimal FFI usage

#[no_mangle]
pub extern "C" fn rs_parserNew() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_vStringNew() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_vStringCatS(_s: *mut std::ffi::c_void, _str: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_vStringPut(_s: *mut std::ffi::c_void, _c: c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_vStringClear(_s: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_getCKinds() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getCKindsCount() -> c_int {
    0
}

#[no_mangle]
pub extern "C" fn rs_fileGetc() -> c_int {
    -1  // EOF
}

#[no_mangle]
pub extern "C" fn rs_getOption(_name: *const c_char) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_stringCopy(_dest: *mut c_char, _src: *const c_char, _size: usize) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_getFile() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_isWhitespace(_c: c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_setTagsToStdout(_enable: bool) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_verbose_impl(_message: *const c_char) {
    // No-op - suppress verbose output
}

#[no_mangle]
pub extern "C" fn findCTags() -> c_int {
    0
}

#[no_mangle]
pub extern "C" fn initializeCParser() {
    // No-op
}

#[no_mangle]
pub extern "C" fn initializeCppParser() {
    // No-op
}

#[no_mangle]
pub extern "C" fn isDestinationStdout() -> bool {
    false
}

#[no_mangle]
pub extern "C" fn tagFileName() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn catFile(_file: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListCount(_list: *mut std::ffi::c_void) -> c_int {
    0
}

#[no_mangle]
pub extern "C" fn rs_stringListItem(_list: *mut std::ffi::c_void, _index: c_int) -> *const c_char {
    std::ptr::null()
}

// Additional stub functions required for fuzz_ctags_vs_syn test linking

#[no_mangle]
pub extern "C" fn rs_isIdentStart(_c: c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn eMalloc(size: usize) -> *mut std::ffi::c_void {
    unsafe { libc::malloc(size) }
}

#[no_mangle]
pub extern "C" fn eRealloc(ptr: *mut std::ffi::c_void, size: usize) -> *mut std::ffi::c_void {
    unsafe { libc::realloc(ptr, size) }
}

#[no_mangle]
pub extern "C" fn eStat(_path: *const c_char, _buf: *mut std::ffi::c_void) -> c_int {
    // Stub: return success (0) to indicate file exists
    0
}

#[no_mangle]
pub extern "C" fn initTagEntry(entry: *mut std::ffi::c_void, name: *const c_char, _kind_index: c_int) {
    // No-op - tag entry initialization stub
    let _ = (entry, name);
}

#[no_mangle]
pub extern "C" fn rs_addToScope(_scope: *mut std::ffi::c_void, _name: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_eFree(ptr: *mut std::ffi::c_void) {
    unsafe { libc::free(ptr) }
}

#[no_mangle]
pub extern "C" fn rs_eStatFree(_stat: *mut std::ffi::c_void) {
    // No-op - stat buffer cleanup
}

#[no_mangle]
pub extern "C" fn rs_getExecutableProgram() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_getRustKinds() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getSets() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getSetUpper(_set: *mut std::ffi::c_void) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_isIdent(_c: c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_setSets(_sets: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_vStringAutoResize(_s: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_vStringDelete(_s: *mut std::ffi::c_void) {
    // No-op - vString deletion
}

#[no_mangle]
pub extern "C" fn rs_vStringNewCopy(_s: *const std::ffi::c_void) -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_vStringSetLength(_s: *mut std::ffi::c_void, _len: usize) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_vStringStripTrailing(_s: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_vStringTruncate(_s: *mut std::ffi::c_void, _pos: usize) {
    // No-op
}

// Final batch of stub functions

#[no_mangle]
pub extern "C" fn combinePathAndFile(_path: *const c_char, _file: *const c_char) -> *mut c_char {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn doesFileExist(_filename: *const c_char) -> bool {
    true
}

#[no_mangle]
pub extern "C" fn getExtensionLanguage(_filename: *const c_char) -> c_int {
    -1  // No language
}

#[no_mangle]
pub extern "C" fn getPatternLanguage(_filename: *const c_char) -> c_int {
    -1  // No language
}

#[no_mangle]
pub extern "C" fn isRecursiveLink(_filename: *const c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_addTotals(_added: c_int, _prev: c_int, _current: c_int) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_argForth(_arg: *mut std::ffi::c_void) -> bool {
    false  // End of arguments
}

#[no_mangle]
pub extern "C" fn rs_argItem(_arg: *const std::ffi::c_void) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_argOff(_arg: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_baseFilename(_filename: *const c_char) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_eStrdup(_s: *const c_char) -> *mut c_char {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_fileExtension(_filename: *const c_char) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_fileReadLine(_buffer: *mut std::ffi::c_void, _file: *mut std::ffi::c_void) -> bool {
    false  // EOF
}

#[no_mangle]
pub extern "C" fn rs_getExcluded() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getLanguageTable() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getRegexBroken() -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_getRustKindsCount() -> c_int {
    0
}

#[no_mangle]
pub extern "C" fn rs_setSetUpper(_set: *mut std::ffi::c_void, _value: bool) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListFileMatched(_list: *mut std::ffi::c_void, _filename: *const c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_vStringNewInit(_s: *const c_char) -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_setNonOptionEncountered(_value: bool) {
    // No-op
}

// Last batch of stub functions

#[no_mangle]
pub extern "C" fn getExecutableName() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn getLanguageName(_lang_index: c_int) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_argNewFromArgv(_argv: *const *const c_char) -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_argNewFromLineFile(_file: *mut std::ffi::c_void) -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_cArgDelete(_arg: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_etagsInclude() -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_filesRequired() -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_freeKeywordTable(_table: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_freeOptionResourcesDirect() {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_freeRegexResourcesDirect() {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_getLanguageCount() -> c_int {
    0
}

#[no_mangle]
pub extern "C" fn rs_incrLanguageCount() {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_isSkipConfiguration() -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_setEtagsMode(_value: bool) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_setLanguageTable(_table: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_setOptionFiles(_files: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_setSkipConfiguration(_value: bool) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListNew() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn setCurrentDirectory(_dir: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn setExecutableName(_name: *const c_char) {
    // No-op
}

// Additional string list stub functions

#[no_mangle]
pub extern "C" fn rs_stringListClear(_list: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListAdd(_list: *mut std::ffi::c_void, _str: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListPrint(_list: *const std::ffi::c_void, _file: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_setFilesRequired(_value: bool) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_getExcludedPtr() -> *mut *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_freeList(_list: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListCombine(_list1: *mut std::ffi::c_void, _list2: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_setExcluded(_excluded: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_freeString(_str: *mut c_char) {
    // No-op - stub for string deallocation
}

// More ctags stub functions

#[no_mangle]
pub extern "C" fn rs_vStringNCopyS(_dest: *mut std::ffi::c_void, _src: *const c_char, _n: usize) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_getCheckFile() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn isSameFile(_file1: *const c_char, _file2: *const c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_argNewFromString(_str: *const c_char) -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getStartOfLine() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_isAbsolutePath(_path: *const c_char) -> bool {
    false
}

// Final set of ctags stub functions

#[no_mangle]
pub extern "C" fn getNamedLanguage(_name: *const c_char, _len: usize) -> c_int {
    -1  // No language
}

#[no_mangle]
pub extern "C" fn rs_stringListAddString(_list: *mut std::ffi::c_void, _str: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListRemoveExtension(_list: *mut std::ffi::c_void, _ext: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListDelete(_list: *mut std::ffi::c_void) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_stringListNewFromArgv(_argv: *const *const c_char) -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_getLicense1() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_getLicense2() -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_setCheckFile(_file: *const c_char) {
    // No-op
}

#[no_mangle]
pub extern "C" fn rs_getOptionFiles() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn rs_stringListHasTest(_list: *const std::ffi::c_void, _test: *const c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_isNonOptionEncountered() -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_isFalse(_str: *const c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_isTrue(_str: *const c_char) -> bool {
    false
}

#[no_mangle]
pub extern "C" fn rs_getHeaderExtensions() -> *mut std::ffi::c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn relativeFilename(_file: *const c_char, _base: *const c_char) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn rs_stringListExtensionMatched(_list: *const std::ffi::c_void, _ext: *const c_char) -> bool {
    false
}
