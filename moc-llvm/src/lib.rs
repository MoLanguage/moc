use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::{LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetMachineEmitToFile};

use std::ffi::CStr;
use std::ffi::CString;
use std::ptr;

pub fn compile_with_llvm() {
    unsafe {
        // Initialize the X86 backend
        LLVMInitializeX86TargetInfo();
        LLVMInitializeX86Target();
        LLVMInitializeX86TargetMC();
        LLVMInitializeX86AsmPrinter();
        LLVMInitializeX86AsmParser();

        // Create context and module
        let context = LLVMContextCreate();
        let module_name = CString::new("hello_module").unwrap();
        let module = LLVMModuleCreateWithName(module_name.as_ptr());
        let builder = LLVMCreateBuilderInContext(context);

        // Define "puts" from libc
        let i8ptr = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
        let puts_param_types = [i8ptr];
        let puts_type = LLVMFunctionType(LLVMInt32TypeInContext(context), puts_param_types.as_ptr() as *mut _, 1, 0);
        let puts_name = CString::new("puts").unwrap();
        LLVMAddFunction(module, puts_name.as_ptr(), puts_type);

        // Create main() function
        let i32_type = LLVMInt32TypeInContext(context);
        let fn_type = LLVMFunctionType(i32_type, ptr::null_mut(), 0, 0);
        let main_fn_name = CString::new("main").unwrap();
        let main_fn = LLVMAddFunction(module, main_fn_name.as_ptr(), fn_type);
        let entry_bb_name = CString::new("entry").unwrap();
        let entry = LLVMAppendBasicBlockInContext(context, main_fn, entry_bb_name.as_ptr());
        LLVMPositionBuilderAtEnd(builder, entry);

        // Make global string constant
        let hello_str = CString::new("Hello, world!").unwrap();
        let global_str = LLVMBuildGlobalString(builder, hello_str.as_ptr(), CString::new("str").unwrap().as_ptr());

        // Call puts("Hello, world!")
        let puts_fn = LLVMGetNamedFunction(module, puts_name.as_ptr());
        let mut args = [global_str];
        LLVMBuildCall2(
            builder,
            puts_type,
            puts_fn,
            args.as_mut_ptr(),
            args.len() as u32,
            CString::new("").unwrap().as_ptr(),
        );

        // Return 0
        let zero = LLVMConstInt(i32_type, 0, 0);
        LLVMBuildRet(builder, zero);

        // Emit object file
        let triple_ptr = LLVMGetDefaultTargetTriple();
        let triple_str = CStr::from_ptr(triple_ptr).to_string_lossy().to_string();
        let triple_cstring = CString::new(triple_str.clone()).unwrap();

        let mut target = ptr::null_mut();
        let mut error = ptr::null_mut();
        if LLVMGetTargetFromTriple(triple_cstring.as_ptr(), &mut target, &mut error) != 0 {
            let err_str = if !error.is_null() {
                CStr::from_ptr(error).to_string_lossy().into_owned()
            } else {
                "unknown".to_string()
            };
            eprintln!("Error getting target: {}", err_str);
            return;
        }

        let cpu = CString::new("generic").unwrap();
        let features = CString::new("").unwrap();
        let tm = LLVMCreateTargetMachine(
            target,
            triple_cstring.as_ptr(),
            cpu.as_ptr(),
            features.as_ptr(),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        );

        let filename = CString::new("hello.o").unwrap();
        if LLVMTargetMachineEmitToFile(
            tm,
            module,
            filename.as_ptr() as *mut _,
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut error,
        ) != 0
        {
            let err_str = if !error.is_null() {
                CStr::from_ptr(error).to_string_lossy().into_owned()
            } else {
                "unknown".to_string()
            };
            eprintln!("Error emitting object file: {}", err_str);
            return;
        }

        println!("Object file 'hello.o' generated!");
        println!("Link it with: gcc hello.o -o hello");
        println!("Run it: ./hello");

        // Cleanup
        LLVMDisposeBuilder(builder);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);
    }
}
