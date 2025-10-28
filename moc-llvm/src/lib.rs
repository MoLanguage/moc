use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::{LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetMachineEmitToFile};

use std::ffi::CString;
use std::ptr;
use std::str::FromStr;

pub fn compile_with_llvm() {
    unsafe {
        // Initialize the target for your machine
        LLVMInitializeX86TargetInfo();
        LLVMInitializeX86Target();
        LLVMInitializeX86TargetMC();
        LLVMInitializeX86AsmPrinter();
        LLVMInitializeX86AsmParser();


        // Create LLVM context and module
        let context = LLVMContextCreate();
        let module_name = CString::new("hello_module").unwrap();
        let module = LLVMModuleCreateWithName(module_name.as_ptr());

        // Create i32 type
        let i32_type = LLVMInt32TypeInContext(context);

        // Define main function: i32 main()
        let fn_type = LLVMFunctionType(i32_type, ptr::null_mut(), 0, 0);
        let main_fn_name = CString::new("main").unwrap();
        let main_fn = LLVMAddFunction(module, main_fn_name.as_ptr(), fn_type);

        // Create entry basic block
        let entry_bb_name = CString::new("entry").unwrap();
        let entry_bb = LLVMAppendBasicBlockInContext(context, main_fn, entry_bb_name.as_ptr());
        let builder = LLVMCreateBuilderInContext(context);
        LLVMPositionBuilderAtEnd(builder, entry_bb);

        // Add return 0 instruction
        let zero = LLVMConstInt(i32_type, 0, 0);
        LLVMBuildRet(builder, zero);

        // Set up target machine
        let triple_str = LLVMGetDefaultTargetTriple();
        //let triple_str = CString::from_str("x86_64-pc-linux-gnu").unwrap().as_ptr();
        //println!("{:?}", CString::from_raw(triple_str));
        let mut target = ptr::null_mut();
        let mut error = ptr::null_mut();
        if LLVMGetTargetFromTriple(triple_str, &mut target, &mut error) != 0 {
            eprintln!("Error getting target: {:?}", CString::from_raw(error));
            return;
        }

        let cpu = CString::new("generic").unwrap();
        let features = CString::new("").unwrap();
        let opt_level = LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
        let reloc_mode = LLVMRelocMode::LLVMRelocDefault;
        let code_model = LLVMCodeModel::LLVMCodeModelDefault;

        let target_machine = LLVMCreateTargetMachine(
            target,
            triple_str,
            cpu.as_ptr(),
            features.as_ptr(),
            opt_level,
            reloc_mode,
            code_model,
        );

        // Emit object file
        let filename = CString::new("hello.o").unwrap();
        if LLVMTargetMachineEmitToFile(
            target_machine,
            module,
            filename.as_ptr() as *mut _,
            LLVMCodeGenFileType::LLVMObjectFile,
            &mut error,
        ) != 0
        {
            eprintln!("Error emitting object file: {:?}", CString::from_raw(error));
            return;
        }

        println!("Object file hello.o generated! Now link it with: gcc hello.o -o hello");

        // Clean up
        LLVMDisposeBuilder(builder);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);
    }
}
