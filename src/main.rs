use core::panic;
use std::{cmp::max, collections::HashMap as HM};
use std::env;
use std::fs::File;
use std::io::prelude::*;

use im::{HashMap, HashSet};


pub mod expr;

use expr::{Defn, Expr, Instr, Op1, Op2, Prog, Reg, Val};

// tests
fn test_number(instrs: &mut Vec<Instr>, code: i32) {
    /*
     * test whether RAX is a number
     */
    instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
    instrs.push(Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(1)));
    instrs.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Imm(0)));

    // throw out the error code
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(code.into())));

    // jump to error label
    instrs.push(Instr::IJne(Val::Label("label_error".to_owned())));
}

fn test_same_type(instrs: &mut Vec<Instr>, code: i32, sp: i32) {
    // raise an "invalid argument" error if type isn't the same
    // type checking... move rax to rcx to check type
    instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
    // xor rcx, sp
    instrs.push(Instr::IXor(
        Val::Reg(Reg::RCX),
        Val::RegOffset(Reg::RBP, sp),
    ));
    // get the right most bit
    instrs.push(Instr::IAnd(Val::Reg(Reg::RCX), Val::Imm(1)));
    // cmp rcx, 1 and jmp error label
    instrs.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Imm(1)));

    // throw out the error
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(code.into())));

    instrs.push(Instr::IJe(Val::Label("label_error".to_owned())));
}

fn test_overflow(instrs: &mut Vec<Instr>, code: i64) { 
    // move code to rdi
    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Imm(code)));
    // jo to label_error
    instrs.push(Instr::IJo(Val::Label("label_error".to_owned())));
}

// Compilers
type Stack = HashMap<String, i32>;
// Mutable Dictionary
type Dict = HM<String, i32>;

fn compile(
    e: &Expr,
    instrs: &mut Vec<Instr>, 
    env: &Stack, 
    sp: i32, 
    cnt: &mut i32, 
    exit_label: &str, 
    inside_loop: &mut Vec<bool>,
    tr: bool,
    funinfo: &mut Dict,
) {
    // instrs.push(Instr::Comment(format!("___start_of_compilation_of_{}___", expr_to_str(e))));
    match e {
        Expr::Number(n) => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))),
        Expr::Id(x) => {
            let x_pos = if env.contains_key(x) {
                env.get(x).unwrap()
            } else {
                panic!("Invalid: unbound variable identifier {}", x)
            };
            instrs.push(Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RBP, *x_pos),
            ));
        }
        Expr::Let(bindings, e2) => {
            // add duplicate binding support here using vec
            let mut new_env = env.clone();
            let mut var_names: HashSet<String> = HashSet::new();
            if bindings.is_empty() {
                panic!("Invalid: Empty binding list")
            }
            for i in 0..bindings.len() {
                let (x, e1) = &bindings[i];
                if var_names.contains(x) {
                    panic!("Duplicate binding");
                }
                var_names.insert(x.clone());
                let x_pos = sp + (i as i32);
                compile(&e1, instrs, &mut new_env, x_pos, cnt, exit_label, inside_loop, false, funinfo);
                // move rax to current stack pointer
                instrs.push(Instr::IMov(
                    Val::RegOffset(Reg::RBP, x_pos),
                    Val::Reg(Reg::RAX),
                ));
                new_env = new_env.update(x.to_owned(), x_pos);
            }
            compile(&e2, instrs, &mut new_env, sp + (bindings.len() as i32), cnt, exit_label, inside_loop, tr, funinfo);
        }
        Expr::UnOp(op1, e1) => match op1 {
            Op1::Add1 => {
                compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                test_number(instrs, 1);
                instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                test_overflow(instrs, 5);
            }
            Op1::Sub1 => {
                compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                test_number(instrs, 1);
                instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                test_overflow(instrs, 5);
            }

            Op1::IsNum => {
                compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                // and rax with 1 to preserve the right most digit
                instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));

                // mov rax to rcx to determine if rax is 0
                instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX)));
                // set rax to 0b01, sub rcx from it. result is 1 => isnum; 0 => notnum
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));

                // left shift rax the right most digit by 1, and add it by 1 to signal bool type
                instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
                instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            }
            Op1::IsBool => {
                compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                // and rax with 1 to preserve the right most digit
                instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                // left shift rax the right most digit by 1, and add it by 1 to signal bool type
                instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(2)));
                instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            }
        },
        Expr::BinOp(op2, e1, e2) => {
            match op2 {
                Op2::Plus => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);
                    // add cur stack pointer to rax
                    instrs.push(Instr::IAdd(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    test_overflow(instrs, 5);
                }
                Op2::Minus => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);

                    // sub stack pointer by the current rax value
                    instrs.push(Instr::ISub(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));
                    // move stack pointer to rax, which should correspond to the sub result
                    instrs.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    test_overflow(instrs, 5);
                }
                Op2::Times => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);
                    // right shift the rax because what sp points to also left shift 1 digit
                    instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // mul rax, sp
                    instrs.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RBP, sp),
                    ));
                    test_overflow(instrs, 5);
                },
                Op2::Equal => {
                    // compile the first expression and leave the res on rax
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);

                    // temporarily store rax to sp
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    // compile the second expression and leave the res on rax
                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // increment cnt by 1
                    
                    // raise an "invalid argument" error if type isn't the same
                    // type checking... move rax to rcx to check type
                    test_same_type(instrs, 3, sp);
                    
                    // compare rax with sp
                    instrs.push(Instr::ICmp(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));
                    // default rax to be false 0b01
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    instrs.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
                },
                Op2::Greater => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    // temporarily store rax to sp
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);

                    
                    // compare rax with sp
                    instrs.push(Instr::ICmp(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));
                    // default rax to be true 0b11
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));

                    // increment cnt by 1
                    *cnt += 1;
                    // jg to current exit label to skip over assigning to true (x11)
                    instrs.push(Instr::IJg(Val::Label(label("exit".to_owned(), cnt))));
                    // not skipped over by jg, assigning to false 0b01
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // exit label, the rest of the code
                    instrs.push(Instr::DefLabel(label("exit".to_owned(), cnt)));
                },
                Op2::GreaterEqual => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    // temporarily store rax to sp
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);

                    
                    // compare rax with sp
                    instrs.push(Instr::ICmp(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));
                    // default rax to be true 0b11
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));

                    // increment cnt by 1
                    *cnt += 1;
                    // jge to current exit label to skip over assigning to true (x11)
                    instrs.push(Instr::IJge(Val::Label(label("exit".to_owned(), cnt))));
                    // not skipped over by jge, assigning to false 0b01
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // exit label, the rest of the code
                    instrs.push(Instr::DefLabel(label("exit".to_owned(), cnt)));
                }
                Op2::Less => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    // temporarily store rax to sp
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);

                    
                    // compare rax with sp
                    instrs.push(Instr::ICmp(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));
                    // default rax to be true 0b11
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));

                    // increment cnt by 1
                    *cnt += 1;
                    // jl to current exit label to skip over assigning to true (x11)
                    instrs.push(Instr::IJl(Val::Label(label("exit".to_string().to_owned(), cnt))));
                    // not skipped over by jl, assigning to false 0b01
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // exit label, the rest of the code
                    instrs.push(Instr::DefLabel(label("exit".to_owned(), cnt)));
                },
                Op2::LessEqual => {
                    compile(e1, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 1
                    test_number(instrs, 1);

                    // temporarily store rax to sp
                    instrs.push(Instr::IMov(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));

                    compile(e2, instrs, env, sp + 1, cnt, exit_label, inside_loop, false, funinfo);
                    // test that rax is a number, if not, call label_error 2
                    test_number(instrs, 2);

                    
                    // compare rax with sp
                    instrs.push(Instr::ICmp(
                        Val::RegOffset(Reg::RBP, sp),
                        Val::Reg(Reg::RAX),
                    ));
                    // default rax to be true 0b11
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));

                    // increment cnt by 1
                    *cnt += 1;
                    // jle to current exit label to skip over assigning to true (x11)
                    instrs.push(Instr::IJle(Val::Label(label("exit".to_owned(), cnt))));
                    // not skipped over by jle, assigning to false 0b01
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));

                    // exit label, the rest of the code
                    instrs.push(Instr::DefLabel(label("exit".to_owned(), cnt)));
                },
            }
        }
        Expr::Boolean(val) => match val {
            true => {
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
            }
            false => instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1))),
        },
        Expr::If(e_cond, e_then, e_else) => {
            *cnt += 1;

            // generate the labels
            let if_else_label = &label("else".to_owned(), cnt);
            let if_exit_label = &label("exit".to_owned(), cnt);
            
            // instrs.push(Instr::Comment("compile if (cond_e) (then_e) (else_e) ".to_owned()));
            // instrs.push(Instr::Comment("compile cond_e".to_owned()));
            compile(&e_cond, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
            instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            instrs.push(Instr::IJe(Val::Label(if_else_label.to_owned())));
            // instrs.push(Instr::Comment("compile then_e".to_owned()));
            compile(&e_then, instrs, env, sp, cnt, exit_label, inside_loop, tr, funinfo);
            instrs.push(Instr::IJmp(Val::Label(if_exit_label.to_owned())));
            instrs.push(Instr::DefLabel(if_else_label.to_owned()));
            // instrs.push(Instr::Comment("compile then_e".to_owned()));
            compile(&e_else, instrs, env, sp, cnt, exit_label, inside_loop, tr, funinfo);
            instrs.push(Instr::DefLabel(if_exit_label.to_owned()));
        }
        Expr::Loop(e) => {
            *cnt += 1;
            inside_loop.push(true);
            
            let loop_start: &str = &label("loop_start".to_owned(), cnt).to_owned();
            let loop_exit: &str = &label("loop_exit".to_owned(), cnt).to_owned();
            instrs.push(Instr::DefLabel(loop_start.to_owned()));
            compile(e, instrs, env, sp, cnt, &loop_exit, inside_loop, false, funinfo);
            instrs.push(Instr::IJmp(Val::Label(loop_start.to_owned())));
            instrs.push(Instr::DefLabel(loop_exit.to_owned()));
            inside_loop.pop();
        }
        Expr::Break(e) => {
            if inside_loop.is_empty() {
                panic!("Invalid break! Not surrounded by any loops.")
            }
            compile(e, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
            instrs.push(Instr::IJmp(Val::Label(exit_label.to_owned())));
        },
        Expr::Set(x, e) => {
            let x_pos = if env.contains_key(x) {
                env.get(x).unwrap()
            } else {
                panic!("Invalid: Unbound variable identifier {}", x)
            };
            compile(e, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
            instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, *x_pos), Val::Reg(Reg::RAX)));

        },
        Expr::Block(es) => {
            if es.is_empty() {
                panic!("Invalid: Empty expr")
            }
            for (i, e) in es.iter().enumerate() {
                compile(e, instrs, env, sp, cnt, exit_label, inside_loop, tr && (i == es.len() - 1), funinfo);
            }
        },
        Expr::Input => {
            instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, 1)));
        }
        Expr::Print(e) => {
            instrs.push(Instr::Comment("Calling print".to_owned()));
            compile(e, instrs, env, sp, cnt, exit_label, inside_loop, false, funinfo);
            // let sp = if sp % 2 == 0 { sp } else { sp + 1 };
            instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
            // instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm((8 * sp).into())));
            instrs.push(Instr::ICall(Val::Label("snek_print".to_owned())));
            // instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm((8 * sp).into())));
        },
        Expr::Call(func_name, args) => {
            instrs.push(Instr::Comment(format!("calling {}", func_name)));

            // before compilation, check if the length of the parameters is the same as in funinfo
            // panic if function not defined
            if !funinfo.contains_key(func_name) {
                panic!("Invalid: function <{}> not found", func_name);
            }

            // panic if not the same
            if funinfo.get(func_name) != Some(&(args.len() as i32)) {
                panic!("Invalid: incorrect number of parameters in the function: {}, expected {}, but got {}",
                func_name, funinfo.get(func_name).unwrap(), args.len());
            }
            
            // tail recursive optimization
            if tr {
                // Tail call optimization
                for (i, arg) in args.iter().enumerate() {
                    compile(arg, instrs, env, sp + i as i32, cnt, exit_label, inside_loop, false, funinfo);
                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, -1 - (args.len() - i) as i32), Val::Reg(Reg::RAX)));
                }
                instrs.push(Instr::IJmp(Val::Label(format!("fun_body_{}", func_name))));

            } else {
                // Compile e_1, e_2, ..., e_n, and push result rax to stack
                for (i, arg) in args.iter().enumerate() {
                    compile(arg, instrs, env, sp + i as i32, cnt, exit_label, inside_loop, false, funinfo);
                    // instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, sp + i as i32), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IPush(Val::Reg(Reg::RAX)));
                }
    
                // Call function
                instrs.push(Instr::ICall(Val::Label(format!("fun_start_{func_name}"))));
                instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(8 * (args.len() as i64))));
            }
        },
    }
    // instrs.push(Instr::Comment(format!("___end_of_compilation_{}___", expr_to_str(e))));
}

// String Formatter
fn x86_one(instr: &Instr) -> String {
    match instr {
        Instr::IMov(dst, src) => format!("  mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("  add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("  sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("  imul {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IJe(label) => format!("  je  {}", val_to_str(label)),
        Instr::ICmp(dst, src) => format!("  cmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IJmp(label) => format!("  jmp {}", val_to_str(label)),
        Instr::DefLabel(s) => format!("{}:", s),
        Instr::IJne(label) => format!("  jne {}", val_to_str(label)),
        Instr::IAnd(dst, src) => format!("  and {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IPush(reg) => format!("  push {}", val_to_str(reg)),
        Instr::ICall(label) => format!("  call {}", val_to_str(label)),
        Instr::IRet => "  ret".to_owned(),
        Instr::IXor(dst, src) => format!("  xor {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IJg(label) => format!("  jg  {}", val_to_str(label)),
        Instr::IJge(label) => format!("  jge  {}", val_to_str(label)),
        Instr::IJl(label) => format!("  jl  {}", val_to_str(label)),
        Instr::IJle(label) => format!("  jle  {}", val_to_str(label)),
        Instr::IJo(label) => format!("  jo {}", val_to_str(label)),
        Instr::IShr(dst, val) => format!("  shr {}, {}", val_to_str(dst), val_to_str(val)),
        Instr::ISar(dst, val) => format!("  sar {}, {}", val_to_str(dst), val_to_str(val)),
        Instr::ICmove(reg1, reg2) => format!("  cmove {}, {}", val_to_str(reg1), val_to_str(reg2)),
        Instr::IPop(reg) => format!("  pop {}", val_to_str(reg)),
        Instr::Comment(str) => format!("  ; {}", str),
    }
}

fn compile_exit(instrs: &mut Vec<Instr>) {
    instrs.push(Instr::IMov(Val::Reg(Reg::RSP), Val::Reg(Reg::RBP)));
    instrs.push(Instr::IPop(Val::Reg(Reg::RBP)));
    instrs.push(Instr::IRet);
}

fn compile_entry(instrs: &mut Vec<Instr>, e: &Expr, sp: usize) {
    let vars = expr_vars(e) + sp;
    // let vars = 1000;
    instrs.push(Instr::IPush(Val::Reg(Reg::RBP)));
    instrs.push(Instr::IMov(Val::Reg(Reg::RBP), Val::Reg(Reg::RSP)));
    instrs.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm((8 * vars).try_into().unwrap())));
}

fn _expr_to_str(e: &Expr) -> &'static str {
    match e {
        Expr::Number(_) => "Number",
        Expr::Boolean(_) => "Boolean",
        Expr::Id(_) => "ID",
        Expr::Let(_, _) => "Let",
        Expr::UnOp(_, _) => "UnOp",
        Expr::BinOp(op2, _, _) => match op2 {
            Op2::Plus => "Plus",
            Op2::Minus => "Minus",
            Op2::Times => "Time",
            Op2::Equal => "Equal",
            Op2::Greater => "Greater",
            Op2::GreaterEqual => "GreaterEqual",
            Op2::Less => "Less",
            Op2::LessEqual => "LessEqual",
        },
        Expr::If(_, _, _) => "If",
        Expr::Loop(_) => "Loop",
        Expr::Break(_) => "Break",
        Expr::Set(_, _) => "Set",
        Expr::Block(_) => "Block",
        Expr::Input => "Input",
        Expr::Print(_) => "Print",
        Expr::Call(_, _) => "Call",
    }
}

fn reg_to_str(reg: &Reg) -> &'static str {
    match reg {
        Reg::RAX => "rax",
        Reg::RSP => "rsp",
        Reg::RDI => "rdi",
        Reg::RCX => "rcx",
        Reg::RBP => "rbp",
        Reg::R11 => "r11",
    }
}

fn val_to_str(val: &Val) -> String {
    match val {
        Val::Imm(imm) => format!("{}", imm),
        Val::Reg(reg) => format!("{}", reg_to_str(reg)),
        Val::RegOffset(reg, offset) => format!("[{} - 8 * {}]", reg_to_str(reg), offset),
        Val::Label(s) => s.to_string(),
    }
}

fn label(prefix: String, count: &i32) -> String {
    format!("{prefix}_{count}")
}

fn x86_string(instrs: Vec<Instr>) -> String {
    instrs
        .iter()
        .map(|i| x86_one(i))
        .collect::<Vec<String>>()
        .join("\n")
}

fn expr_vars(e: &Expr) -> usize {
  match e {
        Expr::Number(_) | Expr::Id(_) | Expr::Input | Expr::Boolean(_) => 0, 
        Expr::Set(_, e)
            | Expr::UnOp(_, e)
            | Expr::Loop(e)
            | Expr::Break(e)
            | Expr::Print(e) => expr_vars(e),
        Expr::BinOp(_, e1 , e2) => max(expr_vars(e1), 1 + expr_vars(e2)),
        Expr::If(e1, e2, e3) => max(expr_vars(e1), max(expr_vars(e2), expr_vars(e3))),
        Expr::Block(es) => es.iter().map(|e| expr_vars(e)).max().unwrap(),
        Expr::Let(es, e2) => max(es.iter().map(|e: &(String, Expr)| expr_vars(&(*e).1)).max().unwrap(), es.len() + expr_vars(e2)),
        Expr::Call(_, args) => args.iter().enumerate().map(|(i, e)| expr_vars(e) + i).max().unwrap_or(0),
  }
}


fn init_env(args: &[String]) -> HashMap<String, i32> {
    let mut env = HashMap::new();
    let mut slot = -2 - (args.len() as i32 - 1);

    for arg in args {
        env.insert(arg.to_string(), slot);
        slot += 1;
    }

    env
}

fn compile_def_body(
    instrs: &mut Vec<Instr>,
    f: &String,
    args: &[String],
    sp: usize,
    body: &Expr,
    count: &mut i32,
    funinfo: &mut Dict,
) {
    let mut inside_loop = vec![];
    // fun_entry
    instrs.push(Instr::Comment("fun_entry".to_owned()));

    let _ = compile_entry(instrs, body, sp);
    instrs.push(Instr::DefLabel(format!("fun_body_{f}")));
    let exit_label: String = format!("fun_exit_{f}");

    // body_code
    instrs.push(Instr::Comment("fun_body".to_owned()));
    let _ = compile(body, instrs, &init_env(args), sp.try_into().unwrap(), count, &exit_label, &mut inside_loop, true, funinfo);
    
    // fun_exit
    instrs.push(Instr::Comment("fun_exit".to_owned()));
    let _ = compile_exit(instrs);
}

fn update_funinfo(def: &Defn, funinfo: &mut Dict) {
    match def {
        Defn::Fun(f, params, _) => {
            // error handling
            // duplicated function name, throw compilation error code of 6
            if funinfo.contains_key(f) {
                panic!("Invalid: duplicated function name {}", f)
            }

            // add the number of args to the funinfo for future checking
            funinfo.insert(f.to_string(), params.len().try_into().unwrap());
        },
    }
}

fn compile_def(instrs: &mut Vec<Instr>, def: &Defn, mut funinfo: &mut Dict, count: &mut i32) {
    let (fun_name, args, body) = match def {
        Defn::Fun(f, params, e) => {
            let mut arg_strings = Vec::new();
            for (_i, param) in params.iter().enumerate() {
                if arg_strings.contains(&param.to_string()) {
                    panic!("Invalid: duplicate argument in function {}, argument <{}> is duplicated", f, param);
                }
                arg_strings.push(param.to_string());
            }
            (f, arg_strings, e)
        },
    };
    instrs.push(Instr::DefLabel(format!("fun_start_{fun_name}")));
    compile_def_body(instrs, fun_name, &args, 1, body, count, &mut funinfo);
}

fn compile_prog(prog: &Prog) -> String {
    /*
//   format!(
//       "section .text
// global our_code_starts_here
// extern snek_error
// extern snek_print
// label_error:
//   push rsp
// call snek_error
// {defs_code}
//   our_code_starts_here:
// {e_entry}
//   mov [rbp - 8], rdi
// {e_code}
// {e_exit}
// global_exit:
//   ret
// "
//   )
     */
    // counter for if and else
    let mut count = 0;
    let mut inside_loop = vec![];
    let mut instrs = vec![];
    // store function names and number of parameters
    let mut funinfo = Dict::new();

    // label_error:
    instrs.push(Instr::DefLabel("label_error".to_owned()));
    instrs.push(Instr::IPush(Val::Reg(Reg::RSP)));
    instrs.push(Instr::ICall(Val::Label("snek_error".to_owned())));

    // update all function info in funinfo first
    prog.defs.iter().for_each(|def| update_funinfo(def, &mut funinfo));

    // defs code
    prog.defs.iter().for_each(|def| compile_def(&mut instrs, def, &mut funinfo, &mut count));

    // for i in 0..prog.defs.len() {
    //     let def = &prog.defs[i];
    //     let _ = compile_def(&mut instrs, def, &mut funinfo, &mut count);
    // }


    // out_code_starts_here:
    instrs.push(Instr::DefLabel("our_code_starts_here".to_owned()));

    // e_entry
    instrs.push(Instr::Comment("e_entry".to_owned()));
    compile_entry(&mut instrs, &prog.expr, 1);

    // save the input
    instrs.push(Instr::Comment("save the input".to_owned()));
    instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, 1), Val::Reg(Reg::RDI)));

    // e_code
    instrs.push(Instr::Comment("e_code".to_owned()));
    compile(
        &prog.expr,
        &mut instrs,
        &mut Stack::new(),
        2,
        &mut count,
        "global_exit",
        &mut inside_loop,
        false,
        &mut funinfo,
    );
    // e_exit
    instrs.push(Instr::Comment("e_exit".to_owned()));
    let _ = compile_exit(&mut instrs);
    instrs.push(Instr::DefLabel("global_exit".to_owned()));
    instrs.push(Instr::IRet);
    x86_string(instrs)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = expr::parse(&in_contents);

    let mut out_file = File::create(out_name)?;
    let res = compile_prog(&prog);
    let asm_program = format!("section .text
global our_code_starts_here
extern snek_error
extern snek_print
{}", res);

    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
