use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM (which Rust uses) that ensures
    // it does not add an underscore in front of the name, which happens on OSX
    // Courtesy of Max New
    // (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub fn snek_error(errcode: i64) {
    eprintln!("an error ocurred {errcode}");
    if errcode == 1 || errcode == 2 {
        eprintln!("Invalid arguments");
    } if errcode == 3 {
        eprintln!("Invalid arguments")
    } if errcode == 5 {
        eprintln!("Invalid: overflow")
    }
    std::process::exit(1);
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val: i64) -> i64 {
    if val == 1 {
        println!("false");
    } else if val == 3 {
        println!("true");
    } else {
        println!("{}", val >> 1);
    }
    return val;
}

fn parse_arg(v: &Vec<String>) -> i64 {
    if v.len() <= 1 {
        return 1;
    }
    let s = &v[1];
    if s == "true" {
        3
    } else if s == "false" {
        1
    } else {
        s.parse::<i64>().unwrap() << 1
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_arg(&args);

    let i: i64 = unsafe { our_code_starts_here(input) };
    snek_print(i);
}