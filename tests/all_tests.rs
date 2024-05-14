mod infra;

// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: double,
        file: "double.snek",
        input: "7",
        expected: "14",
    },
    {
        name: quick_brown_fox0,
        file: "quick_brown_fox.snek",
        input: "55",
        expected: "-3776",
    },
    {
        name: quick_brown_fox1,
        file: "quick_brown_fox.snek",
        input: "500",
        expected: "-35816",
    },
    {
        name: complicated_args,
        file: "complicated_args.snek",
        expected: "-114",
    }, 
    {
        name: complicated_args1,
        file: "complicated_args1.snek",
        input: "4",
        expected: "-2178126642651594820",
    }
}

runtime_error_tests! {
    {
        name: complicated_args_overflow,
        file: "complicated_args1.snek",
        input: "5",
        expected: "Invalid: overflow",
    }
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_function,
        file: "duplicate_function0.snek",
        expected: "Invalid",
    },
    {
        name: incorrect_params,
        file: "incorrect_params.snek",
        expected: "Invalid",
    },
    {
        name: fun_body_input,
        file: "fun_body_input.snek",
        expected: "Invalid",
    },
    {
        name: fun_header_input,
        file: "fun_header_input.snek",
        expected: "Invalid",
    },
    {
        name: fun_name_invalid,
        file: "fun_name_invalid.snek",
        expected: "Invalid",
    },
    {
        name: fun_name_undefined,
        file: "fun_name_undefined.snek",
        expected: "Invalid",
    }
}
