use lox_rs::{
    ast::{parser::Parser, resolver::Resolver},
    interpreter::ast_interpreter::Interpreter,
    lexer::Scanner,
};

fn run_and_get_output(source: &'static str) -> String {
    let mut output = Vec::new();
    let tokens = Scanner::new(source).scan_tokens().unwrap();
    let mut ast = Parser::new(tokens).parse().unwrap();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut ast).unwrap();

    let mut interpreter = Interpreter::new(&mut output);
    interpreter.interpret(ast).unwrap();
    String::from_utf8(output).unwrap()
}

#[test]
fn test_program_fibonnacci() {
    let source = r#"
        fun fib(n) {
            if (n <= 1) return 1;
            return fib(n - 1) + fib(n - 2);
        }

        print fib(6);
    "#;

    assert_eq!(run_and_get_output(source), "13\n");
}

#[test]
fn test_program_inner_loop() {
    let source = r#"
        fun loop() {
            var i = 0;
            while (i < 10) {
                print i;
                i = i + 1;
            }
        }

        loop();
    "#;

    assert_eq!(run_and_get_output(source), "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n");
}

#[test]
fn test_program_while_for_equivalence() {
    let source = r#"
        fun increment(n) {
            return n*2;
        }

        var while_total = 0;
        var for_total = 0;

        var i = 0;
        while (i < 10) {
            while_total = increment(while_total);
            i = i + 1;
        }

        for (var j = 0; j < 10; j = j + 1) {
            for_total = increment(for_total);
        }

        print while_total == for_total;
    "#;

    assert_eq!(run_and_get_output(source), "true\n");
}

#[test]
fn test_program_inner_function() {
    let source = r#"
        fun outer() {
            var i = 0;
            fun inner() {
                print i;
                i = i + 1;
            }

            return inner;
        }

        var inner = outer();
        inner();
        inner();
        inner();
    "#;

    assert_eq!(run_and_get_output(source), "0\n1\n2\n");
}
#[test]
fn test_program_empty() {
    let source = r#"
        print "Hello, World!";
    "#;

    assert_eq!(run_and_get_output(source), "Hello, World!\n");
}

#[test]
fn test_program_variable_declaration() {
    let source = r#"
        var x = 10;
        var y = 5;
        var z = x + y;
        print z;
    "#;

    assert_eq!(run_and_get_output(source), "15\n");
}

#[test]
fn test_program_if_statement() {
    let source = r#"
        var x = 10;
        if (x > 5) {
            print "x is greater than 5";
        } else {
            print "x is less than or equal to 5";
        }
    "#;

    assert_eq!(run_and_get_output(source), "x is greater than 5\n");
}

#[test]
fn test_program_for_loop() {
    let source = r#"
        var sum = 0;
        for (var i = 1; i <= 10; i = i + 1) {
            sum = sum + i;
        }
        print sum;
    "#;

    assert_eq!(run_and_get_output(source), "55\n");
}

#[test]
fn test_program_function_call() {
    let source = r#"
        fun add(a, b) {
            return a + b;
        }

        var result = add(5, 3);
        print result;
    "#;

    assert_eq!(run_and_get_output(source), "8\n");
}

