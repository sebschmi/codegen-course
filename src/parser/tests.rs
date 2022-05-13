use crate::{build_ast, initialise_logging, ReadIterator, Scanner};

#[test]
fn test_gcd_1() {
    let program = "program gcd;
begin
var n1, n2: integer;
read(n1);
read(n2);
if n2 > n1 then
begin
var tmp: integer;
tmp := n1;
n1 := n2;
n2 := tmp;
end;
while n2 > 0 do
begin
var new: integer;
new := n1 % n2;
n1 := n2;
n2 := new;
end;
writeln(n1);
end.
";

    initialise_logging();
    let scanner = Scanner::new(ReadIterator::new(program.as_bytes())).unwrap();
    let ast = build_ast(scanner).unwrap();
    println!("{ast:#?}");
}
