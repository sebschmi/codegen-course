use crate::{compile, initialise_logging};
use std::io::Write;

pub struct NullWriter;

impl Write for NullWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

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
    let mut output = Vec::<u8>::new();
    compile(program.as_bytes(), &mut output).unwrap();
    let output = String::from_utf8(output).unwrap();
    println!("{output}");
}

#[test]
fn test_gcd_2() {
    let program = "program gcd2;

function gcd(n1: integer, n2: integer): integer;
begin
    if n2 > n1 then
    begin
        var new: integer;
        new := n2 % n1;
        n2 := new;
    end
    else
    begin
        var new: integer;
        new := n1 % n2;
        n1 := n2;
        n2 := new;
    end;

    if n2 > 0 then
    begin
        return gcd(n1, n2);
    end
    else
    begin
        return n1;
    end;
end;

begin
    var n1, n2: integer;
    read(n1);
    read(n2);
    writeln(gcd(n1, n2));
end.";

    initialise_logging();
    let mut output = Vec::<u8>::new();
    compile(program.as_bytes(), &mut output).unwrap();
    let output = String::from_utf8(output).unwrap();
    println!("{output}");
}

#[test]
fn test_gcd_3() {
    let program = "program gcd3;
procedure sort_pair_descending(pair: array[2] of integer);
begin
    if pair[0] < pair[1] then
    begin
        var tmp: integer;
        tmp := pair[0];
        pair[0] := pair[1];
        pair[1] := pair[0];
    end;
end;

procedure gcd(pair: array[2] of integer, var result: integer);
begin
    sort_pair_descending(pair);

    var new: integer;
    new := pair[0] % pair[1];
    pair[0] := pair[1];
    pair[1] := new;

    if pair[1] > 0 then
        return gcd(pair, result) {* semicolons are only used between statements in a block *}
    else
        result := pair[0]
    ;
end;

begin
    var pair: array[2] of integer;
    read(pair[0]);
    read(pair[1]);

    var result: integer;
    gcd(pair, result);
    writeln(result);
end.";

    initialise_logging();
    let mut output = Vec::<u8>::new();
    compile(program.as_bytes(), &mut output).unwrap();
    let output = String::from_utf8(output).unwrap();
    println!("{output}");
}
