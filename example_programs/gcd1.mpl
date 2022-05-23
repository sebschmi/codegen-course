program gcd;
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