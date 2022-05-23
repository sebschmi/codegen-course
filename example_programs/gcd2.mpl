program gcd2;

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
end.