program arrays;

function produce_array(var n: array[] of integer, length: integer): integer;
begin
    var a: array[length] of integer;
    n := a;

    var i : integer;
    i := 0;
    while i < n.size do
    begin
        writeln(i, n.size, a.size);
        a[i] := 2 * 2 + i * i;
        i := i + 1;
    end;
    return a.size;
end;

begin
    var a: array[3] of integer;
    var b: array[] of integer;
    b := a;

    var i: integer;
    i := 0;
    while i < a.size do
    begin
        a[i] := i;
        i := i + 1;
    end;

    i := 0;
    while i < b.size do
    begin
        assert(b[i] = i);
        i := i + 1;
    end;

    var length: integer
    length := produce_array(a, 40);
    i := 0;
    while i < length do
    begin
        assert(a[i] = 2 * 2 + i * i);
        i := i + 1;
    end;

    i := 0;
    while i < b.size do
    begin
        assert(b[i] = i);
        i := i + 1;
    end;
end.