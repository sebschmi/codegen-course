program strings;

function concat(str1: string, str2: string): string;
begin
    return str1 + str2;
end;

begin
    var pair: array[3] of string;
    read(pair[0]);
    read(pair[1]);
    pair[2] := "abc::%\"{*l";

    var result: string;
    result := concat(pair[0], pair[1]);
    result := concat(result, pair[2]);
    assert(pair[0] <> result);
    writeln(result);
end.