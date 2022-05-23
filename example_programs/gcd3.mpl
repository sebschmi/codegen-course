program gcd3;
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
end.