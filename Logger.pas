unit Logger;

interface

procedure Log(msg : string);

implementation

procedure Log(msg : string);
var
  f : TextFile;
begin
  AssignFile(f, 'D:\Test.txt');
  Append(f);
  Writeln(f, msg);
  CloseFile(f);
end;
end.
