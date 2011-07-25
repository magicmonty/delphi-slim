unit Logger;

interface

var doLog : Boolean = True;
procedure Log(msg : string);

implementation

procedure Log(msg : string);
var
  f : TextFile;
begin
  if doLog then
  begin
    AssignFile(f, 'D:\Test.txt');
    Append(f);
    Writeln(f, msg);
    CloseFile(f);
  end;
end;
end.
