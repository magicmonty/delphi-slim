unit SlimDirectiveDeserializer;

interface

uses SlimDirective;

type TSlimDirectiveDeserializer = class
  public
    function Deserialize(input : string) : TSlimDirective;
  private
    function ParseString(input : string) : TSlimDirective;
    function ParseList(input : string) : TSlimDirective;
end;

implementation

uses StrUtils, SysUtils;
{ TSlimDirectiveDeserializer }

function TSlimDirectiveDeserializer.Deserialize(input : string): TSlimDirective;
begin
  Result := ParseString(RightStr(input, Length(input) - 7));
end;


function TSlimDirectiveDeserializer.ParseString(input: string): TSlimDirective;
begin
  if LeftStr( input, 1) = '[' then
  begin
    Result := ParseList(Copy(input, 2,  Length(input) - 2));
  end
  else begin
    Result := TSlimStringDirective.Create(input);
  end;
end;

function TSlimDirectiveDeserializer.ParseList(input: string): TSlimDirective;
var size : Integer;
  i : Integer;
  currentPos : Integer;
begin
    Result := TSlimDirective.Create;
    currentPos := 8;
    for i := 1 to StrToInt(LeftStr(input, 6))  do
    begin
      size := StrToInt(Copy(input, currentPos, 6));
      Result.Add(ParseString(Copy(input, currentPos + 7, size)));
      Inc(currentPos, 8 + size);
    end;
end;

end.
