unit SlimDirectiveSerializer;

interface

uses SlimDirective;

type TSlimDirectiveSerializer = class
  public
    function Serialize(directive : TSlimDirective) : string;
  private
    function LengthString(len : Integer) : string; overload;
    function LengthString(str : string) : string; overload;
end;

implementation

uses SysUtils, StrUtils, Logger;

{ TSlimDirectiveSerializer }

function TSlimDirectiveSerializer.Serialize(directive: TSlimDirective): string;
var i : Integer;
begin
  if directive is TSlimStringDirective then
  begin
    Result := LengthString(directive.Value) + ':' + directive.Value;
  end
  else
  begin
    Result := '[' + LengthString(directive.Length) + ':';
    for i := 0 to directive.Length - 1 do
    begin
      Result := Result + Serialize(directive.GetItem(i)) + ':';
    end;
    Result := Result + ']';
    Result := LengthString(Result) + ':' + Result;
  end;
end;

function TSlimDirectiveSerializer.LengthString(len: Integer): string;
begin
  Result := IntToStr(len);
  Result := DupeString('0', 6 - Length(Result)) + Result;
end;

function TSlimDirectiveSerializer.LengthString(str: string): string;
begin
  Result := LengthString(Length(str));
end;

end.
