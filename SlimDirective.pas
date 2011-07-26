unit SlimDirective;

interface

type TSlimDirective = class
  private
    _directives : array of TSlimDirective;
  protected
    function GetValue : string; virtual;
    function GetLength : Integer; virtual;
  public
    constructor ListWith(directive : string);
    function GetItem(index : Integer) : TSlimDirective;
    function GetItemValue(index : Integer) : string;
    function Add(directive : TSlimDirective): TSlimDirective; overload;
    function Add(directive: string): TSlimDirective; overload;
    property Length : Integer read GetLength;
    property Value : string read GetValue;
end;

type TSlimStringDirective = class(TSlimDirective)
  strict private
    _value : string;
  protected
    function GetValue : string; override;
    function GetLength: Integer; override;
  public
    constructor Create(value : string);
end;


implementation

{ TSlimDirective }

function TSlimDirective.Add(directive: TSlimDirective): TSlimDirective;
begin
  SetLength(_directives, Length + 1);
  _directives[Length - 1] := directive;
  Exit(Self);
end;

function TSlimDirective.Add(directive: string): TSlimDirective;
begin
  Add(TSlimStringDirective.Create(directive));
  Exit(Self);
end;

function TSlimDirective.GetItem(index: Integer): TSlimDirective;
begin
  Result := _directives[index];
end;

function TSlimDirective.GetItemValue(index: Integer): string;
begin
  Result := GetItem(index).Value;
end;

function TSlimDirective.GetLength: Integer;
begin
  Result := System.Length(_directives);
end;

function TSlimDirective.GetValue: string;
begin
  Result := '';
end;

constructor TSlimDirective.ListWith(directive : string);
begin
  Add(directive);
end;

{ TSlimStringDirective }

constructor TSlimStringDirective.Create(value: string);
begin
  _value := value;
end;

function TSlimStringDirective.GetLength: Integer;
begin
  Result := 1;
end;

function TSlimStringDirective.GetValue: string;
begin
  Result := _value;
end;

end.
