unit SlimDirective;

interface

type TSlimDirective = class
  public
    Value : string;
    constructor ListWith(directive : string);
    function GetItem(index : Integer) : TSlimDirective;
    function Add(directive : TSlimDirective): TSlimDirective; overload;
    function Add(directive: string): TSlimDirective; overload;
    function GetLength : Integer; virtual;
    property Length : Integer read GetLength;
  private
    directives : array of TSlimDirective
end;

type TSlimStringDirective = class(TSlimDirective)
  public
    constructor Create(value : string);
    function GetLength: Integer; override;
end;


implementation

{ TSlimDirective }

function TSlimDirective.Add(directive: TSlimDirective): TSlimDirective;
begin
  SetLength(directives, Length + 1);
  directives[Length - 1] := directive;
  Exit(Self);
end;

function TSlimDirective.Add(directive: string): TSlimDirective;
begin
  Add(TSlimStringDirective.Create(directive));
  Exit(Self);
end;

function TSlimDirective.GetItem(index: Integer): TSlimDirective;
begin
  Result := directives[index];
end;

function TSlimDirective.GetLength: Integer;
begin
  Result := System.Length(directives);
end;

constructor TSlimDirective.ListWith(directive : string);
begin
  Add(directive);
end;

{ TSlimStringDirective }

constructor TSlimStringDirective.Create(value: string);
begin
  Self.Value := value;
end;

function TSlimStringDirective.GetLength: Integer;
begin
  Result := 1;
end;

end.
