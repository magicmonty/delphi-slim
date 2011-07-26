unit SlimMethod;

interface

uses Rtti, Classes;

type TSlimMethod = class
  strict private
    _rttiContextInitialized : Boolean;
    _rttiContext : TRttiContext;
    function RttiContext: TRttiContext;
    function MethodToCall: TRttiMethod;
    function BuildArgValues(arguments : TStrings) : TArray<TValue>;
  strict protected
    function InstanceType: TRttiType;
  public
    FunctionName : string;
    Instance : TObject;
    constructor Create; overload;
    constructor Create(instance : TObject; functionName : string); overload;
    function Call(arguments : TStrings) : string; overload; virtual;
    function Call(const arguments: array of string): string; overload; virtual;
end;

type TNullSlimMethod = class(TSlimMethod)
  strict private
    function TypeName : string;
  public
    function Call(arguments : TStrings) : string; overload; override;
end;

implementation

uses SysUtils, Logger, TypInfo;

{ TSlimMethod }

constructor TSlimMethod.Create;
begin

end;

constructor TSlimMethod.Create(instance: TObject; functionName: string);
begin
  Self.Instance := instance;
  Self.FunctionName := functionName;
end;



function TSlimMethod.Call(const arguments: array of string): string;
var argList : TStringList;
  i : integer;
begin
  argList := TStringList.Create;
  for i := Low(arguments) to High(arguments) do
    argList.Add(arguments[i]);

  Result := Call(argList);
end;

function TSlimMethod.Call(arguments: TStrings): string;
var returnedValue : TValue;
begin
  try
    returnedValue := MethodToCall.Invoke(Instance, BuildArgValues(arguments));
    if returnedValue.IsEmpty then
      Exit('/__VOID__/')
    else
      Exit(returnedValue.ToString);
  except
    on e : Exception do
      Exit('__EXCEPTION__:message:<<' + e.Message + '>>');
  end;
end;

function TSlimMethod.BuildArgValues(arguments: TStrings): TArray<TValue>;
var
  parameters : TArray<TRttiParameter>;
  paramType : TRttiType;
  i : Integer;
begin
    SetLength(Result, arguments.Count);
    parameters := MethodToCall.GetParameters;
    for i := 0 to arguments.Count - 1 do
    begin
      paramType := parameters[i].ParamType;
      if paramType.TypeKind = TTypeKind.tkInteger then
        Result[i] := TValue.From(StrToInt(Arguments[i]))
      else
        Result[i] := TValue.From(arguments[i]);
    end;
end;

function TSlimMethod.MethodToCall: TRttiMethod;
begin
  Result := InstanceType.GetMethod(FunctionName);
end;

function TSlimMethod.InstanceType: TRttiType;
begin
  Result := RttiContext.GetType(Instance.ClassType);
end;

function TSlimMethod.RttiContext: TRttiContext;
begin
  if not _rttiContextInitialized then
    _rttiContext := TRttiContext.Create;
  Result := _rttiContext;
end;

{ TNullSlimMethod }

function TNullSlimMethod.Call(arguments: TStrings): string;
begin
  Result := '__EXCEPTION__:message:<<NO_METHOD_IN_CLASS ' + FunctionName + ' ' + TypeName +'>>';
end;

function TNullSlimMethod.TypeName : string;
begin
  Result := InstanceType.Name;
end;

end.
