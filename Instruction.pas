unit Instruction;

interface

uses SlimDirective, SlimContext, Rtti, Classes, SlimMethod;

type TInstruction = class
  strict private
    _rttiContextInitialized : Boolean;
    _rttiContext : TRttiContext;
    _instructions : array of TInstruction;
  protected
    function RttiContext : TRttiContext;
  public
    Id : string;
    constructor Create; overload;
    constructor Create(id : string); overload;
    function GetLength : Integer; virtual;
    function GetItem(index : Integer) : TInstruction;
    property Length : Integer read GetLength;
    function Execute(context : TSlimContext) : TSlimDirective; virtual;
    procedure Add(item : TInstruction);
end;

type TListInstruction = class(TInstruction)
  public
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

type TMakeInstruction = class(TInstruction)
  strict private
    function FindType(context : TSlimContext) : TRttiInstanceType;
    function GetQualifiedNamesToTry(context : TSlimContext) : TStrings;
    function CreateInstance(rttiType : TRttiInstanceType) : TObject;
  public
    InstanceName : string;
    ClassToMake : string;
    constructor Create; overload;
    constructor Create(id, classToMake, instanceName : string); overload;
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

type TImportInstruction = class(TInstruction)
  public
    Path : string;
    constructor Create; overload;
    constructor Create(id, path : string); overload;
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

type TCallInstruction = class(TInstruction)
  strict private
    _arguments : TStrings;
    _currentContext : TSlimContext;
    function MethodToCall : TRttiMethod;
    function InstanceType : TRttiType;
    function Instance : TObject;
    function SlimMethod : TSlimMethod;
  public
    InstanceName : string;
    FunctionName : string;
    property Arguments : TStrings read _arguments;
    constructor Create; overload;
    constructor Create(id, instanceName, functionToCall : string); overload;
    procedure AddArgument(argument : string);
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

implementation

uses Logger, SysUtils;

{ TInstruction }

procedure TInstruction.Add(item : TInstruction);
begin
  SetLength(_instructions, GetLength() + 1);
  _instructions[GetLength() - 1] := item;
end;

constructor TInstruction.Create;
begin

end;

constructor TInstruction.Create(id: string);
begin
  Self.Id := id;
end;

function TInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
//  Log('Execute ' + Self.ClassName + ' ' + Id);
  Result := TSlimDirective.Create;
  Result.Add(Id);
end;

function TInstruction.GetItem(index: Integer): TInstruction;
begin
  Result := _instructions[index];
end;

function TInstruction.GetLength: Integer;
begin
  Result := System.Length(_instructions);
end;

function TInstruction.RttiContext: TRttiContext;
begin
  if not _rttiContextInitialized then
    _rttiContext := TRttiContext.Create;
  Result := _rttiContext;
end;

{ TMakeInstruction }

constructor TMakeInstruction.Create;
begin

end;

constructor TMakeInstruction.Create(id, classToMake, instanceName: string);
begin
  inherited Create(id);
  Self.ClassToMake := classToMake;
  Self.InstanceName := instanceName;
end;

function TMakeInstruction.Execute(context : TSlimContext): TSlimDirective;
var foundType : TRttiInstanceType;
begin
  Result := inherited;
  foundType := FindType(context);
  if foundType = nil then
    Result.Add('__EXCEPTION__:message:<<NO_CLASS ' + ClassToMake + '>>')
  else
  begin
    Result.Add('OK');
    context.RegisterInstance(InstanceName, CreateInstance(foundType));
  end;
end;

function TMakeInstruction.CreateInstance(rttiType : TRttiInstanceType): TObject;
var instance : TValue;
begin
  instance := rttiType.GetMethod('Create').Invoke(rttiType.MetaclassType, []);
  Result := instance.AsObject;
end;

function TMakeInstruction.FindType(context : TSlimContext): TRttiInstanceType;
var qualifiedName : string;
begin
  Result := nil;
  for qualifiedName in GetQualifiedNamesToTry(context) do
  begin
    Result := RttiContext.FindType(qualifiedName) as TRttiInstanceType;
    if Result <> nil then
      break;
  end;
end;

function TMakeInstruction.GetQualifiedNamesToTry(context: TSlimContext): TStrings;
var importPath : string;
begin
  Result := TStringList.Create;
  Result.Add(ClassToMake);
  for importPath in context.ImportPaths do
  begin
    Result.Add(importPath + '.' + ClassToMake);
  end;
end;

{ TImportInstruction }

constructor TImportInstruction.Create(id, path : string);
begin
  inherited Create(id);
  Self.Path := path;
end;

constructor TImportInstruction.Create;
begin
end;

function TImportInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
  Result := inherited;
  Result.Add('OK');
  context.AddImportPath(Path);
end;

{ TCallInstruction }

constructor TCallInstruction.Create;
begin
  _arguments := TStringList.Create;
end;

constructor TCallInstruction.Create(id, instanceName, functionToCall: string);
begin
  inherited Create(id);
  _arguments := TStringList.Create;
  Self.FunctionName := functionToCall;
  Self.InstanceName := instanceName;
end;

procedure TCallInstruction.AddArgument(argument: string);
begin
  Arguments.Add(argument);
end;

function TCallInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
  Result := inherited;
  _currentContext := context;
  Result.Add(SlimMethod.Call(Arguments));
end;

function TCallInstruction.SlimMethod: TSlimMethod;
begin
  if MethodToCall = nil then
    Result := TNullSlimMethod.Create
  else
    Result := TSlimMethod.Create;

  Result.FunctionName := FunctionName;
  Result.Instance := Instance;
end;

function TCallInstruction.MethodToCall: TRttiMethod;
begin
  Result := InstanceType.GetMethod(FunctionName);
end;

function TCallInstruction.InstanceType: TRttiType;
begin
  Result := RttiContext.GetType(Instance.ClassType);
end;

function TCallInstruction.Instance: TObject;
begin
  Result := _currentContext.GetRegisteredInstance(InstanceName);
end;

{ TListInstruction }

function TListInstruction.Execute(context : TSlimContext): TSlimDirective;
var i : Integer;
begin
  inherited;
  Result := TSlimDirective.Create;
  for i := 0 to GetLength - 1 do
  begin
    Result.Add(GetItem(i).Execute(context));
  end;
end;

end.
