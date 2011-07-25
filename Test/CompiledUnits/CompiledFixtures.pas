unit CompiledFixtures;

interface

uses Classes, Dialogs;

type TCompiledFixture = class(TPersistent)
  published
    procedure Hello;
  public
    toto : Integer;
end;

implementation

{ TCompiledFixture }

procedure TCompiledFixture.Hello;
begin

end;

initialization
//  showMessage('registered');

  RegisterClass(TCompiledFixture);
end.
