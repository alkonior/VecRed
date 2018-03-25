program VecRed;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SMain, SFigures, STools, UScale, SParams, SHistroy, TextForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TVecRedF, VecRedF);
  Application.CreateForm(TTextRedactor,TextRedactor);
  Application.Run;
end.

