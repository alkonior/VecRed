unit TextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UScale;

type

  { TTextRedactor }

  TTextRedactor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FontDialog1: TFontDialog;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private

  public

  end;

var
  TextRedactor: TTextRedactor;
  TextFigure:^String;
  FontFigure:^TFont;
implementation

{$R *.lfm}

{ TTextRedactor }

procedure TTextRedactor.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Text:=TextFigure^;
  Memo1.Font:=FontFigure^;
  FontDialog1.Font:=FontFigure^;
end;

procedure TTextRedactor.Memo1Change(Sender: TObject);
begin
   TextFigure^:=Memo1.Lines.Text;
   FontFigure^:=Memo1.Font;
   InvalidateHandler;
end;

procedure TTextRedactor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FontDialog1.Font:=Font.Create;
end;

procedure TTextRedactor.Button2Click(Sender: TObject);
begin
  if FontDialog1.Execute then Memo1.Font:=FontDialog1.Font;
  InvalidateHandler;
end;

procedure TTextRedactor.Button1Click(Sender: TObject);
begin
  TextRedactor.Close;
end;



end.
