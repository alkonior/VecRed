unit TextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, UScale;

type

  { TTextRedactor }
  PFont=^TFont;
  PString=^String;
  TTextRedactor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FontDialog1: TFontDialog;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private

  public
    TextFigure:PString;
    FontFigure:PFont;
  end;
 procedure ShowTextMenu(figure_text:PString; figure_font:PFont);
var
  TextRedactor: TTextRedactor;

implementation

procedure ShowTextMenu(figure_text: PString; figure_font: PFont);
begin
  with  TextRedactor do
   begin
     Visible:=true;
     TextFigure:=figure_text;
     FontFigure:=figure_font;
     Memo1.lines.Text:=TextFigure^;
     Memo1.Font:=TFont.Create;
     FontDialog1.Font:=TFont.Create;
     Memo1.Font:=FontFigure^;
     FontDialog1.Font:=FontFigure^;
   end;
end;


{$R *.lfm}

{ TTextRedactor }

procedure TTextRedactor.Memo1Change(Sender: TObject);
begin
   TextFigure^:=Memo1.Lines.Text;
   InvalidateHandler;
end;

procedure TTextRedactor.Button2Click(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    Memo1.Font:=FontDialog1.Font;
    FontFigure^.Assign(Memo1.Font);
    InvalidateHandler;
  end;
end;


procedure TTextRedactor.Button1Click(Sender: TObject);
begin
  TextRedactor.Visible:=false;
end;



end.
